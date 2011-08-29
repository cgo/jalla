{-# LANGUAGE PackageImports #-}
module Main where

-- import qualified Data.ByteString as BS
import Text.Parsec.Language
import Text.Parsec.Char
import Text.Parsec.Error
import Text.Parsec
import "mtl" Control.Monad.Identity
import qualified Control.Applicative as App
import Data.List
import Data.Maybe
import System.Environment
import qualified "mtl" Control.Monad.State as State

type Ret = Arg

data Arg = A { 
  argName :: String,
  cArg :: String,
  hsArg :: String,               
  inMarsh :: String,
  outMarsh :: String, 
  retMarsh :: String} deriving (Eq, Show)

allArgs = [
  A "uplo" "char" "CChar" "fromEnum" "" "fromEnum",
  A "" "char" "CChar" "fromEnum" "" "fromEnum",
  A "" "int" "Int" "fromIntegral" "" "fromIntegral",
  A "" "lapack_int" "Int" "fromIntegral" "" "fromIntegral",
  A "" "lapack_logical" "Int" "fromIntegral" "" "fromIntegral",
  A "" "float" "CFloat" "id" "" "realToFrac",
  A "" "double" "CDouble" "id" "" "realToFrac",
  A "" "lapack_int*" "Ptr CInt" "id" "" "id",
  A "" "lapack_logical*" "Ptr CInt" "id" "" "id",
  A "" "int*" "Ptr CInt" "id" "" "id",
  A "" "float*" "Ptr CFloat" "id" "" "id",
  A "" "double*" "Ptr CDouble" "id" "" "id",
  A "" "lapack_complex_float*" "Ptr (Complex CFloat)" "castComplexToPtr" "" "castComplexToPtr",
  A "" "lapack_complex_double*" "Ptr (Complex CDouble)" "castZomplexToPtr" "" "castZomplexToPtr",
  A "" "void*" "Ptr ()" "id" "" "id",
  A "" "void" "()" "" "id" "id",
  A "" "CBLAS_INDEX" "CblasIndex" "fromIntegral" "" "fromIntegral",
  A "" "CBLAS_ORDER" "CblasOrder" "cFromEnum" "" "cToEnum",
  A "" "CBLAS_TRANSPOSE" "CblasTranspose" "cFromEnum" "" "cToEnum",
  A "" "CBLAS_UPLO" "CblasUplo" "cFromEnum" "" "cToEnum",
  A "" "CBLAS_DIAG" "CblasDiag" "cFromEnum" "" "cToEnum",
  A "" "CBLAS_SIDE" "CblasSide" "cFromEnum" "" "cToEnum"]

                         
data PState = PState {pstateFunctions :: [CFunction] }

type P = ParsecT String PState Identity

data CFunction = CFunction { cfRet :: Ret,            -- Return value
                             cfCName :: String,       -- Original name of the c function
                             cfHsName :: String,      -- Name of the imported Haskell function
                             cfCommonName :: String, -- Name in class, without prefix s,d,c,z.
                             cfArgs :: [Arg],         -- Arguments 
                             cfType :: TypeCode
                           } deriving (Eq, Show)


data TypeCode = BSingle | BDouble | BComplex | BZomplex | Extra deriving (Show, Eq)

{- | Helper function; replaces occurrences of elements in a list for which a predicate function
     is true. The other elements are simply copied. -}
replaceBy :: (a -> Bool) -> (a -> a) -> [a] -> [a]
replaceBy t f (a:as) = if t a then f a : replaceBy t f as else a : replaceBy t f as
replaceBy _ _ [] = []


toCFunction :: String -> String -> [(String,String)] -> Maybe CFunction
toCFunction ret fname args = 
  let hsname' | "_work" `isSuffixOf` fname = []  -- drop all _work functions
              | "LAPACKE_" `isPrefixOf` fname = drop 8 fname 
              | "LAPACK_" `isPrefixOf` fname = [] -- drop all LAPACK functions; only export LAPACKE.
              | otherwise = fname
      -- Handle some exceptions...
      headElem | hsname' == [] = '\0'
               | otherwise = head hsname'
      (hsname, typeCode) | hsname' == "scnrm2" = ("nrm2", BComplex)
                         | hsname' == "dznrm2" = ("nrm2", BZomplex)                 
                         | hsname' == "scasum" = ("asum", BComplex)
                         | hsname' == "dzasum" = ("asum", BZomplex)
                         | hsname' == "dsdot" =  ("dot", BSingle)
                         | hsname' == "sdsdot" = ("sdsdot", Extra)
                         | hsname' == "sdot"   = ("sdot", Extra)
                         | hsname' == "cscal" = ("scal", BComplex)
                         | hsname' == "zscal" = ("scal", BZomplex)                                                             
                         | hsname' == "csscal" = ("scal'", BComplex)
                         | hsname' == "zdscal" = ("scal'", BZomplex)                                                
                         | headElem == 's' = (drop 1 hsname', BSingle)
                         | headElem == 'd' = (drop 1 hsname', BDouble)                                             
                         | headElem == 'c' = (drop 1 hsname', BComplex)                                             
                         | headElem == 'z' = (drop 1 hsname', BZomplex)                                             
                         | "is" `isPrefixOf` hsname' = ('i' : drop 2 hsname', BSingle) -- isamax
                         | "id" `isPrefixOf` hsname' = ('i' : drop 2 hsname', BDouble) -- idamax
                         | "ic" `isPrefixOf` hsname' = ('i' : drop 2 hsname', BComplex) -- isamax
                         | "iz" `isPrefixOf` hsname' = ('i' : drop 2 hsname', BZomplex) -- idamax
                         | otherwise = error $ "Failed to convert c function " ++ fname -- (hsname',Extra)
      getHsArg (c, name) = let r = find (\a -> (c,"") == (cArg a, argName a) || 
                                              (c,name) == (cArg a, argName a)) allArgs 
                   in fromMaybe (error $ "Unknown argument " ++ c) r

      resultArgs :: [Arg]
      resultArgs | typeCode == BComplex = replaceBy (\a -> hsArg a == "Ptr ()") (\a -> a {hsArg = "Ptr (Complex CFloat)", inMarsh = "castPtr"}) (map getHsArg args)
                 | typeCode == BZomplex = replaceBy (\a -> hsArg a == "Ptr ()") (\a -> a {hsArg = "Ptr (Complex CDouble)", inMarsh = "castPtr"}) (map getHsArg args)
                 | otherwise = map getHsArg args
  in 
   case hsname' of
        [] -> Nothing
        _ -> Just CFunction { cfRet = getHsArg (ret,""),
                             cfCName = fname,
                             cfHsName = hsname',
                             cfCommonName = hsname,
                             cfArgs = resultArgs, 
                             cfType = typeCode }
                            

{--------------- Header file parsing -------------}

{- | Extracts all C function declarations from a C header file. -}
cHeaderFile :: P PState
cHeaderFile = do       
  manyTill (try (skipMany cComment >> cFunDec) <|> skipLine) eof
  getState
  

skipLine = anyChar `manyTill` try eol >> return ()

cSpace = try (space >> return ()) 
         <|> eol
-- cSpaces = skipMany $ choice [try (cComment >> return ()), try cSpace]
cSpaces = skipMany cSpace

cComment = between (string "/*") (string "*/") $ many anyChar

cPP = do
  cSpaces
  char '#'
  anyChar `manyTill` try eol

cIgnorables = skipMany $ choice [try (cComment >> return ()), try (cPP >> return ()), cSpace]

--eol = do
--  choice [string "\n\r", string "\r\n", string "\n"]
--  return ()

eol = 
  try (string "\r\n" >> r)
   <|> try (string "\n\r" >> r)
   <|> (string "\n" >> r)
   <|> (string "\r" >> r)
    where
      r = return ()


cLineComment = do
  spaces
  string "//"
  anyChar `manyTill` eol


{--------------- Creating BLAS type classes -------------------}

data Classes = Classes { clFloatClass :: [(String, CFunction)],
                         clDoubleClass :: [(String, CFunction)],
                         clComplexClass :: [(String, CFunction)],
                         clZomplexClass :: [(String, CFunction)] } deriving Show

-- FIXME: reader/state monad, filling the classes. They can also fill instantiations.

type CreateClasses = State.State Classes 

createClassFun :: CFunction -> CreateClasses ()
createClassFun cf@(CFunction ret cname hsname' hsname args typeCode) = clst
  where 
    clst = cls typeCode
    line = "  " ++ hsname ++ " :: " ++ (intercalate " -> " . map hsArg) args ++ " -> IO (" ++ hsArg ret ++ ")"
    cls BSingle = State.get >>= (\s -> let ss = clFloatClass s in return $ s { clFloatClass = (line,cf) : ss }) >>= State.put 
    cls BDouble = State.get >>= (\s -> let ss = clDoubleClass s in return $ s { clDoubleClass = (line,cf) : ss }) >>= State.put
    cls BComplex = State.get >>= (\s -> let ss = clComplexClass s in return $ s { clComplexClass = (line,cf) : ss })  >>= State.put
    cls BZomplex = State.get >>= (\s -> let ss = clZomplexClass s in return $ s { clZomplexClass = (line,cf) : ss }) >>= State.put
    cls Extra = return ()


createClasses :: [CFunction] -> Classes
createClasses fs = State.execState
                   (mapM createClassFun fs) (Classes [] [] [] [])


filterCommonFunctions :: Classes -> (Classes, Classes)
filterCommonFunctions classes@(Classes fc dc cc zc) = (commonClasses, nonCommonClasses)
  where 
    (commonClasses, nonCommonClasses) = partition' (`inAll` classes) classes
    partition' f (Classes fc dc cc zc) = (Classes f1 d1 c1 z1, Classes f2 d2 c2 z2)
      where
        [(f1,f2), (d1,d2), (c1,c2), (z1,z2)] = map (partition f) [fc, dc, cc, zc]
    inOne f = any (\a -> cfCommonName (snd a) == (cfCommonName . snd) f)
    inAll f (Classes fc dc cc zc) = all (inOne f) [fc, dc, cc, zc]


data Similarity = Equal | Similar [ArgDiff] | Different deriving (Eq, Show)
data ArgDiff = ArgDiff (String,String) | ArgEqual deriving (Eq, Show)
argDiff :: Arg -> Arg -> ArgDiff
argDiff a1 a2 = result
  where
    cc@(c1,c2) = (cArg a1, cArg a2)
    result | c1 == c2 = ArgEqual
           | otherwise = ArgDiff cc


{-| Finds out whether two functions have the same signature up to some differing arguments A.
The difference between the arguments a \in A must always be the same; i.e. a Float in one function
always is a Double in the other; there can not be a Float in function 1 and a Bool in function 2, when
there were other Floats in function 1 which mapped to, say, Double.
If the functions are similar in this sense, Similar [argdiffs] is returned, where [argdiffs] is the list
of differences in arguments (for all arguments; the first entry is the return value difference). -}
compareFunctions :: CFunction -> CFunction -> Similarity
compareFunctions (CFunction r1 _ _ name1 args1 tp1) (CFunction r2 _ _ name2 args2 tp2) = 
  if name1 == name2 then diffsAreEqual else Different
  where
    argdiffs = zipWith argDiff (r1:args1) (r2:args2)
    argdiffs' = filter (/= ArgEqual) argdiffs
    diffsAreEqual :: Similarity
    diffsAreEqual | argdiffs' == [] = Equal
                  | otherwise = if diffsAreDistinct then Similar argdiffs else Different
                    where (a:as) = argdiffs'
    -- Ueberall gleich muss heissen: Wenn eine Differenz vorkommt, dann darf keiner der beiden Partner in
    -- einer anderen Differenz vorkommen. Dann muesste es hinhauen.
    diffsAreDistinct :: Bool
    diffsAreDistinct = not . null $ nubBy (\(ArgDiff (a,b)) (ArgDiff (a',b')) -> 
                                          (a == a') && (b /= b') 
                                          || 
                                          (a /= a') && (b == b')) $ nub argdiffs'
    

{-| Filters similar functions: functions of which the cfCommonName is equal and which have similar parameters
in the sense of compareFunctions. -}
filterSimilarFunctions :: [CFunction] -> [[(CFunction,Similarity)]]
filterSimilarFunctions [] = []
filterSimilarFunctions fns@(f:fs) = a : filterSimilarFunctions (map fst b)
  where
    sims :: CFunction -> [CFunction] -> [(CFunction,Similarity)]
    sims f fns = zip fns (map (compareFunctions f) fns)
    filterSimilar (_,s) = case s of
      Similar _ -> True
      Equal     -> True
      _         -> False
    (a',b) = partition filterSimilar $ sims f fs
    a = (f,Equal):a'


data CClass = CClass { className :: String,
                       classTypeVars :: [String],
                       classFunctions :: [(CFunction,Similarity)] }


filterClasses :: [[(CFunction,Similarity)]] -> [CClass]
filterClasses = undefined

{-
filterSDCZ :: [[(CFunction,Similarity)]] -> [[(CFunction,Similarity)]]
filterSDCZ a = filterClass a (\fslist -> all (==True) $ map (`elem` [BSingle,BDouble,BComplex,BZomplex]) (map (cfType . fst) fslist))


filterClass :: [[(CFunction,Similarity)]] -> ([(CFunction,Similarity)] -> Bool) -> [[(CFunction,Similarity)]]
filterClass groups f = classGroups $ zip members allTypeReplacements
  where
    members = filter f groups -- Alle Funktionen, die in die Klasse gehoeren
    --replaceables :: [(String,String)]
    --representatives :: [CFunction] -- Funktionsprototypen, die in die Klassendefinition geschrieben werden
    --representatives = map getRepresentative members
    allTypeReplacements = map getTypeReplacements members
    classGroups ((group,replacement):rest) = g : classGroups (rest \\ g)
      where g = (group,replacement) : filter (\a -> all (==True) $ map (`elem` replacement) (snd a)) rest
-}

{-| Get an alist with the replacements from strings in the ArgDiff to type variables for a class definition. -}
getTypeReplacements :: [(CFunction,Similarity)] -> [(String, String)]
getTypeReplacements group = concatMap (uncurry replacements) $ zip allDistinctArgDiffs typeVars
  where
    replacements (ArgDiff (a,b)) typevar = [(a,[typevar]),(b,[typevar])]
    replacements _ _ = []
    typeVars = ['a'..]
    typeVarCount = length allDistinctArgDiffs
    allDistinctArgDiffs = nub $ concatMap filterSimilarArgDiffs $ map snd group  
    filterSimilarArgDiffs :: Similarity -> [ArgDiff]
    filterSimilarArgDiffs s = case s of
      Similar a -> a
      _         -> []



allClassesText :: Classes -> String
allClassesText classes = commonText ++ "\n\n" ++ nonCommonComplex ++ "\n\n" ++ nonCommonReal ++ "\n\n" ++ instances
  where
    (common, nonCommon) = filterCommonFunctions classes
    commonText' = classText "class (Field e he) => LapackeOps e he" (map snd (clFloatClass common))
    replaceText :: String -> String -> String -> String
    replaceText s r text = let p' = runP (
                                 do 
                                   -- This is slow (++), but I can't figure out "shows" right now.
                                   many (try (string s >> modifyState (++ r)) <|> 
                                         (anyChar >>= \a -> modifyState (\s -> s ++ [a])))
                                   getState ) "" "" text
                               p = case p' of
                                 Left pe -> concatMap messageString $ errorMessages pe
                                 Right pp -> pp
                           in p
    -- FIXME: Hier nicht float ersetzen; alle Funktionen filtern, die den gleichen Namen haben, und
    -- da die Funktionen filtern, die an der gleichen Stelle die entsprechenden Parameter haben (die Klassenabhaengigen).
    -- Wenn alle klassenabhaengigen an den gleichen Stellen der Argumentliste sind, passts. Wenn nicht, ist das keine
    -- gemeinsame Funktion, bzw. eine von einer Untermenge der Klassen.
    commonText = replaceText "Float" "he" $ replaceText "CFloat" "e" commonText'
    nonCommonComplex = replaceText "Float" "he" $ replaceText "CFloat" "e" nonCommonComplex'
    nonCommonComplex' = classText "class (LapackeOps (Complex e) (Complex he)) => LapackeOpsComplex e he"
                        (map snd (clComplexClass nonCommon))
    nonCommonReal = replaceText "Float" "he" $ replaceText "CFloat" "e" nonCommonReal'
    nonCommonReal' = classText "class (LapackeOps e he) => LapackeOpsReal e he"
                     (map snd (clFloatClass nonCommon))
    instances = allClassesInstantiations (common, nonCommon)


allClassesInstantiations :: (Classes, Classes) -> String
allClassesInstantiations (common, nonCommon) = intercalate "\n\n" text
  where
    protoFns = map snd $ clFloatClass common
    protoFnsC = map snd $ clComplexClass nonCommon
    protoFnsR = map snd $ clFloatClass nonCommon
    text = [instanceText "instance LapackeOps CFloat Float" (map snd (clFloatClass common)) protoFns,
            instanceText "instance LapackeOps CDouble Double" (map snd (clDoubleClass common)) protoFns,
            instanceText "instance LapackeOps (Complex CFloat) (Complex Float)" (map snd (clComplexClass common)) protoFns,
            instanceText "instance LapackeOps (Complex CDouble) (Complex Double)" (map snd (clZomplexClass common)) protoFns,
            instanceText "instance LapackeOpsComplex CFloat Float" (map snd (clComplexClass nonCommon)) protoFnsC,
            instanceText "instance LapackeOpsComplex CDouble Double" (map snd (clZomplexClass nonCommon)) protoFnsC,
            instanceText "instance LapackeOpsReal CFloat Float" (map snd (clFloatClass nonCommon)) protoFnsR,
            instanceText "instance LapackeOpsReal CDouble Double" (map snd (clDoubleClass nonCommon)) protoFnsR]


classText :: String -> [CFunction] -> String
classText clsStart clsFns =
  clsStart ++ " where\n"
  ++ intercalate "\n" (map fnDec clsFns)
    where
      fnDec :: CFunction -> String
      fnDec f = "  " ++ cfCommonName f ++ " :: " ++ intercalate " -> " args ++ " -> " ++ "IO " ++ ret
        where 
          args = map hsArg (cfArgs f)
          ret = hsArg $ cfRet f



-- TODO: Suche die Argumente, bei denen die Implementierung einen Ptr e erwartet,
--       die Klasse aber ein "he" vorgibt. Da dann eine Konvertierung und ein "with"
--       benutzen. Vergleich cfFloatClass common/nonCommon Argumente mit den gegebenen Argumenten.
instanceText :: String -> [CFunction] -> [CFunction] -> String
instanceText clsStart clsFns protoFns =
  clsStart ++ " where\n" ++ intercalate "\n" (map fnDec $ zip clsFns protoFns)
    where
      fnDec :: (CFunction, CFunction) -> String
      fnDec (f, protoF) = lhs ++ rhs
        where
          lhs | anyArgNeedsMarsh || retNeedsMarsh = "  " ++ cfCommonName f ++ " " ++ unwords argNamesf ++ " = "
              | otherwise = "  " ++ cfCommonName f ++ " = " 
          rhs | anyArgNeedsMarsh || retNeedsMarsh = concat marshArgs ++ cfHsName f ++ " " ++ unwords newArgs ++ " " ++ retMarsh
              | otherwise = cfHsName f
                              
          (marshArgs, newArgs) = unzip $ map marshArg (zip3 (cfArgs f) (cfArgs protoF) argNamesf)
                              
          marshArg :: (Arg, Arg, String) -> (String, String)
          marshArg (aF, aProto, aName) | argNeedsMarsh (aF, aProto) = 
            let aName' = aName ++ "'" 
            in ("with (convComplex " ++ aName ++ ") $ \\" ++ aName' ++ " -> ", aName')
                              
          marshArg (_, _, aName) = ("", aName)
                              
          -- cfHsName f aufrufen mit den parametern in argNames (cfArgs f) 0; 
          -- die, fuer die argNeedsMarsh wahr ist, muessen mit "with" an 
          -- die Implementierung uebergeben werden.

          argNames :: [a] -> Int -> [String]
          argNames [] _ = [] 
          argNames (_:as) n = ('a' : show n) : argNames as (n+1)
          argNamesf = argNames (cfArgs f) 0
          isPtr a = "Ptr" `isPrefixOf` hsArg a
          argNeedsMarsh (aF,aProto) | isPtr aF && isPtr aProto = False
                                    | not (isPtr aF) && not (isPtr aProto) = False
                                    | isPtr aF && not (isPtr aProto) = True
                                    | otherwise = error $ "instanceText: This case is not handled and should not occur in BLAS! Function: " ++ cfCName f ++ " proto Haskell name: " ++ cfHsName protoF
          --anyArgNeedsMarsh = any (==True) $ map argNeedsMarsh $ zip (cfArgs f) (cfArgs protoF)
          anyArgNeedsMarsh = False
          
          (retNeedsMarsh, retMarsh) | cfHsName f == "scnrm2" = retRealToComplex
                                    | cfHsName f == "dznrm2" = retRealToComplex
                                    | cfHsName f == "scasum" = retRealToComplex
                                    | cfHsName f == "dzasum" = retRealToComplex
                                    | otherwise = (False, "")
          retRealToComplex = (True, " >>= \\a -> return (a :+ 0)")
          
                                      


createC2hsFun :: CFunction -> String
createC2hsFun cf = let fname = cfCName cf
                       hsname = cfHsName cf
                       args = map (\a -> inMarsh a ++ " `" ++ hsArg a ++ "'") $ cfArgs cf
                       ret = "`" ++ hsArg (cfRet cf) ++ "' " ++ retMarsh (cfRet cf)
                   in
                    "{# fun unsafe " ++ fname ++ " as " ++ hsname ++ " {" ++ intercalate ", " args ++ "}" ++ " -> "++ ret ++ " #}"
                    
{-| These function names will be ignored. -}
ignoreList :: [String]
ignoreList = ["lapack_make_complex_float", "lapack_make_complex_double", "LAPACKE_zcgesv", "LAPACKE_dsgesv", "LAPACKE_zcposv", "LAPACKE_dsposv", "LAPACKE_zcgesv_work", "LAPACKE_dsgesv_work", "LAPACKE_zcposv_work", "LAPACKE_dsposv_work"]

cFunDec :: P ()
cFunDec = do
  r <- cRetVal <?> "cRetVal"
  cSpaces
  fn <- cFunName <?> "cFunName"
  cSpaces
  args <- char '(' >> sepBy (cSpaces >> cArgument App.<* cSpaces) (char ',') >>= \r -> cSpaces >> char ')' >> return r
  cSpaces
  char ';'
  when (fn `notElem` ignoreList) $
    modifyState (\(PState fs) -> let mf = toCFunction r fn args 
                                in PState $ maybe fs (: fs) mf)
  -- return $ r ++ fn ++ (concat args)

cRetVal = cTypeIdent

cFunName = cIdentifier

cArgument :: P (String, String)
cArgument = do
  i <- cTypeIdent
  cSpaces
  n <- cVarName
  return (i,n)

cTypeIdent :: P String
cTypeIdent = do
  c <- option "" (string "const")
  cSpaces
  e <- option "" (string "enum")
  cSpaces
  i <- cIdentifier
  cSpaces
  star <- option "" $ string "*"
  return $ i ++ star
  
cIdentifier :: P String
cIdentifier = do
  l1 <- optionMaybe (try letter <|> char '_')
  l2 <- many $ try alphaNum <|> char '_'
  return $ case l1 of
                Nothing -> l2
                Just l -> l : l2
  
cVarName = cIdentifier

cToC2HS :: String -> PState
cToC2HS s = let e = runP cHeaderFile (PState []) "" s
            in 
             case e of
               Left pe -> let s = map messageString (errorMessages pe) in PState []
               Right rs -> rs
                
writeClasses :: Classes -> IO ()
writeClasses cls = do
  writeFile "zomplex.hs" $ intercalate "\n" (get clZomplexClass)
  writeFile "complex.hs" $ intercalate "\n" (get clComplexClass) 
  writeFile "float.hs"   $ intercalate "\n" (get clFloatClass) 
  writeFile "double.hs"  $ intercalate "\n" (get clDoubleClass) 
    where      
      get f = map fst (f cls)
    

writeClasses' :: Classes -> IO ()
writeClasses' classes = writeFile "lapacke_classes.hs" s
  where
    s = allClassesText classes


printGroups :: [[(CFunction,Similarity)]] -> IO ()
printGroups = mapM_ printGroup 

printGroup :: [(CFunction,Similarity)] -> IO ()
printGroup g = putStrLn "Group" >> mapM_ (putStrLn . cfCName . fst) g

main = do
  args <- getArgs
  when (length args < 1) $ error "Usage: parselapacke <lapacke header file>"
  let fname = head args
  f <- readFile fname
  let functions = pstateFunctions $ cToC2HS f
      --classes = createClasses functions
  --writeClasses' classes
  --putStrLn $ show $ filterSimilarFunctions functions
  printGroups $ filterSimilarFunctions functions
  writeFile "lapacke.chs" $ (concat . reverse . intersperse "\n") (map createC2hsFun functions)


