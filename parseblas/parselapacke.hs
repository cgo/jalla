{-
  Somewhat newer wrapper generator for LAPACKE.
  The thing that works is generating wrappers
  for the LAPACKE functions via c2hs.
  I have been playing with automatic generation of the type classes and instantiations
  in this file, but it didn't work yet as I imagined it would. 
  The parts that create a working input file for c2hs are at the beginning of
  this source file, the other ones (which are not needed for wrapping)
  come after a short comment further below.
  
  Wrappers for all functions starting with /LAPACKE_/ should be created.
  Functions with suffix /_work/ are ignored.

  By changing /nameAndClass/, /lapackeIgnoreSuffixes/, /lapackFilter/ in /main/,
  and /lapackArgs/ accordingly, one can probably also wrap (C)BLAS functions
  using this generator.

  --* Note on XBLAS functions
  There are functions in LAPACKE which apparently rely on XBLAS to be present.
  If you do not have that, the variable /lapackeIgnoreSuffixes/ already holds
  a list of suffices of functions to ignore.
  If you have XBLAS and want to use these functions, fell free to change that list.

  Call this program with the lapacke.h header as argument, and you should get a 
  LAPACKE_.chs file that you need to paste into LAPACKE.chs in BLAS/Foreign.
-}

module Main where

import Language.C
import Language.C.Data.Ident
import Language.C.Analysis
import Language.C.System.GCC
import System
import Data.Map (toList, Map, fromListWith, fromList, empty, unions)
import qualified Data.Map as Map (lookup)
import Data.Maybe
import Data.List
import Control.Monad (when, zipWithM)

import Control.Monad.Trans.Writer

main = do
  [fn] <- getArgs
  translUnit <- parseMyFile fn
  let Right (decls, _) = runTrav_ $ analyseAST translUnit
  let funcDecls = filter declFilter $ map snd $ toList (gObjs decls)
      lapackFilter = \a -> ("LAPACKE_" `isPrefixOf` (cfName a)) && not("_work" `isSuffixOf` (cfName a)) && not (ignore $ cfName a)
      funcs = filter lapackFilter $ catMaybes $ map fun funcDecls

      -- These are the C function wrappers for C2HS.
      (cWrapper, cWrapperl) = runM $ mapM createC2hsFun funcs 
      
  -- This file is to be added to the LAPACKE.chs file in BLAS/Foreign.
  writeFile "LAPACKE_.chs" $ unlines $ catMaybes cWrapper
  mapM_ putStrLn cWrapperl



parseMyFile :: FilePath -> IO CTranslUnit
parseMyFile input_file = do
  parse_result <- parseCFile (newGCC "gcc") Nothing ["-DLAPACK_COMPLEX_STRUCTURE","-DHAVE_LAPACK_CONFIG_H"] input_file
  case parse_result of
    Left e -> error (show e)
    Right ast -> return ast

printAst :: CTranslUnit -> IO ()
printAst = print . pretty


declFilter :: IdentDecl -> Bool
declFilter (Declaration _) = True
declFilter _ = False

data CFunction = CFunction { cfName :: String, 
                             cfType :: FunType }

-- | Get function name and type from an IdentDecl.
fun :: IdentDecl -> Maybe CFunction
fun (Declaration (Decl (VarDecl (VarName (Ident str _ _) _) _ typ) _ )) = funType typ >>= \ft -> Just (CFunction str ft)
fun _ = Nothing

-- | Get the FunType from a Type
funType :: Type -> Maybe FunType
funType (FunctionType ft _) = Just ft
funType _ = Nothing

funTypeStr :: FunType -> (String, [String])
funTypeStr (FunType ret params _) = (show (pretty ret), map (show . pretty) params)
funTypeStr _ = ([],[])


type M = Writer [String]

logger :: String -> M ()
logger s = tell [s]

runM :: M a -> (a, [String])
runM = runWriter

-- Ignore functions using XBLAS.
lapackeIgnoreSuffixes = ["xx", "sx"]
ignore :: String -> Bool
ignore s = or $ map (`isSuffixOf` s) lapackeIgnoreSuffixes

{-| Creates a line for a c2hs input file, wrapping the given 'CFunction'. -}
createC2hsFun :: CFunction -> M (Maybe String)
createC2hsFun cf = do
  margs' <- funArgs cf
  let fname  = cfName cf
      -- hsname = fname -- cfHsName cf                       
      margs_fixed = mnac >>= \(_,_,_,cc) -> margs' >>= return . fixArgs cc
      margs  = margs_fixed >>= \args -> return $ map (\a -> inMarsh a ++ " `" ++ hsArg a ++ "'") (init args)
      mret   = margs_fixed >>= return . last >>= \ret -> Just ("`" ++ hsArg ret ++ "' " ++ retMarsh ret)
      mnac   = nameAndClass cf
  return $ do
    args <- margs
    ret <- mret 
    (CName name, HsName hsname, ShortName short_name, classCode) <- mnac
    Just $ "{# fun unsafe " ++ fname ++ " as " ++ hsname ++ " {" ++ intercalate ", " args ++ "}" ++ " -> "++ ret ++ " #}"
  
  
-- | Marks the class of BLAS/LAPACK functions a function belongs to.
data BClassCode = BSingle | BDouble | BComplex | BZomplex | Extra deriving (Show, Eq, Ord)


-- | Return the 'Arg' list of arguments for the given function, or Nothing if that's impossible.
funArgs :: CFunction -> M (Maybe [Arg])
funArgs cf = do  
  let FunType ret params _ = cfType cf
  margs' <- sequence $ map paramArg params
  let margs = sequence margs'
  case margs of
    Just _ -> return ()
    Nothing -> logger ("Function " ++ cfName cf ++ " has margs = Nothing!")
  mretarg <- typeArg "" ret
  return $ margs >>= \args -> mretarg >>= \retarg -> return (args ++ [retarg])


-- | Replace pointers in complex type functions.
fixArgs :: BClassCode -> [Arg] -> [Arg]
fixArgs BComplex args = replaceBy 
                             (\a -> hsArg a == "Ptr ()") 
                             (\a -> a {hsArg = "Ptr (Complex CFloat)", inMarsh = "castPtr"}) args
fixArgs BZomplex args = replaceBy 
                             (\a -> hsArg a == "Ptr ()") 
                             (\a -> a {hsArg = "Ptr (Complex CDouble)", inMarsh = "castPtr"}) args
fixArgs _ args = args

{- | Helper function; replaces occurrences of elements in a list for which a predicate function
     is true. The other elements are simply copied. -}
replaceBy :: (a -> Bool) -> (a -> a) -> [a] -> [a]
replaceBy t f (a:as) = if t a then f a : replaceBy t f as else a : replaceBy t f as
replaceBy _ _ [] = []


data Arg = A { 
  argName :: String,
  cArg :: String,
  hsArg :: String,
  inMarsh :: String,
  outMarsh :: String,
  retMarsh :: String} deriving (Eq, Show)

{-| Defines the mappings from C argument types to Haskell types, as well as marshalling functions
    that will be used with c2hs. -}
lapackArgs = [
  A "uplo" "char" "CChar" "castChar" "" "castChar",
  A "" "char" "CChar" "castChar" "" "castChar",
  A "" "char*" "Char" "withSingleChar*" "withSingleCharPtr*" "withSingleChar*",
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
  A "" "_lapack_complex_float*" "Ptr (Complex CFloat)" "castComplexToPtr" "" "castComplexToPtr",
  A "" "_lapack_complex_double*" "Ptr (Complex CDouble)" "castZomplexToPtr" "" "castZomplexToPtr",
  A "" "void*" "Ptr ()" "id" "" "id",
  A "" "void" "()" "" "id" "id",
  A "" "CBLAS_INDEX" "CblasIndex" "fromIntegral" "" "fromIntegral",
  A "" "CBLAS_ORDER" "CblasOrder" "cFromEnum" "" "cToEnum",
  A "" "CBLAS_TRANSPOSE" "CblasTranspose" "cFromEnum" "" "cToEnum",
  A "" "CBLAS_UPLO" "CblasUplo" "cFromEnum" "" "cToEnum",
  A "" "CBLAS_DIAG" "CblasDiag" "cFromEnum" "" "cToEnum",
  A "" "CBLAS_SIDE" "CblasSide" "cFromEnum" "" "cToEnum"]



-- From here on downwards, there are some functions I experimented with (for creating the classes).
-- Do not bother. All needed for wrapping LAPACKE should be above.
--------------------------------------------------------------------------------------------------------



-- | The C function name
data CName = CName { unCName :: String } deriving (Eq, Show, Ord)

-- | The Haskell function name
data HsName = HsName String deriving (Eq, Show, Ord)

-- | The short Haskell function name, to be used for the type classes.
data ShortName = ShortName { unShortName :: String } deriving (Eq, Show, Ord)
  

-- | Contains all that is needed to wrap a function.
data Wrap = Wrap { wrapCName :: CName,
                   wrapHsName :: HsName,
                   wrapShortName :: ShortName,
                   wrapClass :: BClassCode,
                   wrapArgs :: [Arg],
                   wrapFun :: CFunction } 
            
instance Show Wrap where
  show = show . unCName . wrapCName

wrap :: CFunction -> M (Maybe Wrap)
wrap cf = do
  margs <- funArgs cf
  return $ margs >>= \args -> nameAndClass cf >>= \(cn, hn, sn, c) -> 
    return (Wrap cn hn sn c args cf)



-- | Group by short names.
groupNames :: [Wrap] -> [[Wrap]]
groupNames ws = map (sortBy (\a b -> compare (wrapClass a) (wrapClass b))) $ (map snd . toList . groupNames') ws


groupNames' :: [Wrap] -> Map ShortName [Wrap]
groupNames' ws = fromListWith f $ zip names (map (\a -> [a]) ws)
  where names = map wrapShortName ws
        f [a] as = a : as



-- | Count how many pairs exist with equal first element.
count1 :: (Eq a, Ord a) => [(a,b)] -> [Int]
count1 ab = map length $ groupBy f1 $ sortBy f2 ab
  where f1 = \(a1,_) (a2,_) -> a1 == a2
        f2 = \(a1,b1) (a2,b2) -> compare a1 a2
  --toList $ fromListWith (+) $ zip as $ repeat 1


-- Like nub, but using Data.Set and taking n*log(n).
uniquify :: (Eq a, Ord a) => [(a,b)] -> [(a,b)]
uniquify = nubBy (\a b -> fst a == fst b)


data DiffArg = DiffArg { daTypeName :: String, daPos :: Int } deriving (Show, Eq, Ord)
data PolymorphicArg = NonPolyArg Arg | 
                      PolyArg { paName :: String, paPos :: Int } deriving (Show)
-- data ArgPair = ArgPair { 

data Class = Class { clContext :: String,
                     clName :: String,
                     clTypeParams :: Map [String] String,
                     clWraps :: [[Wrap]] } deriving Show

-- lapack4TypeMap :: Map [String] String
-- lapack4TypeMap = fromList [(["float*","double*","_lapack_complex_float*","_lapack_complex_double*"], "Ptr e")
--                            ,["float*","double*","_lapack_complex_float*","_lapack_complex_double*"], []


printClass :: Class -> String
printClass c = s ++ sf
  where s = "class " ++ context ++ clName c ++ " " ++ params ++ " where\n"
        sf = unwords $ map ((\a -> ' ':' ':a) . printClassFun c) $ clWraps c
        context | null (clContext c) = ""
                | otherwise = clContext c ++ " => "
        params = intercalate " " $ map snd (toList (clTypeParams c))


printClassFun :: Class -> [Wrap] -> String
printClassFun cl ws@(w:_) = name ++ " :: " ++ intercalate " -> " arg_list
  where ts = argTuples ws
        name = unShortName $ wrapShortName w
        arg_list = zipWith argTuple2arg ts hs_args
        argTuple2arg t hs_t = maybe hs_t id (Map.lookup t (clTypeParams cl))
        hs_args = map hsArg (wrapArgs w)
          
        
createClass :: [[Wrap]] -> String -> M Class
createClass wraps cname = do
  check <- checkUniqueTuples unique_tss 
  if check 
    then return cl
    else logger "There was a non-unique tuple" >> return cl
  where
    cl = Class { clContext = "(" ++ intercalate "," (map (f . snd) (toList typeParams)) ++ ")"
              ,  clName = cname  
              ,  clTypeParams = typeParams
              ,  clWraps = wraps }
      where f c = "Field1 " ++ c
    
    tss = map argTuples wraps
    unique_tss = uniqueTuples $ concat tss
    uts = reverse $ sortBy (\a b -> compare (length (nub a)) (length (nub b))) $ unique_tss
    -- Mapping C type-string tuples to type parameters
    typeParams = fromList $ zip uts (map (:[]) ['a'..])
    

argTuples :: [Wrap] -> [[String]]
argTuples ws = transpose $ map (\a -> map cArg (wrapArgs a)) ws

tuplesCounts :: Eq a => [[a]] -> [Int]
tuplesCounts a = map length $ map nub a

-- Find the tuples which are unique and have length > 1.
-- I.e. throw away the argument tuples which are the same in all classes, and keep
-- the tuples which are different in some of them.
uniqueTuples :: [[String]] -> [[String]]
uniqueTuples ts = gs
  where gs = filter ((> 1) . length . nub) $ concatMap nub $ group $ sort ts
        

-- At no point in a list of corresponding C argument type tuples
-- may there be a type coming up more than once. I.e., the resulting mapping must be reversible.
checkUniqueTuples :: [[String]] -> M Bool
checkUniqueTuples t = do
  let check = l1 == l2
  when (not check) (logger $ "Non-unique tuples: " ++ show t)
  return check
  where l1 = map length tt
        l2 = map (length . nub) tt
        tt = transpose t



differingArgs :: Wrap -> Wrap -> M (Map BClassCode DiffArg)
differingArgs w1 w2 = do
  if argPairsAreUnique alist
    then return $ fromList $ concatMap diffArgs alist
    else logger ("Functions have incompatible arguments: " ++ unCName (wrapCName w1) ++ ", " ++ unCName (wrapCName w2)) >> 
         return empty
  where 
    -- assoc list of arguments and their positions
    alist = uniquify $ catMaybes $ zipWith3 f (wrapArgs w1) (wrapArgs w2) [0..] 

    diffArgs ((ca1,ca2),i) = [(wrapClass w1, DiffArg ca1 i), (wrapClass w2, DiffArg ca2 i)]
    
    f a1 a2 i = if ca1 /= ca2 
                then Just ((ca1,ca2), i) -- cName 1, cName 2, and position in the argument list.
                else Nothing
      where { ca1 = cArg a1; ca2 = cArg a2 }


--groupArguments :: [[Wrap]] -> [[Wrap]]
--groupArguments ws
-- | Finds out whether differing argument pairs are occurring only once.
argPairsAreUnique :: [((String,String),Int)] -> Bool
argPairsAreUnique alist = isNothing $ find (/= 1) $ concat $ occurrenceCounts alist

occurrenceCounts :: [((String,String),Int)] -> [[Int]]
occurrenceCounts alist = [(count1 $ map fst alist),(count1 $ map (swap . fst) alist)]
swap (a,b) = (b,a)



nameAndClass :: CFunction -> Maybe (CName, HsName, ShortName, BClassCode)
nameAndClass (CFunction n ft) | "_work" `isSuffixOf` n = Nothing
                              | "LAPACKE_" `isPrefixOf` n = Just (CName n, HsName n', ShortName n'', typeCode)
                              | otherwise = Nothing
  where n' | "_work" `isSuffixOf` n = []
           | "LAPACKE_" `isPrefixOf` n = drop 8 n
           | "LAPACK_" `isPrefixOf` n = []
           | otherwise = n

       -- Handle some exceptions...
        headElem | null n' = '\0'
                 | otherwise = head n'
        (n'', typeCode) | n' == "scnrm2" = ("nrm2", BComplex)
                        | n' == "dznrm2" = ("nrm2", BZomplex)                 
                        | n' == "scasum" = ("asum", BComplex)
                        | n' == "dzasum" = ("asum", BZomplex)
                        | n' == "dsdot"  =  ("dot", BSingle)
                        | n' == "sdsdot" = ("sdsdot", Extra)
                        | n' == "sdot"   = ("sdot", Extra)
                        | n' == "cscal"  = ("scal", BComplex)
                        | n' == "zscal"  = ("scal", BZomplex)                                                             
                        | n' == "csscal" = ("scal'", BComplex)
                        | n' == "zdscal" = ("scal'", BZomplex)                                                
                        | n' == "sgesvd" = ("gesvd", BSingle)
                        | n' == "dgesvd" = ("gesvd", BDouble)
                        | n' == "cgesvd" = ("gesvd", BComplex)
                        | n' == "zgesvd" = ("gesvd", BZomplex)
                        | headElem == 's' = (tail n', BSingle)
                        | headElem == 'd' = (tail n', BDouble)                                             
                        | headElem == 'c' = (tail n', BComplex)                                             
                        | headElem == 'z' = (tail n', BZomplex)                                             
                        | "is" `isPrefixOf` n' = ('i' : drop 2 n', BSingle) -- isamax
                        | "id" `isPrefixOf` n' = ('i' : drop 2 n', BDouble) -- idamax
                        | "ic" `isPrefixOf` n' = ('i' : drop 2 n', BComplex) -- isamax
                        | "iz" `isPrefixOf` n' = ('i' : drop 2 n', BZomplex) -- idamax
                        | otherwise = (n', Extra) -- error $ "Failed to convert c function " ++ n -- (hsname',Extra)
      

paramName :: ParamDecl -> M String
paramName (ParamDecl (VarDecl (VarName (Ident arg_name _ _) _) _ _) _) = return arg_name
paramName _ = return ""

paramType :: ParamDecl -> M Type
paramType (ParamDecl (VarDecl _ _ t) _) = return t

typeString :: Type -> M String
typeString (DirectType tn _ _) = typeNameString tn
typeString (PtrType t _ _) = typeString t >>= \s -> s `seq` return (s ++ "*")
typeString (TypeDefType (TypeDefRef (Ident n _ _) mtype _) _ _) = return n
typeString _ = logger ("typeString: unsupported type") >> return [] -- error "typeString: unsupported type: " ++ (show . pretty) t

typeNameString :: TypeName -> M String
typeNameString TyVoid = return "void"
typeNameString (TyIntegral n) = return $ show n
typeNameString (TyFloating n) = return $ show n
typeNameString (TyComplex n)  = return $ show n
typeNameString (TyComp (CompTypeRef n _ _))  = return $ show n
typeNameString (TyEnum (EnumTypeRef n _))   = return $ show n
typeNameString (TyBuiltin _)   = logger ("typeNameString: TyBuiltin found. Not so good.") >> return [] -- error "TyBuiltin found."

typeArg :: String -> Type -> M (Maybe Arg)
typeArg n t = do
              ts <- typeString t
              maybe (logger ("typeArg: " ++ ts ++ " could not be matched.") >> return Nothing) (return . Just) (m ts)
  where m ts = find (\a -> (ts,"") == (cArg a, argName a) || (ts,n) == (cArg a, argName a)) lapackArgs

paramArg :: ParamDecl -> M (Maybe Arg)
paramArg p = do
  t <- paramType p
  n <- paramName p
  typeArg n t

