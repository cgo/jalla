import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.Program
import Distribution.Simple.Program.Types
import Distribution.Simple.Utils
import Distribution.Verbosity
import Distribution.PackageDescription
import System.FilePath
import System.Process
import System.Exit

main = defaultMainWithHooks $ 
         simpleUserHooks {
           hookedPrograms = [], --scons : hookedPrograms simpleUserHooks,
           preConf = sconsPreConf -- >> (preConf simpleUserHooks) a c
           }


sconsPreConf :: Args -> ConfigFlags -> IO HookedBuildInfo
sconsPreConf args cf = do
  let flags = configConfigurationsFlags cf
      mf = lookup (FlagName "build_lapacke") flags
  case mf of
    Just True -> invokeScons
    _ -> return (Nothing, [])
  
invokeScons = do
  let lapackeDir = currentDir </> "lapacke"
  msl <- programFindLocation scons verbose
  let mSconsInvoke = msl >>= \sl -> 
        return (simpleConfiguredProgram "scons" (FoundOnSystem sl)) >>= \confScons ->
        return (programInvocation confScons []) >>= \sconsInvoke ->
        return (sconsInvoke { progInvokeCwd = Just lapackeDir })
        
  success <- 
    case mSconsInvoke of
      Just sconsInvoke -> 
        callProgram (progInvokePath sconsInvoke) [] (progInvokeCwd sconsInvoke)
      -- runProgramInvocation deafening sconsInvoke -- This is apparently not implemented in cabal.
      _ -> 
        die "*** Running scons has failed while trying to build LAPACKE. Is scons installed?" >>
        return False
  
  return $ if success 
           then (Just (emptyBuildInfo { buildable = True }), [])
           else (Just (emptyBuildInfo { buildable = False }), [])


callProgram :: FilePath -> [String] -> Maybe FilePath -> IO Bool
callProgram prog args cwd = do
  h <- runProcess prog args cwd Nothing Nothing Nothing Nothing 
  e <- waitForProcess h
  return $ e == ExitSuccess 

findScons :: Verbosity -> IO (Maybe FilePath)
findScons verb = do
  let locs = ["/usr/bin","/usr/local/bin","/usr/sbin","/usr/local/sbin"]
  ml <- mapM (findProgramLocation verb) locs
  return $ firstJust ml

scons :: Program
scons = simpleProgram "scons"

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Just a:as) = Just a
firstJust (_:as) = firstJust as

