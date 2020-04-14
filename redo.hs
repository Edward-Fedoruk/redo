import Control.Monad (filterM)
import System.Environment (getArgs)
import System.Process (createProcess, waitForProcess, shell)
import System.Directory (renameFile, removeFile, doesFileExist)
import System.FilePath (hasExtension, replaceBaseName)
import System.Exit (ExitCode(..))
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do 
  args <- getArgs
  mapM_ redo args
  
redo :: String -> IO ()
redo target = do
  let tmp = target ++ "---redoing"
  maybePath <- redoPath target
  case maybePath of
    Nothing -> error $ "file not found '" ++ target ++ "'"
    Just path -> do
      (_, _, _, ph) <- createProcess $ shell $ "sh " ++ path ++ " - - " ++ tmp ++ " > " ++ tmp
      exit <- waitForProcess ph
      case exit of 
        ExitSuccess -> do renameFile tmp target
        ExitFailure code -> do hPutStrLn stderr $ "Redo script exited with non zero exit code: " ++ show code
                               removeFile tmp
      
redoPath :: FilePath -> IO (Maybe FilePath)
redoPath target = do
  fmap safeHead $ filterM doesFileExist candidates
  where candidates = [target ++ ".do"] ++ if hasExtension target then [replaceBaseName target "default" ++ ".do"] else []

safeHead [] = Nothing
safeHead (x:_) = Just x 