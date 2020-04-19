import Control.Monad (filterM)
import Data.Maybe (listToMaybe)
import System.Environment (getArgs)
import System.Process (createProcess, waitForProcess, shell)
import System.Directory (renameFile, removeFile, doesFileExist)
import System.FilePath (hasExtension, replaceBaseName, takeBaseName)
import System.Exit (ExitCode(..))
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = mapM_ redo =<< getArgs 
  
redo :: String -> IO ()
redo target = maybe printMissing redo' =<< redoPath target
  where redo' :: FilePath -> IO ()
        redo' path = do
          (_, _, _, ph) <- createProcess $ shell $ cmd path
          exit <- waitForProcess ph
          case exit of 
            ExitSuccess -> do renameFile tmp target
            ExitFailure code -> do hPutStrLn stderr $ "Redo script exited with non zero exit code: " ++ show code
                                   removeFile tmp
        tmp = target ++ "---redoing"
        printMissing = error $ "file not found '" ++ target ++ "'"
        cmd path = unwords ["sh", path, "0", takeBaseName target, tmp, ">", tmp]

redoPath :: FilePath -> IO (Maybe FilePath)
redoPath target = listToMaybe <$> filterM doesFileExist candidates
  where candidates = [target ++ ".do"] ++ if hasExtension target 
                                          then [replaceBaseName target "default" ++ ".do"] 
                                          else []
