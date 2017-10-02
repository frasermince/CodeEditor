module Main where
import Eval
import Parser
import Types (EditorParseError, Command(..))
import Pipes ((>->), for, each, yield)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Pipes.Prelude (toListM)
import Data.ByteString.Char8 as BC
import Data.ByteString as B
import Criterion.Main
import qualified Pipes.Prelude as P

handleEither :: Either EditorParseError a -> IO ()
handleEither (Left e) = Prelude.putStrLn $ "Error: " ++ show e
handleEither (Right value) = do
  Prelude.putStrLn $ "Testing"
  return ()

handleEitherWithValue :: Either EditorParseError [Command] -> IO [Command]
handleEitherWithValue (Left e) = do Prelude.putStrLn $ "Error: " ++ show e
                                    return []
handleEitherWithValue (Right value) = do
  Prelude.putStrLn $ "Testing"
  return value


parseList file n = toListM $ (for (each $ BC.lines $ file) yield) >-> P.take n >-> parsePipe
parseOnlyPipeline file n = do eitherVal <- (runExceptT $ parseList file n)
                              handleEither eitherVal

evalOnlyPipeline parses = do eitherVal <- runExceptT $ toListM $ ((for (each parses) yield) >-> evalPipe)
                             handleEither eitherVal

main :: IO ()
main = do

  file <- B.readFile "./benchInput.txt"
  let n = 1000000
  exceptParses <- runExceptT $ parseList file n
  parses <- handleEitherWithValue exceptParses
  defaultMain [ bench "parse" $ nfIO $ parseOnlyPipeline file n
              , bench "eval" $ nfIO $ evalOnlyPipeline parses
              ]
