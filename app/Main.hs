module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Mash
import           System.Environment        (getArgs)

main :: IO ()
main = do
  args <- getArgs
  print =<< execMashM (build args >> corrupt) defaultOptions

-- | use our state monad to modify the options record for each pair of args
build :: [String] -> MashM ()
build [_] = liftIO usage
build xs = forM_ (chunksOf 2 xs) $ \pair ->
  case pair of
    ["--start", s]     -> modify $ \r -> r { start = Just $ parse s }
    ["--end", e]       -> modify $ \r -> r { end   = Just $ parse e }
    ["--skip", s]      -> modify $ \r -> r { skip  = parse s }
    ["--increment", n] -> modify $ \r -> r { ops   = Increment (parse n) : ops r }
    ["--decrement", n] -> modify $ \r -> r { ops   = Decrement (parse n) : ops r }
    ["--left", n]      -> modify $ \r -> r { ops   = LShift (parse n) : ops r }
    ["--right", n]     -> modify $ \r -> r { ops   = RShift (parse n) : ops r }
    ["--file", f]      -> modify $ \r -> r { file  = Just f }
    ["--to", f]        -> modify $ \r -> r { to = f }
    ["--help", _]      -> liftIO usage
    _                  -> liftIO usage

usage :: IO ()
usage = do
  putStrLn "mash: [OPTIONS] filename"
  putStrLn "--start start index"
  putStrLn "--end end index"
  putStrLn "--skip corrupt every n bytes"
  putStrLn "--increment increment by n"
  putStrLn "--decrement decrement by n"
  putStrLn "--left bitwise shift left"
  putStrLn "--right bitwise shift right"
  putStrLn "--file filename to corrupt"

-- | read will gladly parse both hex and decimal for us.
parse :: String -> Int
parse = read
