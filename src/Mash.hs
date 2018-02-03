module Mash where

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.State
import           Data.Bits                  (shiftL, shiftR)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Char                  (chr, ord)
import           Data.Maybe                 (fromMaybe)
import           System.Posix               (fileSize, getFileStatus)

data Operation = Increment Int | Decrement Int | LShift Int | RShift Int
  deriving (Read, Show, Eq, Ord)

data Options =
  Options { start :: Maybe Int
          , end   :: Maybe Int
          , skip  :: Int
          , file  :: Maybe String
          , to    :: String
          , ops   :: [Operation]}
  deriving (Read, Show)

defaultOptions :: Options
defaultOptions = Options { start = Nothing
                         , end = Nothing
                         , skip = 0
                         , file = Nothing
                         , to = "a.out"
                         , ops = [] }

type MashM a = StateT Options IO a

execMashM :: MashM a -> Options -> IO Options
execMashM = execStateT

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = chunkify []
  where chunkify dest [] = reverse dest
        chunkify dest xs = chunkify (take n xs : dest) (drop n xs)

corrupt :: MashM ()
corrupt = do
  options <- get
  (size, content) <- liftIO $ case (file options) of
    Just filename -> do
      size <- fromIntegral . fileSize <$> getFileStatus filename
      content <- BS.readFile filename
      return (size, content)
    Nothing -> do
      content <- BS.getContents
      return (fromIntegral $ BS.length content, content)
      
  let corrupted = with content $  \(index, char) ->
        let inRange = index >= fromMaybe 0 (start options) && index <= fromMaybe size (end options) in
          if not inRange then
            (index, char)
          else if skip options == 0 || index `mod` skip options == 0 then
            (index, foldr perform char $ ops options )
          else
            (index, char)
  liftIO $ BS.writeFile (to options) corrupted
  where with bs fn = BS.pack . map snd $ for (zip [0..] $ BS.unpack bs) fn

perform :: Operation -> Char -> Char
perform op byte =
  case op of
    Increment n -> head . drop n $ iterate succ byte
    Decrement n -> head . drop n $ iterate pred byte
    LShift    n -> chr $ ord byte `shiftL` n
    RShift    n -> chr $ ord byte `shiftR` n

for = flip map
