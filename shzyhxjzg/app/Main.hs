module Main (main) where

import Lib
import qualified Data.Map as M
import qualified Data.ByteString as B
import Data.Bits
import Data.Word
import System.Posix.IO as SPB
import System.IO(interact)
import GHC.IO.Handle (Handle)
import Data.ByteString.Internal(packChars,unpackChars)
import System.Environment(getArgs)
import System.Console.GetOpt
import Data.Maybe(fromMaybe)
import System.IO (hFlush, putStrLn, stderr, stdout)

data Options = Options
    { optMode     :: Maybe String
    -- , optShowVersion :: Bool
    -- , optOutput      :: Maybe FilePath
    -- , optInput       :: Maybe FilePath
    -- , optLibDirs     :: [FilePath]
    } deriving Show

defaultOptions    = Options
    { optMode     = Just "en"
    -- , optShowVersion = False
    -- , optOutput      = Nothing
    -- , optInput       = Nothing
    -- , optLibDirs     = []
    }

options :: [OptDescr (Options -> Options)]
options =
    [Option ['m']     ["mode"]
        (OptArg ((\f opts -> opts { optMode = Just f }) . fromMaybe "en")
                "mode")
        "mode en(encode)/de(decode)"
 -- , Option ['V','?'] ["version"]
     -- (NoArg (\ opts -> opts { optShowVersion = True }))
     -- "show version number"
 -- , Option ['o']     ["output"]
     -- (OptArg ((\ f opts -> opts { optOutput = Just f }) . fromMaybe "output")
             -- "FILE")
     -- "output FILE"
 -- , Option ['c']     []
     -- (OptArg ((\ f opts -> opts { optInput = Just f }) . fromMaybe "input")
             -- "FILE")
     -- "input FILE"
 -- , Option ['L']     ["libdir"]
     -- (ReqArg (\ d opts -> opts { optLibDirs = optLibDirs opts ++ [d] }) "DIR")
     -- "library directory"
 ]

parseArgs :: IO Options
parseArgs = do
    argv <- getArgs
    case getOpt Permute options argv of
        (o,_,[]) -> return (foldl (flip id) defaultOptions o)
        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: ic [OPTION...] files..."

main :: IO ()
main = do
    arg <- parseArgs
    case optMode arg of
        Just "en" -> en
        Just "de" -> de
        Just "" -> en
        Just f -> error f
        Nothing -> error "no mode"
    where
        en = interact (encode . packChars)
        de = interact (unpackChars . B.pack . decode)

shzyhxjzg :: M.Map String Int
shzyhxjzg = M.fromList [("富强", 0),("民主", 1),("文明", 2),("和谐", 3),("自由", 4),("平等", 5),("公正", 6),("法治", 7),("爱国", 8),("敬业", 9),("诚信", 10),("友善", 11)]

encodeCodeMap :: M.Map Word8 String
encodeCodeMap = M.fromList [(0, "富强"), (1, "民主"),(2, "文明"),(3, "和谐"),(4, "自由"),(5, "平等"),(6, "公正"),(7, "法治"),(8, "爱国"),(9, "敬业"),(10, "诚信富强"),(11, "友善文明"),(12, "友善平等"),(13, "友善法治"),(14, "诚信爱国"),(15, "诚信友善")]

decodeCodeMap :: M.Map String Word8
decodeCodeMap = M.fromList . map (\(k,v) -> (v,k)) $ M.toList encodeCodeMap

encode :: B.ByteString -> String
encode = concat . map get . concat . map splitWord8 . B.unpack
    where
        get k = case M.lookup k encodeCodeMap of
            Just v -> v
            Nothing -> error "not found codes"

splitWord8 :: Word8 -> [Word8]
splitWord8 w = [(w .&. 0xf0) `shiftR` 4, w .&. 0x0f]

decode :: String -> [Word8]
decode = map mergeWord8 . splitWord . map tocode . group . splitString
    where
        splitString [] = []
        splitString [_] = []
        splitString (x:y:xs) = [x,y]:(splitString xs)
        group [] = []
        group [_] = []
        group (x:xs@(y:ys)) = case M.lookup x shzyhxjzg of
           Just v -> if v >= 10 then (x++y):(group ys) else x:(group xs)
           Nothing -> error "not found shzyhxjzg"
        tocode k = case M.lookup k decodeCodeMap of
           Just v -> v
           Nothing -> error "not found codes"
        splitWord [] = []
        splitWord [_] = []
        splitWord (x:y:xs) = (x,y):splitWord xs


mergeWord8 :: (Word8,Word8) -> Word8
mergeWord8 (hight,low) = hight `shiftL` 4 .|. low
