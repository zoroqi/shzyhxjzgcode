module Main (main) where

import Lib
import qualified Data.Map as M
import qualified Data.ByteString as B
import Data.Bits
import Data.Word


main :: IO ()
main = do
    input <- B.readFile "input.txt"
    out <- return (encode input)
    putStrLn out

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

-- decode :: String -> B.ByteString
-- decode = let
--
--     in
--         verify .readd
--     where
--         readd [] = []
--         readd [x] = error "input is short"
--         readd (x:y:xs) = [x,y]:(read xs)
--         verify k = case M.member k shzyhxjzg of
--             Just v -> v
--             Nothing -> error "not found shzyhxjzg"
--
-- mergeWord8 :: [Word8] -> Word8
-- mergeWord8 [hight,low] = hight `shiftL` 4 .|. low
