import * as Consts from "../constants/action-types";
import { combineReducers } from "redux";
import { initialFactState, factReducer } from "./factReducer";
import { initialCandidateState, candidateReducer } from "./candidateReducer";

const initialState = {
    numArgs: 2,
    facts: initialFactState,
    candidates: initialCandidateState,
};

// This allows us to silo state in these separate areas and break the code
// out into different more self contained files.
const restOfReducers = combineReducers({
    candidates: candidateReducer,
    facts: factReducer
})

const rootReducer = (state = initialState, action) => {
    switch (action.type) {
        case Consts.SET_COMPONENTS:
            return {
                candidates: [{
                        code: "\\arg0 arg1-> replicate arg0 arg1",
                        examples: [
                            ["x", "2", "xx"],
                            ["x", "0", ""],
                        ]
                    },
                    {
                        code: "\\arg0 arg1-> replicate2 arg0 arg1",
                        examples: [
                            ["y", "3", "yyy"]
                        ]
                    }
                ],
                library: [{
                        moduleName: "Data.Bool",
                        components: [
                            "Data.Bool",
                            "data Bool",
                            "False :: Bool",
                            "True :: Bool",
                            "(&&) :: Bool -> Bool -> Bool",
                            "infixr 3 &&",
                            "(||) :: Bool -> Bool -> Bool",
                            "infixr 2 ||",
                            "not :: Bool -> Bool",
                            "otherwise :: Bool",
                            "bool :: a -> a -> Bool -> a"
                        ]
                    },
                    {
                        moduleName: "Data.ByteString.Builder",
                        components: [
                            "Data.ByteString.Builder",
                            "data Builder",
                            "toLazyByteString :: Builder -> ByteString",
                            "hPutBuilder :: Handle -> Builder -> IO ()",
                            "byteString :: ByteString -> Builder",
                            "lazyByteString :: ByteString -> Builder",
                            "shortByteString :: ShortByteString -> Builder",
                            "int8 :: Int8 -> Builder",
                            "word8 :: Word8 -> Builder",
                            "int16BE :: Int16 -> Builder",
                            "int32BE :: Int32 -> Builder",
                            "int64BE :: Int64 -> Builder",
                            "word16BE :: Word16 -> Builder",
                            "word32BE :: Word32 -> Builder",
                            "word64BE :: Word64 -> Builder",
                            "floatBE :: Float -> Builder",
                            "doubleBE :: Double -> Builder",
                            "int16LE :: Int16 -> Builder",
                            "int32LE :: Int32 -> Builder",
                            "int64LE :: Int64 -> Builder",
                            "word16LE :: Word16 -> Builder",
                            "word32LE :: Word32 -> Builder",
                            "word64LE :: Word64 -> Builder",
                            "floatLE :: Float -> Builder",
                            "doubleLE :: Double -> Builder",
                            "char7 :: Char -> Builder",
                            "string7 :: String -> Builder",
                            "char8 :: Char -> Builder",
                            "string8 :: String -> Builder",
                            "charUtf8 :: Char -> Builder",
                            "stringUtf8 :: String -> Builder",
                            "int8Dec :: Int8 -> Builder",
                            "int16Dec :: Int16 -> Builder",
                            "int32Dec :: Int32 -> Builder",
                            "int64Dec :: Int64 -> Builder",
                            "intDec :: Int -> Builder",
                            "integerDec :: Integer -> Builder",
                            "word8Dec :: Word8 -> Builder",
                            "word16Dec :: Word16 -> Builder",
                            "word32Dec :: Word32 -> Builder",
                            "word64Dec :: Word64 -> Builder",
                            "wordDec :: Word -> Builder",
                            "floatDec :: Float -> Builder",
                            "doubleDec :: Double -> Builder",
                            "word8Hex :: Word8 -> Builder",
                            "word16Hex :: Word16 -> Builder",
                            "word32Hex :: Word32 -> Builder",
                            "word64Hex :: Word64 -> Builder",
                            "wordHex :: Word -> Builder",
                            "int8HexFixed :: Int8 -> Builder",
                            "int16HexFixed :: Int16 -> Builder",
                            "int32HexFixed :: Int32 -> Builder",
                            "int64HexFixed :: Int64 -> Builder",
                            "word8HexFixed :: Word8 -> Builder",
                            "word16HexFixed :: Word16 -> Builder",
                            "word32HexFixed :: Word32 -> Builder",
                            "word64HexFixed :: Word64 -> Builder",
                            "floatHexFixed :: Float -> Builder",
                            "doubleHexFixed :: Double -> Builder",
                            "byteStringHex :: ByteString -> Builder",
                            "lazyByteStringHex :: ByteString -> Builder",
                            "instance Data.String.IsString",
                            "           Data.ByteString.Builder.Internal.Builder"
                        ]
                    },
                    {
                        moduleName: "Data.ByteString.Lazy",
                        components: [
                            "Data.ByteString.Lazy",
                            "data ByteString",
                            "empty :: ByteString",
                            "singleton :: Word8 -> ByteString",
                            "pack :: [Word8] -> ByteString",
                            "unpack :: ByteString -> [Word8]",
                            "fromStrict :: ByteString -> ByteString",
                            "toStrict :: ByteString -> ByteString",
                            "fromChunks :: [ByteString] -> ByteString",
                            "toChunks :: ByteString -> [ByteString]",
                            "foldrChunks :: (ByteString -> a -> a) -> a -> ByteString -> a",
                            "foldlChunks :: (a -> ByteString -> a) -> a -> ByteString -> a",
                            "cons :: Word8 -> ByteString -> ByteString",
                            "infixr 5 `cons`",
                            "cons' :: Word8 -> ByteString -> ByteString",
                            "infixr 5 `cons'`",
                            "snoc :: ByteString -> Word8 -> ByteString",
                            "infixl 5 `snoc`",
                            "append :: ByteString -> ByteString -> ByteString",
                            "head :: ByteString -> Word8",
                            "uncons :: ByteString -> Maybe (Word8, ByteString)",
                            "unsnoc :: ByteString -> Maybe (ByteString, Word8)",
                            "last :: ByteString -> Word8",
                            "tail :: ByteString -> ByteString",
                            "init :: ByteString -> ByteString",
                            "null :: ByteString -> Bool",
                            "length :: ByteString -> Int64",
                            "map :: (Word8 -> Word8) -> ByteString -> ByteString",
                            "reverse :: ByteString -> ByteString",
                            "intersperse :: Word8 -> ByteString -> ByteString",
                            "intercalate :: ByteString -> [ByteString] -> ByteString",
                            "transpose :: [ByteString] -> [ByteString]",
                            "foldl :: (a -> Word8 -> a) -> a -> ByteString -> a",
                            "foldl' :: (a -> Word8 -> a) -> a -> ByteString -> a",
                            "foldl1 :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8",
                            "foldl1' :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8",
                            "foldr :: (Word8 -> a -> a) -> a -> ByteString -> a",
                            "foldr1 :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8",
                            "concat :: [ByteString] -> ByteString",
                            "concatMap :: (Word8 -> ByteString) -> ByteString -> ByteString",
                            "any :: (Word8 -> Bool) -> ByteString -> Bool",
                            "all :: (Word8 -> Bool) -> ByteString -> Bool",
                            "maximum :: ByteString -> Word8",
                            "minimum :: ByteString -> Word8",
                            "scanl ::",
                            "      (Word8 -> Word8 -> Word8) -> Word8 -> ByteString -> ByteString",
                            "mapAccumL ::",
                            "          (acc -> Word8 -> (acc, Word8)) ->",
                            "            acc -> ByteString -> (acc, ByteString)",
                            "mapAccumR ::",
                            "          (acc -> Word8 -> (acc, Word8)) ->",
                            "            acc -> ByteString -> (acc, ByteString)",
                            "repeat :: Word8 -> ByteString",
                            "replicate :: Int64 -> Word8 -> ByteString",
                            "cycle :: ByteString -> ByteString",
                            "iterate :: (Word8 -> Word8) -> Word8 -> ByteString",
                            "unfoldr :: (a -> Maybe (Word8, a)) -> a -> ByteString",
                            "take :: Int64 -> ByteString -> ByteString",
                            "drop :: Int64 -> ByteString -> ByteString",
                            "splitAt :: Int64 -> ByteString -> (ByteString, ByteString)",
                            "takeWhile :: (Word8 -> Bool) -> ByteString -> ByteString",
                            "dropWhile :: (Word8 -> Bool) -> ByteString -> ByteString",
                            "span :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)",
                            "break :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)",
                            "group :: ByteString -> [ByteString]",
                            "groupBy :: (Word8 -> Word8 -> Bool) -> ByteString -> [ByteString]",
                            "inits :: ByteString -> [ByteString]",
                            "tails :: ByteString -> [ByteString]",
                            "stripPrefix :: ByteString -> ByteString -> Maybe ByteString",
                            "stripSuffix :: ByteString -> ByteString -> Maybe ByteString",
                            "split :: Word8 -> ByteString -> [ByteString]",
                            "splitWith :: (Word8 -> Bool) -> ByteString -> [ByteString]",
                            "isPrefixOf :: ByteString -> ByteString -> Bool",
                            "isSuffixOf :: ByteString -> ByteString -> Bool",
                            "elem :: Word8 -> ByteString -> Bool",
                            "notElem :: Word8 -> ByteString -> Bool",
                            "find :: (Word8 -> Bool) -> ByteString -> Maybe Word8",
                            "filter :: (Word8 -> Bool) -> ByteString -> ByteString",
                            "partition ::",
                            "          (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)",
                            "index :: ByteString -> Int64 -> Word8",
                            "elemIndex :: Word8 -> ByteString -> Maybe Int64",
                            "elemIndexEnd :: Word8 -> ByteString -> Maybe Int64",
                            "elemIndices :: Word8 -> ByteString -> [Int64]",
                            "findIndex :: (Word8 -> Bool) -> ByteString -> Maybe Int64",
                            "findIndices :: (Word8 -> Bool) -> ByteString -> [Int64]",
                            "count :: Word8 -> ByteString -> Int64",
                            "zip :: ByteString -> ByteString -> [(Word8, Word8)]",
                            "zipWith :: (Word8 -> Word8 -> a) -> ByteString -> ByteString -> [a]",
                            "unzip :: [(Word8, Word8)] -> (ByteString, ByteString)",
                            "copy :: ByteString -> ByteString",
                            "getContents :: IO ByteString",
                            "putStr :: ByteString -> IO ()",
                            "putStrLn :: ByteString -> IO ()",
                            "interact :: (ByteString -> ByteString) -> IO ()",
                            "readFile :: FilePath -> IO ByteString",
                            "writeFile :: FilePath -> ByteString -> IO ()",
                            "appendFile :: FilePath -> ByteString -> IO ()",
                            "hGetContents :: Handle -> IO ByteString",
                            "hGet :: Handle -> Int -> IO ByteString",
                            "hGetNonBlocking :: Handle -> Int -> IO ByteString",
                            "hPut :: Handle -> ByteString -> IO ()",
                            "hPutNonBlocking :: Handle -> ByteString -> IO ByteString",
                            "hPutStr :: Handle -> ByteString -> IO ()"
                        ]
                    },
                    {
                        moduleName: "Data.Either",
                        components: [
                            "Data.Either",
                            "data Either a b",
                            "Left :: a -> Either a b",
                            "Right :: b -> Either a b",
                            "either :: (a -> c) -> (b -> c) -> Either a b -> c",
                            "lefts :: [Either a b] -> [a]",
                            "rights :: [Either a b] -> [b]",
                            "isLeft :: Either a b -> Bool",
                            "isRight :: Either a b -> Bool",
                            "fromLeft :: a -> Either a b -> a",
                            "fromRight :: b -> Either a b -> b",
                            "partitionEithers :: [Either a b] -> ([a], [b])",
                            "instance (GHC.Show.Show a, GHC.Show.Show b) =>",
                            "         GHC.Show.Show (Data.Either.Either a b)",
                            "instance (GHC.Read.Read a, GHC.Read.Read b) =>",
                            "         GHC.Read.Read (Data.Either.Either a b)",
                            "instance (GHC.Classes.Ord a, GHC.Classes.Ord b) =>",
                            "         GHC.Classes.Ord (Data.Either.Either a b)",
                            "instance (GHC.Classes.Eq a, GHC.Classes.Eq b) =>",
                            "         GHC.Classes.Eq (Data.Either.Either a b)",
                            "instance GHC.Base.Functor (Data.Either.Either a)",
                            "instance GHC.Base.Semigroup (Data.Either.Either a b)",
                            "instance GHC.Base.Applicative (Data.Either.Either e)",
                            "instance GHC.Base.Monad (Data.Either.Either e)"
                        ]
                    },
                    {
                        moduleName: "Data.Eq",
                        components: [
                            "Data.Eq",
                            "class Eq a",
                            "(==) :: Eq a => a -> a -> Bool",
                            "(/=) :: Eq a => a -> a -> Bool",
                            "infix 4 ==",
                            "infix 4 /=",
                            "instance Eq a => Eq [a]"
                        ]
                    },
                    {
                        moduleName: "Data.Function",
                        components: [
                            "Data.Function",
                            "id :: a -> a",
                            "const :: a -> b -> a",
                            "(.) :: (b -> c) -> (a -> b) -> a -> c",
                            "infixr 9 .",
                            "flip :: (a -> b -> c) -> b -> a -> c",
                            "($) :: forall r a (b :: TYPE r) . (a -> b) -> a -> b",
                            "infixr 0 $",
                            "(&) :: a -> (a -> b) -> b",
                            "infixl 1 &",
                            "fix :: (a -> a) -> a",
                            "on :: (b -> b -> c) -> (a -> b) -> a -> a -> c",
                            "infixl 0 `on`"
                        ]
                    },
                    {
                        moduleName: "Data.Int",
                        components: [
                            "Data.Int",
                            "data Int",
                            "data Int8",
                            "data Int16",
                            "data Int32",
                            "data Int64"
                        ]
                    },
                    {
                        moduleName: "Data.Maybe",
                        components: [
                            "Data.Maybe",
                            "data Maybe a",
                            "Nothing :: Maybe a",
                            "Just :: a -> Maybe a",
                            "maybe :: b -> (a -> b) -> Maybe a -> b",
                            "isJust :: Maybe a -> Bool",
                            "isNothing :: Maybe a -> Bool",
                            "fromJust :: Maybe a -> a",
                            "fromMaybe :: a -> Maybe a -> a",
                            "listToMaybe :: [a] -> Maybe a",
                            "maybeToList :: Maybe a -> [a]",
                            "catMaybes :: [Maybe a] -> [a]",
                            "mapMaybe :: (a -> Maybe b) -> [a] -> [b]"
                        ]
                    },
                    {
                        moduleName: "Data.Tuple",
                        components: [
                            "Data.Tuple",
                            "fst :: (a, b) -> a",
                            "snd :: (a, b) -> b",
                            "curry :: ((a, b) -> c) -> a -> b -> c",
                            "uncurry :: (a -> b -> c) -> (a, b) -> c",
                            "swap :: (a, b) -> (b, a)"
                        ]
                    },
                    {
                        moduleName: "GHC.Char",
                        components: [
                            "GHC.Char",
                            "chr :: Int -> Char",
                            "eqChar :: Char -> Char -> Bool",
                            "neChar :: Char -> Char -> Bool"
                        ]
                    },
                    {
                        moduleName: "GHC.List",
                        components: [
                            "GHC.List",
                            "map :: (a -> b) -> [a] -> [b]",
                            "(++) :: [a] -> [a] -> [a]",
                            "infixr 5 ++",
                            "filter :: (a -> Bool) -> [a] -> [a]",
                            "concat :: [[a]] -> [a]",
                            "head :: [a] -> a",
                            "last :: [a] -> a",
                            "tail :: [a] -> [a]",
                            "init :: [a] -> [a]",
                            "uncons :: [a] -> Maybe (a, [a])",
                            "null :: [a] -> Bool",
                            "length :: [a] -> Int",
                            "(!!) :: [a] -> Int -> a",
                            "infixl 9 !!",
                            "foldl :: forall a b . (b -> a -> b) -> b -> [a] -> b",
                            "foldl' :: forall a b . (b -> a -> b) -> b -> [a] -> b",
                            "foldl1 :: (a -> a -> a) -> [a] -> a",
                            "foldl1' :: (a -> a -> a) -> [a] -> a",
                            "scanl :: (b -> a -> b) -> b -> [a] -> [b]",
                            "scanl1 :: (a -> a -> a) -> [a] -> [a]",
                            "scanl' :: (b -> a -> b) -> b -> [a] -> [b]",
                            "foldr :: (a -> b -> b) -> b -> [a] -> b",
                            "foldr1 :: (a -> a -> a) -> [a] -> a",
                            "scanr :: (a -> b -> b) -> b -> [a] -> [b]",
                            "scanr1 :: (a -> a -> a) -> [a] -> [a]",
                            "iterate :: (a -> a) -> a -> [a]",
                            "iterate' :: (a -> a) -> a -> [a]",
                            "repeat :: a -> [a]",
                            "replicate :: Int -> a -> [a]",
                            "cycle :: [a] -> [a]",
                            "take :: Int -> [a] -> [a]",
                            "drop :: Int -> [a] -> [a]",
                            "sum :: Num a => [a] -> a",
                            "product :: Num a => [a] -> a",
                            "maximum :: Ord a => [a] -> a",
                            "minimum :: Ord a => [a] -> a",
                            "splitAt :: Int -> [a] -> ([a], [a])",
                            "takeWhile :: (a -> Bool) -> [a] -> [a]",
                            "dropWhile :: (a -> Bool) -> [a] -> [a]",
                            "span :: (a -> Bool) -> [a] -> ([a], [a])",
                            "break :: (a -> Bool) -> [a] -> ([a], [a])",
                            "reverse :: [a] -> [a]",
                            "and :: [Bool] -> Bool",
                            "or :: [Bool] -> Bool",
                            "any :: (a -> Bool) -> [a] -> Bool",
                            "all :: (a -> Bool) -> [a] -> Bool",
                            "elem :: Eq a => a -> [a] -> Bool",
                            "infix 4 `elem`",
                            "notElem :: Eq a => a -> [a] -> Bool",
                            "infix 4 `notElem`",
                            "lookup :: Eq a => a -> [(a, b)] -> Maybe b",
                            "concatMap :: (a -> [b]) -> [a] -> [b]",
                            "zip :: [a] -> [b] -> [(a, b)]",
                            "zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]",
                            "zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]",
                            "zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]",
                            "unzip :: [(a, b)] -> ([a], [b])",
                            "unzip3 :: [(a, b, c)] -> ([a], [b], [c])"
                        ]
                    },
                    {
                        moduleName: "Text.Show",
                        components: [
                            "Text.Show",
                            "type ShowS = String -> String",
                            "class Show a",
                            "showsPrec :: Show a => Int -> a -> ShowS",
                            "show :: Show a => a -> String",
                            "showList :: Show a => [a] -> ShowS",
                            "shows :: Show a => a -> ShowS",
                            "showChar :: Char -> ShowS",
                            "showString :: String -> ShowS",
                            "showParen :: Bool -> ShowS -> ShowS",
                            "showListWith :: (a -> ShowS) -> [a] -> ShowS"
                        ]
                    }
                ]
            };

            //Catch any modifies of numArgs here.
        default:
            return {
                ...state,
                ...restOfReducers(state, action)
            }
    }
}

export default rootReducer;