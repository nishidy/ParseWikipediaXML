import System.Environment(getArgs)
import System.IO
import System.Exit
import qualified Data.Map as M
import Data.Char
--import Control.Applicative
import qualified Text.Parsec as P
import Text.Regex.Posix
import Debug.Trace
--import Codec.Binary.UTF8.String
--import Control.Exception as E
import System.Locale.SetLocale
import Data.Time
import Text.Printf
import Control.Exception

type S = String

data ArgsRec = ArgsRec {
    inWikiFile :: String,
    inDictFile :: String,
    outBofwFile :: String,
    outTitleFile :: String,
    outTfIdfFile :: String,
    numMinTermsInDoc :: Int,
    numMaxTermsInDoc :: Int,
    numMinFreqOfTerm :: Int
} deriving Show

parseShortArgs (_:[]) rec = rec
parseShortArgs ([]) rec = rec
parseShortArgs (('-':op:[]):val:other) rec =
    let localrec = parseShortArgs other rec in
    case op of
        'i' -> localrec { inWikiFile = val }
        'd' -> localrec { inDictFile = val }
        's' -> localrec { outBofwFile = val }
        't' -> localrec { outTitleFile = val }
        'f' -> localrec { outTfIdfFile = val }
        'm' -> localrec { numMinTermsInDoc = read val::Int }
        'x' -> localrec { numMaxTermsInDoc = read val::Int }
        'c' -> localrec { numMinFreqOfTerm = read val::Int }
        _   -> localrec

parseLongArgs (_:[]) rec = rec
parseLongArgs ([]) rec = rec
parseLongArgs (('-':op):val:other) rec =
    let localrec = parseLongArgs other rec in
    case op of
        "inWikiFile" -> localrec { inWikiFile = val }
        "inDictFile" -> localrec { inDictFile = val }
        "outBofwFile" -> localrec { outBofwFile = val }
        "outTitleFile" -> localrec { outTitleFile = val }
        "outTfIdfFile" -> localrec { outTfIdfFile = val }
        "numMinTermsInDoc" -> localrec { numMinTermsInDoc = read val::Int }
        "numMaxTermsInDoc" -> localrec { numMaxTermsInDoc = read val::Int }
        "numMinFreqOfTerm" -> localrec { numMinFreqOfTerm = read val::Int }
        _ -> localrec

main :: IO ()
main = do

    setLocale LC_ALL (Just "C")
    args <- getArgs

    let init = ArgsRec {
        inWikiFile = "",
        inDictFile = "",
        outBofwFile = "",
        outTitleFile = "",
        outTfIdfFile = "",
        numMinTermsInDoc = 1,
        numMaxTermsInDoc = 65535,
        numMinFreqOfTerm = 1
    }

    shortargs <- return $ parseShortArgs args init
    args <- return $ parseLongArgs args shortargs

    s <- getCurrentTime
    mapDict <- getDictionaryFromFile args
    e <- getCurrentTime
    putStrLn $ " > Read dictionary in " ++ (printf "%.2f" (realToFrac  (diffUTCTime e s)::Double)) ++ " sec."

    stopwords <- return getStopwords
    getContentFromFile args mapDict stopwords

    --getTfIdf args

getTfIdf :: ArgsRec -> IO ()
getTfIdf args = 
    let
        (ArgsRec { outBofwFile = s }) = args
        (ArgsRec { outTfIdfFile = f }) = args
    in do
        hdlr <- openFile s ReadMode
        corpus <- getTfIdfLine hdlr M.empty
        appendFile f $ (show corpus) ++ "\n"

getTfIdfLine :: Handle -> M.Map S Int -> IO (M.Map S Int)
getTfIdfLine hdlr m = do
    isEOF <- hIsEOF hdlr
    if isEOF then return m
    else do
        line <- hGetLine hdlr
        getTfIdfLine hdlr $ getTermFreq m $ words line

getTermFreq :: M.Map S Int -> [S] -> M.Map S Int
getTermFreq m [] = m
getTermFreq m (x:y:xs) =
    let mx = getTermFreq m xs in
        M.insertWithKey (\_ _ o->o+yi) x yi mx
    where yi = read y::Int

getStopwords :: [S]
getStopwords =
    let stopwords = "a,able,about,across,after,all,almost,also,am,among,an,and,any,are,as,at,be,because,been,but,by,can,cannot,could,dear,did,do,does,either,else,ever,every,for,from,get,got,had,has,have,he,her,hers,him,his,how,however,i,if,in,into,is,it,its,just,least,let,like,likely,may,me,might,most,must,my,neither,no,nor,not,of,off,often,on,only,or,other,our,own,rather,said,say,says,she,should,since,so,some,than,that,the,their,them,then,there,these,they,this,tis,to,too,twas,us,wants,was,we,were,what,when,where,which,while,who,whom,why,will,with,would,yet,you,your"
    in splitByComma stopwords "" []

splitByComma :: S -> S -> [S] -> [S]
splitByComma [] _ arr = arr
splitByComma (x:xs) s arr | x==',' = splitByComma xs "" (s:arr)
splitByComma (x:xs) s arr = splitByComma xs (s++[x]) arr

getDictionaryFromFile :: ArgsRec -> IO (M.Map S S)
getDictionaryFromFile args = do
    let (ArgsRec { inDictFile = d }) = args
    hdlr <- openFile d ReadMode
    getLineFromInDictFile hdlr M.empty (0,0)

getLineFromInDictFile :: Handle -> M.Map S S -> (Int,Int) -> IO (M.Map S S)
getLineFromInDictFile hInDictFile mapDict (lc,pc) = do
    hInDictFileEOF <- hIsEOF hInDictFile
    if hInDictFileEOF then do
        putStrLn ""
        return mapDict
    else do
        line <- hGetLine hInDictFile
        newMapDict <- return $ getNewMapDict line mapDict
        if M.size newMapDict == M.size mapDict then do
            showReadDictProg (lc,pc+1)
            getLineFromInDictFile hInDictFile newMapDict (lc,pc+1)
        else do
            showReadDictProg (lc+1,pc+1)
            getLineFromInDictFile hInDictFile newMapDict (lc+1,pc+1)

getNewMapDict :: S -> M.Map S S -> M.Map S S
getNewMapDict line mapDict = do
    case line =~ "^([a-zA-Z'-]+) \t\t([a-zA-Z'-]+)\t" :: (S,S,S,[S]) of
        (_,_,_,(prac:base:_))
            | prac == base -> mapDict
            | otherwise -> M.insertWithKey (\_ _ _->base) prac base mapDict
        (_,_,_,[]) -> mapDict -- trace("!"++line) $ mapDict

showReadDictProg :: (Int,Int) -> IO ()
showReadDictProg (lc,pc) = do
    putStr (" > Read dictionary [ # word (loaded/parsed) " ++ (show lc) ++ " / " ++ (show pc) ++ " ]\r")


showReadDataProg :: (Int,Int,Int) -> IO ()
showReadDataProg (sc,pc,lc) = do
    putStr (" > Read database [ # page (saved/parsed) " ++ (show sc) ++ " / " ++ (show pc) ++ " / # line " ++ (show lc) ++ " ]\r")

getContentFromFile :: ArgsRec -> M.Map S S -> [S] -> IO ()
getContentFromFile args mapDict stopwords = do

    let (ArgsRec { inWikiFile = i }) = args

    s <- getCurrentTime
    hInWikiFile <- openFile i ReadMode
    encoding <- mkTextEncoding "UTF-8"
    hSetEncoding hInWikiFile encoding
    getLineFromFile hInWikiFile "" args mapDict stopwords (0,0,0)
    hClose hInWikiFile
    e <- getCurrentTime
    putStrLn $ " > Read database in " ++ (printf "%.2f" (realToFrac  (diffUTCTime e s)::Double)) ++ " sec."

getLineFromFile :: Handle -> S -> ArgsRec -> M.Map S S -> [S] -> (Int,Int,Int) -> IO ()
getLineFromFile hInWikiFile page args mapDict stopwords (sc,pc,lc)= do

    hInWikiFileEOF <- hIsEOF hInWikiFile
    if hInWikiFileEOF then do
        putStrLn ""
        return ()
    else do
        line <- hGetLine hInWikiFile
        let pageline = page++line

        -- FIXME: Currently regex-posix (Text.Regex.Lazy in sourceforge)
        --        could not successfuly match with text if it contains
        --        Japanese space whose character code in UTF8 is E38080.
        --        ASCII compatible characters in UTF8 is probably OK.
        --case pageline =~ "</page>" :: Bool of
        case search line "</page>" of
            True -> do
                len <- parsePage args mapDict stopwords pageline (sc,pc,lc+1)
                case len of
                    0 -> do
                        showReadDataProg (sc,pc+1,lc+1)
                        getLineFromFile hInWikiFile "" args mapDict stopwords (sc,pc+1,lc+1)
                    _ -> do
                        showReadDataProg (sc+1,pc+1,lc+1)
                        getLineFromFile hInWikiFile "" args mapDict stopwords (sc+1,pc+1,lc+1)
            False -> do
                showReadDataProg (sc,pc,lc+1)
                getLineFromFile hInWikiFile pageline args mapDict stopwords (sc,pc,lc+1)

parsePage :: ArgsRec -> M.Map S S -> [S] -> S -> (Int,Int,Int) -> IO (Int)
parsePage _ _ _ "" _ = return (0)
parsePage args mapDict stopwords page (sc,pc,lc) =
    let
        (ArgsRec { outBofwFile = s }) = args
    in
        case P.parse (myTextParser "text") "Error:" page of
            Right text -> do
                output <- return $ parseText args mapDict stopwords text
                unlessEmptyWriteToFile s output
                parsePageTitle args page output
                return $ length output
            Left err -> trace(show err ++ ": Nothing for "++ show page) $ exitFailure

parseText :: ArgsRec -> M.Map S S -> [S] -> S -> S
parseText args mapDict stopwords text =
    let
        (ArgsRec { numMinTermsInDoc = m }) = args
        (ArgsRec { numMaxTermsInDoc = x }) = args
        (ArgsRec { numMinFreqOfTerm = c }) = args
    in
        compMakeOutputText m x c mapDict stopwords text

parsePageTitle :: ArgsRec -> S -> S -> IO ()
parsePageTitle _ _ "" = return ()
parsePageTitle args page output =
    let
        (ArgsRec { outTitleFile = t }) = args
    in
        case P.parse (myTextParser "title") "Error:" page of
            Right title -> unlessEmptyWriteToFile t title
            Left err -> trace(show err ++ ": Nothing for "++ show page) $ exitFailure

unlessEmptyWriteToFile :: FilePath -> S -> IO ()
unlessEmptyWriteToFile _ "" = return ()
unlessEmptyWriteToFile "" bofw = putStrLn bofw
unlessEmptyWriteToFile outBofwFile bofw = appendFile outBofwFile $ bofw++"\n"

compMakeOutputText :: Int -> Int -> Int -> M.Map S S -> [S] -> ( S -> S )
compMakeOutputText m x c d s =
    joinWithSpaceAfterFlatten
    . filterByFreqTerms c
    . filterByNumTermsInDoc m x
    . makeTupleList
    . makeBagofwords
    . convertToBaseform d
    . excludeStopwords s
    . filterByRegex
    . makeCharLower
    . splitBySpace

joinWithSpaceAfterFlatten :: [[S]] -> S
joinWithSpaceAfterFlatten llt = unwords $ concat llt

filterByFreqTerms :: Int -> [(S,Int)] -> [[S]]
filterByFreqTerms lower ltp =
    map (\ (term,freq) ->
        if freq >= lower
        then [term,show freq]
        else []
    ) ltp

filterByNumTermsInDoc :: Int -> Int -> [(S,Int)] -> [(S,Int)]
filterByNumTermsInDoc lower upper ltp =
    if length ltp >= lower && length ltp <= upper
    then ltp
    else []

makeTupleList :: M.Map S Int -> [(S,Int)]
makeTupleList m = M.toList m

-- insertWithKey takes 4 arguments (in this case)
-- (key->initvalue->oldvalue->newvalue)->key->initvalue->ExistentingMap
-- (makeBagofwords xs) recursively returns back to M.empty for xs
-- M.insertWithKey then starts to run its job like foldr
makeBagofwords :: [S] -> M.Map S Int
makeBagofwords [] = M.empty
makeBagofwords (x:xs)=
    let m = makeBagofwords xs in
        M.insertWithKey (\_ _ o->o+1) x 1 m

convertToBaseform :: M.Map S S -> [S] -> [S]
convertToBaseform mapDict lst =
    map (\t -> getBaseform mapDict t) lst

excludeStopwords :: [S] -> [S] -> [S]
excludeStopwords stopwords lst =
    filter (\t -> not $ elem t stopwords) lst

filterByRegex :: [S] -> [S]
filterByRegex lst =
    filter (\t -> t =~ "^[a-z][0-9a-z'-]*[0-9a-z]$") lst

makeCharLower :: [S] -> [S]
makeCharLower lst =
    map (\t -> map toLower t) lst

splitBySpace :: S -> [S]
splitBySpace text = words text


getBaseform :: M.Map S S -> S -> S
getBaseform mapDict t =
    case M.lookup t mapDict of
        Just v -> v
        Nothing -> t


-- Regular expresion match can replace this
search :: S -> S -> Bool
search x y = doSearch x y

doSearch :: S -> S -> Bool
doSearch (x:xs) (y:ys)
    | x==y = case doSearchIn xs ys of
        True -> True
        False -> doSearch xs (y:ys)
    | otherwise = doSearch xs (y:ys)
doSearch _ _ = False

doSearchIn :: S -> S -> Bool
doSearchIn _ [] = True
doSearchIn [] _ = False
doSearchIn (x:xs) (y:ys)
    | x==y = doSearchIn xs ys
    | otherwise = False

myTextParser :: S -> P.Parsec S () S
myTextParser tag = do
    _ <- P.manyTill P.anyChar (P.try $ P.string ("<"++tag) )
    _ <- P.manyTill P.anyChar (P.char '>')
    P.manyTill P.anyChar (P.try $ P.string ("</"++tag++">") )

