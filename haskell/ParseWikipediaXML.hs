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

type S = String

data ArgsRec = ArgsRec {
	inWikiFile :: String,
	inDictFile :: String,
	outBofwFile :: String,
	outTitleFile :: String,
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
		numMinTermsInDoc = 1,
		numMaxTermsInDoc = 65535,
		numMinFreqOfTerm = 1
	}

	shortargs <- return $ parseShortArgs args init
	args <- return $ parseLongArgs args shortargs

	putStrLn "Begin reading dictionary."
	mapDict <- getDictionaryFromFile args
	-- Force strict evaluation for mapDict
	_mapDict <- return $! mapDict
	putStrLn "Finish reading dictionary."

	stopwords <- return getStopwords
	getContentFromFile args _mapDict stopwords

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
	getLineFromInDictFile hdlr M.empty

getLineFromInDictFile :: Handle -> M.Map S S -> IO (M.Map S S)
getLineFromInDictFile hInDictFile mapDict = do
	hInDictFileEOF <- hIsEOF hInDictFile
	if hInDictFileEOF then return mapDict
	else do
		line <- hGetLine hInDictFile
		let newMapDict = case line =~ "^([\\S]*)[\\s]*([\\S]*)\\s" :: (S,S,S,[S]) of
			(_,_,_,(prac:base:_)) -> M.insertWithKey (\_ _ _->base) prac base mapDict
			_ -> mapDict
		getLineFromInDictFile hInDictFile newMapDict


getContentFromFile :: ArgsRec -> M.Map S S -> [S] -> IO ()
getContentFromFile args mapDict stopwords = do

	let (ArgsRec { inWikiFile = i }) = args
	hInWikiFile <- openFile i ReadMode
	encoding <- mkTextEncoding "UTF-8"
	hSetEncoding hInWikiFile encoding
	_ <- getLineFromFile hInWikiFile "" args mapDict stopwords
	hClose hInWikiFile

getLineFromFile :: Handle -> S -> ArgsRec -> M.Map S S -> [S] -> IO ()
getLineFromFile hInWikiFile page args mapDict stopwords = do

	hInWikiFileEOF <- hIsEOF hInWikiFile
	if hInWikiFileEOF then return ()
	else do
		line <- hGetLine hInWikiFile
		let pageline = page++line
	
		-- FIXME: Currently regex-posix (Text.Regex.Lazy in sourceforge)
		--        could not successfuly match with text if it contains
		--        Japanese space whose character code in UTF8 is E38080.
		--        ASCII compatible characters in UTF8 is probably OK.
		--case pageline =~ "</page>" :: Bool of
		case search line "</page>" of
			True -> parsePage args mapDict stopwords pageline
			False -> getLineFromFile hInWikiFile pageline args mapDict stopwords
		getLineFromFile hInWikiFile "" args mapDict stopwords

parsePage :: ArgsRec -> M.Map S S -> [S] -> S -> IO ()
parsePage args mapDict stopwords page =
	let
		(ArgsRec { outBofwFile = s }) = args
		(ArgsRec { numMinTermsInDoc = m }) = args
		(ArgsRec { numMaxTermsInDoc = x }) = args
		(ArgsRec { numMinFreqOfTerm = c }) = args
	in
		case P.parse myTextParser "Error:" page of
			Right text ->
				unlessEmptyWriteToFile s $
				compMakeOutputText m x c mapDict stopwords text

			Left err ->
				trace(show err ++ ": Nothing for "++ show page) $
				exitFailure

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

myTextParser :: P.Parsec S () S
myTextParser = do
	_ <- P.manyTill P.anyChar (P.try $ P.string "<text")
	_ <- P.manyTill P.anyChar (P.char '>')
	P.manyTill P.anyChar (P.try $ P.string "</text>")


