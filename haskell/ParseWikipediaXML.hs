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

-- getDictionaryFromFile :: M.Map S S -> IO (M.Map S S)
getDictionaryFromFile :: ArgsRec -> IO (M.Map S S)
getDictionaryFromFile args = do

	--let inDictFile = tk mapArgs "d"
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
				-- Write to file or stdout
				notEmptyWriteToFile s $
				-- Make list to string joined with space
				unwords $
				-- Make list's list to list flattened
				concat $
				-- Note that concat infers that y is String but Int actually
				-- Make tuple's list to list's list with conditions
				-- Apply condition for the number of a term in a doc
				map (\ (x,y) ->
						if y >= c
						then [x,show y]
						else []
					) $
				-- Apply condition for the number of (any) terms in a doc
				(\ term ->
					--trace("trace2:" ++ show x) $
					if length term >= m && length term <= x
					then term
					else []
				) $
				-- Make map to tuple's list
				M.toList $
				-- Make bag-of-words by counting each term's frequency
				makeBagofwords $
				-- Get baseform from dictionary for each string(term)
				map (\x -> getBaseformFromDict mapDict x) $
				-- Exclude stopwords
				filter (\x -> not $ elem x stopwords) $
				-- Filter terms out by this regular expression
				filter (\x -> x =~ "^[a-z][0-9a-z'-]*[0-9a-z]$") $
				-- Lower all characters in each string
				map (\x ->
					--trace("trace1:" ++ show x) $
					map toLower x) $
				-- Split text by space
				words text

			Left err ->
				--trace(err ++ ": Nothing for "++ show page) $
				trace(show err ++ ": Nothing for "++ show page) $
				exitFailure

--ignore :: SomeException -> IO Bool
--ignore _ = trace("SomeException.") return False

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

--{-
myTextParser :: P.Parsec S () S
myTextParser = do
	_ <- P.manyTill P.anyChar (P.try $ P.string "<text")
	_ <- P.manyTill P.anyChar (P.char '>')
	P.manyTill P.anyChar (P.try $ P.string "</text>")
---}

{-
matchText :: S -> (S,Char,S) -> Either S S
matchText text (begin,mid,end) = doMatch text begin mid end []

doMatch :: S -> S -> Char -> S -> S -> Either S S
doMatch _ _ _ [] cont = Right (reverse cont)
doMatch [] _ _ _ _ = Left "Did not match."
doMatch (t:text) [] ' ' (e:end) cont
	| t==e = case doMatchIn text end of
		True -> Right (reverse cont)
		False -> doMatch text [] ' ' (e:end) (t:cont) -- Reversed order
	| otherwise = doMatch text [] ' ' (e:end) (t:cont) -- Reversed order
doMatch (t:text) [] m end []
	| t==m = doMatch text [] ' ' end []
	| otherwise = doMatch text [] m end []
doMatch (t:text) (b:begin) m end []
	| t==b = case doMatchIn text begin of
		True -> doMatch (drop (length (b:begin)) text) [] m end []
		False -> doMatch text (b:begin) m end []
	| otherwise = doMatch text (b:begin) m end []
doMatch _ _ _ _ _ = Left "Something wrong."

doMatchIn :: S -> S -> Bool
doMatchIn _ [] = True
doMatchIn [] _ = False
doMatchIn (t:text) (b:begin)
	| t==b = doMatchIn text begin
	| otherwise = False
-}

getBaseformFromDict :: M.Map S S -> S -> S
getBaseformFromDict mapDict term =
	case M.lookup term mapDict of
		Just t -> t
		Nothing -> term

-- The shorter function name is better for frequent use
-- Take Key from map
tk :: M.Map S S -> S -> S
tk m s =
	case M.lookup s m of
		Just v -> v
		Nothing -> ""

notEmptyWriteToFile :: FilePath -> S -> IO ()
notEmptyWriteToFile _ "" = return ()
notEmptyWriteToFile "" bofw = putStrLn bofw
notEmptyWriteToFile outBofwFile bofw = appendFile outBofwFile $ bofw++"\n"

-- insertWithKey takes 4 arguments (in this case)
-- (key->initvalue->oldvalue->newvalue)->key->initvalue->ExistentingMap
-- (makeBagofwords xs) recursively returns back to M.empty for xs
-- M.insertWithKey then starts to run its job like foldr
makeBagofwords :: [S] -> M.Map S Int
makeBagofwords [] = M.empty
makeBagofwords (x:xs)=
	let m = makeBagofwords xs in
		M.insertWithKey (\_ _ o->o+1) x 1 m

