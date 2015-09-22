import System.Environment(getArgs)
import System.IO
import System.Exit
import qualified Data.Map as M
import Data.Char
--import Control.Applicative
import Text.Regex.Posix

type S = String

main :: IO ()
main = do
	args <- getArgs
	mapArgs <- return $ parseArgsAbbr args $ parseArgs args

	putStrLn "Begin reading dictionary."
	mapDict <- getDictionaryFromFile mapArgs
	-- Force strict evaluation for mapDict
	_mapDict <- return $! mapDict
	putStrLn "Finish reading dictionary."

	stopwords <- return getStopwords
	getContentFromFile mapArgs _mapDict stopwords

getStopwords :: [String]
getStopwords =
	let stopwords = "a,able,about,across,after,all,almost,also,am,among,an,and,any,are,as,at,be,because,been,but,by,can,cannot,could,dear,did,do,does,either,else,ever,every,for,from,get,got,had,has,have,he,her,hers,him,his,how,however,i,if,in,into,is,it,its,just,least,let,like,likely,may,me,might,most,must,my,neither,no,nor,not,of,off,often,on,only,or,other,our,own,rather,said,say,says,she,should,since,so,some,than,that,the,their,them,then,there,these,they,this,tis,to,too,twas,us,wants,was,we,were,what,when,where,which,while,who,whom,why,will,with,would,yet,you,your"
	in splitByComma stopwords "" []

splitByComma :: String -> String -> [String] -> [String]
splitByComma [] _ arr = arr
splitByComma (x:xs) s arr | x==',' = splitByComma xs "" (s:arr)
splitByComma (x:xs) s arr = splitByComma xs (s++[x]) arr

parseArgsAbbr :: [String] -> M.Map S S -> M.Map S S
parseArgsAbbr [] mapArgs = mapArgs
parseArgsAbbr (('-':option):value:args) mapArgs =
	let m = parseArgsAbbr args mapArgs in
		case option of
			"i" -> M.insertWithKey (\_ _ _ -> value) "i" value m
			"d" -> M.insertWithKey (\_ _ _ -> value) "d" value m
			"s" -> M.insertWithKey (\_ _ _ -> value) "s" value m
			"t" -> M.insertWithKey (\_ _ _ -> value) "t" value m
			"m" -> M.insertWithKey (\_ _ _ -> value) "m" value m
			"x" -> M.insertWithKey (\_ _ _ -> value) "x" value m
			"c" -> M.insertWithKey (\_ _ _ -> value) "c" value m
			_ -> m
parseArgsAbbr _ mapArgs = mapArgs

parseArgs :: [String] -> M.Map S S
parseArgs [] = M.empty
parseArgs (('-':option):value:args) =
	let m = parseArgs args in
		case option of
			"i" -> M.insertWithKey (\_ _ _ -> value) "inWikiFile" value m
			"d" -> M.insertWithKey (\_ _ _ -> value) "inDictFile" value m
			"s" -> M.insertWithKey (\_ _ _ -> value) "outBofwFile" value m
			"t" -> M.insertWithKey (\_ _ _ -> value) "outTitleFile" value m
			"m" -> M.insertWithKey (\_ _ _ -> value) "numMinTermsInDoc" value m
			"x" -> M.insertWithKey (\_ _ _ -> value) "numMaxTermsInDoc" value m
			"c" -> M.insertWithKey (\_ _ _ -> value) "numMinWordOfTerm" value m
			_ -> m
parseArgs _ = M.empty

getDictionaryFromFile :: M.Map S S -> IO (M.Map S S)
getDictionaryFromFile mapArgs = do

	let inDictFile = case M.lookup "d" mapArgs of
		Just i -> i
		Nothing -> "../share/morph_english.flat"

	hInDictFile <- openFile inDictFile ReadMode
	getLineFromInDictFile hInDictFile M.empty

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


getContentFromFile :: M.Map S S -> M.Map S S -> [String] -> IO () 
getContentFromFile mapArgs mapDict stopwords = do

	let inWikiFile = case M.lookup "i" mapArgs of
		Just i -> i
		Nothing -> "../share/enwiki-test-5000"

	hInWikiFile <- openFile inWikiFile ReadMode
	--encoding <- mkTextEncoding "65001"
	--hSetEncoding hInWikiFile encoding
	_ <- getLineFromFile hInWikiFile "" mapArgs mapDict stopwords
	hClose hInWikiFile

getLineFromFile :: Handle -> S -> M.Map S S -> M.Map S S -> [String] -> IO ()
getLineFromFile hInWikiFile page mapArgs mapDict stopwords = do

	hInWikiFileEOF <- hIsEOF hInWikiFile
	if hInWikiFileEOF then return ()
	else do
		let outBofwFile = case M.lookup "s" mapArgs of
			Just s -> s
			Nothing -> "bofw.txt"
	
		line <- hGetLine hInWikiFile
		let pageline = page++line
	
		--case search pageline "</page>" of
		case pageline =~ ".*</page>" :: Bool of
			True ->
				case pageline =~ "<text.*>([^<>]*)</text>" :: (S,S,S,[S]) of
	
					(_,_,_,(text:_)) ->
						-- Write to file or stdout
						notEmptyWriteToFile outBofwFile $
						-- Make list to string joined with space
						unwords $
						-- Make list's list to list flattened
						concat $
						-- Note that concat infers that y is String but Int
						-- Make tuple's list to list's list
						map (\(x,y)->[x,show y]) $
						-- Make map to tuple's list
						M.toList $
						-- Make bag-of-words by counting each term's frequency
						makeBagofwords $
						-- Get baseform from dictionary for each string(term)
						map (\x -> getBaseformFromDict mapDict x) $
						-- Exclude stopwords
						filter (\x -> not $ elem x stopwords) $
						-- Lower all characters in each string
						map (\x -> map toLower x) $
						-- Filter terms out by this regular expression
						filter (\x -> x =~ "^[a-z][0-9a-z'-]*[0-9a-z]$") $
						-- Split text by space
						words text
	
					_ -> exitFailure
	
			False -> getLineFromFile hInWikiFile pageline mapArgs mapDict stopwords
	
		getLineFromFile hInWikiFile "" mapArgs mapDict stopwords

-- Regular expresion match can replace this
-- search [] _ = False
-- search _ [] = True
-- search (x:xs) (y:ys)
-- 	| x == y = search xs ys
-- 	| otherwise = search xs (y:ys)

getBaseformFromDict :: M.Map S S -> S -> S
getBaseformFromDict mapDict term =
	case M.lookup term mapDict of
		Just t -> t
		Nothing -> term

notEmptyWriteToFile :: FilePath -> String -> IO ()
notEmptyWriteToFile _ "" = return ()
notEmptyWriteToFile "" bofw = putStrLn bofw
notEmptyWriteToFile outBofwFile bofw = appendFile outBofwFile $ bofw++"\n"

-- insertWithKey takes 4 arguments (in this case)
-- (key->initvalue->oldvalue->newvalue) key initvalue ExistentingMap
-- (makeBagofwords xs) recursively returns back to M.empty for xs
-- M.insertWithKey then starts to run its job like foldr
makeBagofwords :: [S] -> M.Map S Int
makeBagofwords [] = M.empty
makeBagofwords (x:xs)=
	let m = makeBagofwords xs in 
		M.insertWithKey (\_ _ o->o+1) x 1 m

