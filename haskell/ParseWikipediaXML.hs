import System.Environment(getArgs)
import System.IO
import System.Exit
import qualified Data.Map as M
import Data.Char
--import Control.Applicative
import Text.Regex.Posix
import Debug.Trace
--import Codec.Binary.UTF8.String
--import Control.Exception as E

type S = String

main :: IO ()
main = do
	args <- getArgs
	mapArgs <- return $ parseArgs args $ defaultArgs "idstmxc"

	putStrLn "Begin reading dictionary."
	mapDict <- getDictionaryFromFile mapArgs
	-- Force strict evaluation for mapDict
	_mapDict <- return $! mapDict
	putStrLn "Finish reading dictionary."

	stopwords <- return getStopwords
	getContentFromFile mapArgs _mapDict stopwords

getStopwords :: [S]
getStopwords =
	let stopwords = "a,able,about,across,after,all,almost,also,am,among,an,and,any,are,as,at,be,because,been,but,by,can,cannot,could,dear,did,do,does,either,else,ever,every,for,from,get,got,had,has,have,he,her,hers,him,his,how,however,i,if,in,into,is,it,its,just,least,let,like,likely,may,me,might,most,must,my,neither,no,nor,not,of,off,often,on,only,or,other,our,own,rather,said,say,says,she,should,since,so,some,than,that,the,their,them,then,there,these,they,this,tis,to,too,twas,us,wants,was,we,were,what,when,where,which,while,who,whom,why,will,with,would,yet,you,your"
	in splitByComma stopwords "" []

splitByComma :: S -> S -> [S] -> [S]
splitByComma [] _ arr = arr
splitByComma (x:xs) s arr | x==',' = splitByComma xs "" (s:arr)
splitByComma (x:xs) s arr = splitByComma xs (s++[x]) arr

defaultArgs :: S -> M.Map S S
defaultArgs [] = M.empty
defaultArgs (x:xs) =
	let m = defaultArgs xs in
		case x of
			'i' -> M.insertWithKey (\_ _ o->o) "i" "" m
			'd' -> M.insertWithKey (\_ _ o->o) "d" "" m
			's' -> M.insertWithKey (\_ _ o->o) "s" "" m
			't' -> M.insertWithKey (\_ _ o->o) "t" "" m
			-- Integer literal cannot be identified as integer without declaration
			-- This may be better instead of getting it back to string from integer
			--'m' -> M.insertWithKey (\_ _ o->o) "m" "1") m
			'm' -> M.insertWithKey (\_ _ o->o) "m" (show (1::Int)) m
			'x' -> M.insertWithKey (\_ _ o->o) "x" (show (65535::Int)) m
			'c' -> M.insertWithKey (\_ _ o->o) "c" (show (1::Int)) m
			_ -> m

parseArgs :: [S] -> M.Map S S -> M.Map S S
parseArgs [] mapArgs = mapArgs
parseArgs (('-':option):value:args) mapArgs =
	let m = parseArgs args mapArgs in
		case option of
			"i" -> M.insertWithKey (\_ _ _ -> value) "i" value $
					( M.insertWithKey (\_ _ _ -> value) "inWikiFile" value m )
			"d" -> M.insertWithKey (\_ _ _ -> value) "d" value $
					( M.insertWithKey (\_ _ _ -> value) "inDictFile" value m )
			"s" -> M.insertWithKey (\_ _ _ -> value) "s" value $
					( M.insertWithKey (\_ _ _ -> value) "outBofwFile" value m )
			"t" -> M.insertWithKey (\_ _ _ -> value) "t" value $
					( M.insertWithKey (\_ _ _ -> value) "outTitleFile" value m )
			"m" -> M.insertWithKey (\_ _ _ -> value) "m" value $
					( M.insertWithKey (\_ _ _ -> value) "numMinTermsInDoc" value m )
			"x" -> M.insertWithKey (\_ _ _ -> value) "x" value $
					( M.insertWithKey (\_ _ _ -> value) "numMaxTermsInDoc" value m )
			"c" -> M.insertWithKey (\_ _ _ -> value) "c" value $
					( M.insertWithKey (\_ _ _ -> value) "numMinFreqOfTerm" value m )
			_ -> m
parseArgs _ mapArgs = mapArgs

getDictionaryFromFile :: M.Map S S -> IO (M.Map S S)
getDictionaryFromFile mapArgs = do

	let inDictFile = tk mapArgs "d"
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


getContentFromFile :: M.Map S S -> M.Map S S -> [S] -> IO ()
getContentFromFile mapArgs mapDict stopwords = do

	let inWikiFile = tk mapArgs "i"
	hInWikiFile <- openFile inWikiFile ReadMode
	encoding <- mkTextEncoding "UTF-8"
	hSetEncoding hInWikiFile encoding
	_ <- getLineFromFile hInWikiFile "" mapArgs mapDict stopwords
	hClose hInWikiFile

getLineFromFile :: Handle -> S -> M.Map S S -> M.Map S S -> [S] -> IO ()
getLineFromFile hInWikiFile page mapArgs mapDict stopwords = do

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

			True ->
				--case pageline =~ "<text.*>([^<>]*)</text>" :: (S,S,S,[S]) of
				case matchText pageline ("<text",'>',"</text>") of
					--(_,_,_,(text:_)) ->
					Just text ->
						-- Write to file or stdout
						notEmptyWriteToFile (tk mapArgs "s") $
						-- Make list to string joined with space
						unwords $
						-- Make list's list to list flattened
						concat $
						-- Note that concat infers that y is String but Int actually
						-- Make tuple's list to list's list with conditions
						map (\ (x,y) ->
								if y >= (read $ tk mapArgs "c")
								then [x,show y]
								else []
							) $
						-- Apply condition for the number of terms in a doc
						(\ x ->
							--trace("trace2:" ++ show x) $
							if length x >= (read $ tk mapArgs "m") &&
								length x <= (read $ tk mapArgs "x")
							then x
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
	
					Nothing -> trace("trace: Nothing.") $ exitFailure
	
			False ->
				getLineFromFile hInWikiFile pageline mapArgs mapDict stopwords
	
		getLineFromFile hInWikiFile "" mapArgs mapDict stopwords

--ignore :: SomeException -> IO Bool
--ignore _ = trace("SomeException.") return False

-- Regular expresion match can replace this
search :: S -> S -> Bool
search x y = doSearch x (y, y)

doSearch :: S -> (S,S) -> Bool
doSearch _ ([],_) = True
doSearch [] _ = False
doSearch (x:xs) ((y:ys),_y)
	| x == y = doSearch xs (ys,_y)
	| otherwise = doSearch xs (_y,_y)

matchText :: S -> (S,Char,S) -> Maybe S
matchText text (begin,mid,end) = doMatch text (begin,begin) mid (end,end) []

doMatch :: S -> (S,S) -> Char -> (S,S) -> S -> Maybe S
doMatch _ _ _ ([],_end) cont
	| cont == [] = Nothing
	| otherwise = Just cont
doMatch [] _ _ _ cont
	| cont == [] = Nothing
	| otherwise = Just cont
doMatch (t:text) ([],_begin) ' ' ((e:end),_end) cont
	| t==e = doMatch text ([],_begin) ' ' (end,_end) cont
	| otherwise = doMatch text ([],_begin) ' ' (_end,_end) (cont++[t])
doMatch (t:text) ([],_begin) m end []
	| t==m = doMatch text ([],_begin) ' ' end []
	| otherwise = doMatch text ([],_begin) m end []
doMatch (t:text) ((b:begin),_begin) m end []
	| t==b = doMatch text (begin,_begin) m end []
	| otherwise = doMatch text (_begin,_begin) m end []
doMatch _ _ _ _ _ = Nothing

getBaseformFromDict :: M.Map S S -> S -> S
getBaseformFromDict mapDict term =
	case M.lookup term mapDict of
		Just t -> t
		Nothing -> term

-- The shorter function name is better for frequent use
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
-- (key->initvalue->oldvalue->newvalue) key initvalue ExistentingMap
-- (makeBagofwords xs) recursively returns back to M.empty for xs
-- M.insertWithKey then starts to run its job like foldr
makeBagofwords :: [S] -> M.Map S Int
makeBagofwords [] = M.empty
makeBagofwords (x:xs)=
	let m = makeBagofwords xs in
		M.insertWithKey (\_ _ o->o+1) x 1 m

