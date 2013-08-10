{-# LANGUAGE RankNTypes,Arrows,BangPatterns, NoMonomorphismRestriction #-}
module Text.Regex.Generate (
	generateRegulars,
	Seed(..) 
) where 

import Data.Monoid
import Control.Monad
import Control.Applicative 
import Text.Parsec hiding ((<|>), many, State) 
import Text.Parsec.String
import Text.Parsec.Prim hiding ((<|>),many, State)
import Text.Parsec.Combinator 
import System.Random 
import Control.Category 
import Control.Arrow 
import Control.Arrow.Random 
import Prelude hiding ((.), id)
import Debug.Trace
-- | main api
--

testHTTP = do 
	s <- newStdGen 
	return $ generateRegulars s "(http|https)://sys.AAA.org/[a-z0-9]{1,4}/BBB"

data RegExpD = Character Char 
	    | Range [(RegExpD, RegExpD)] (Maybe RegExpD)
	    | Star 
	    | StarPlus
	    | One  
	    | From Int  	
	    | Till Int 
	    | FromTill (Int,Int)  
	    | Group [RegExp] (Maybe RegExpD) 
	deriving Show 
type Seed = StdGen 
newtype RegExp = RegExp {
	unRegExp :: [RegExpD]
	} 

instance Show RegExp where 
	show (RegExp xs) = showRegExpD =<< xs 
-- |  Generate string, which match the regular expression
--
--

generateRegulars :: Seed -> String -> [String]
generateRegulars xs s = do 
	case parseRegularExpression s of
		Left e -> error (show e)
		Right a -> fst $ runRandomArrow (foreverA generate) xs a 
--
foreverA f = RandomArrow $ \s a ->    let (b,s') = runRandomArrow f s a 
					  (xs, s'') = runRandomArrow (foreverA f) s' a 
				      in (b : xs,s'') 


generate :: RandomGen g => RandomArrow g RegExp String 
generate = proc (RegExp xs) -> do 
		ys <- id -<  xs 
		flatMapList genOne -< ys   

flatMapList :: RandomGen g => Show b => Monoid b => RandomArrow g a b -> RandomArrow g [a] b  
flatMapList f = proc xs -> do
			case xs of 
			  [] -> returnA -< mempty 
			  (x:xs) ->  do  
				p <- flatMapList f -< xs
				n <- f -< x  
				returnA -<  mappend n p 

genOne :: RandomGen g => RandomArrow g RegExpD String   
genOne  = proc x -> do 
	case x of 
	  Character x -> arr (\x -> [x]) -< x 
	  Range xs Nothing -> do 
			p <- choose -< xs
			arr return <<< rangeRegexp -< p
	  Range xs (Just a) -> do 
			 b <- toNum -< a 
			 repeatFold (:) "" rangeOne -< (xs,b) 
	  Group r Nothing -> do 
			x <- choose -< r  
			generate -< x
	  Group r (Just a) ->  do 
		x <- choose -< r 
		b <- toNum -< a
		repeatFold (++) "" generate -< (x,b)  
	where rangeOne = proc xs -> do  
			p <- choose -< xs 
			rangeRegexp -< p 


rangeRegexp :: RandomGen g => RandomArrow g (RegExpD, RegExpD) Char 
rangeRegexp = proc (Character x, Character y) -> do 
	xs <- range -< (x,y)
	returnA -< xs 

toNum :: RandomGen g => RandomArrow g RegExpD Int 
toNum = proc x -> do 
		case x of 
		  Star -> arr abs <<< state randomR -< (1,50)   
		  StarPlus -> arr ((+1).abs) <<< state randomR  -< (1,50) 
		  From x -> state randomR -< (x,50)
		  Till x  -> state randomR -< (0, x)
		  FromTill x -> state randomR -< x 


{--	
instance Show RegExpD where 
	show = showRegExpD
--}
-- | Parse a regular expression

parseRegularExpression :: String -> Either ParseError RegExp 
parseRegularExpression s = parse parseReg "" s


parseReg :: Parser RegExp 
parseReg =  RegExp <$> many1 parseRegExpD 

parseRegExpD :: Parser RegExpD
parseRegExpD = try parseGroup <|> try parseRange  <|> parseChar  

parseInt' :: Parser Int 
parseInt' = read <$> (many1 $ oneOf "1234567890") 

-- a-zA-Z0-9
parseChar :: Parser RegExpD 
parseChar = try parseEscapedChar <|> parseNormalChar 

parseEscapedChar :: Parser RegExpD 
parseEscapedChar = char '\\' *> (Character <$> oneOf "[]-(){}|")

parseNormalChar :: Parser RegExpD 
parseNormalChar = Character <$> oneOf "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLM NOPQRSTUVWXYZ!\"#$%&'*+,./:;<=>?@\\^_`~ \t"

parseRange :: Parser RegExpD 
parseRange = char '[' *> step 
	where step =  
		do 
		   x <- (many twice) 
		   char ']'
		   m <- optionMaybe parseModifier 
		   case x of 	
			xs -> return $ Range (xs) m 
twice :: Parser (RegExpD, RegExpD) 
twice = (,) <$> parseChar <*> (char '-' *> parseChar)
groupTwo :: [a] -> [(a,a)]	
groupTwo (x:y:xs) = (x,y) : groupTwo xs 
groupTwo (x:ys) = [] 
groupTwo _ = [] 

parseModifier :: Parser RegExpD 
parseModifier = try (char '*' *> pure Star) <|> try (char '+'*> pure StarPlus) <|> try (char '?' *> pure One) <|>  try to <|>  try from <|> fromto 
	where fromto =  char '{' *> 
		(FromTill <$> ((,) <$> (parseInt' <* char ',')  <*> (parseInt'))) <* char '}' 
	      from = char '{' *> (From <$> (parseInt' <* char ',')) <* char '}'
	      to = char '{' *> char ',' *> (char ',' *> (Till <$> parseInt')) <* char '}'
			
			

parseGroup :: Parser RegExpD 
parseGroup  = do 
			xs <- between (char '(') (char ')') $ parseReg `sepBy` (char '|')
			p <- optionMaybe parseModifier 
			return $ Group xs p    

-- | Show a regular expression in a normal form 
showRegExpD :: RegExpD -> String
showRegExpD (Character c) = [c] 
showRegExpD (Range xs n) = "[" <> foldr (\(a,b) z -> show a <> "-" <> show b <> z) [] xs <> "]" <> showMaybe n (showRegExpD) 
showRegExpD (Star) = "*"
showRegExpD (StarPlus) = "+"
showRegExpD (From n) = "{" <> show n <> ",}"
showRegExpD (Till n) = "{," <> show n <> "}"
showRegExpD (FromTill (p,q)) = "{" <> show p <> "," <> show q <> "}"
showRegExpD (Group bs p ) = "(" <> (show bs) <> ")" <> showMaybe p (showRegExpD)
showRegExpD (One) = "?"


-- | Helper 
showMaybe ::  Maybe a -> (a -> String) -> String 
showMaybe Nothing f = mempty 
showMaybe (Just a) f = f a 
