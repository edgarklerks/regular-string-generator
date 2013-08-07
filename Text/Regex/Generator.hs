{-# LANGUAGE RankNTypes,Arrows,BangPatterns #-}
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

newtype RandomArrow a b = RandomArrow {
		runRandomArrow :: Seed -> a -> (b,Seed)
	}	

instance Category RandomArrow where 
	id = RandomArrow (\s a -> (a, s))
	(.) (RandomArrow f) (RandomArrow g) = RandomArrow (\s a -> let (b, s') = g s a 
								   in f s' b)
instance Arrow RandomArrow where 
	arr f = RandomArrow (\s a -> (f a, s))
	first (RandomArrow f) = RandomArrow (\s (a,b) -> 
				let (c, s') = f s a 
			  	in ((c,b), s')
			)
	second (RandomArrow f) = RandomArrow (\s (a,b) ->
				let (c, s') = f s b 
				in ((a,c),s'))
instance ArrowChoice RandomArrow where 
	left (RandomArrow f) = RandomArrow $ \s a -> case a of 
							Left a -> let (b, s') = f s a	
								  in (Left b, s')
							Right b -> (Right b, s)


state :: (a -> Seed -> (b, Seed)) -> RandomArrow a b 
state f = RandomArrow (\s a -> f a s)  

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


generate :: RandomArrow RegExp String 
generate = proc (RegExp xs) -> do 
		ys <- id -<  xs 
		flatMapList genOne -< ys   

flatMapList :: Show b => Monoid b => RandomArrow a b -> RandomArrow [a] b  
flatMapList f = proc xs -> do
			case xs of 
			  [] -> returnA -< mempty 
			  (x:xs) ->  do  
				p <- flatMapList f -< xs
				n <- f -< x  
				returnA -<  mappend n p 

genOne :: RandomArrow RegExpD String   
genOne  = proc x -> do 
	case x of 
	  Character x -> arr (\x -> [x]) -< x 
	  Range xs Nothing -> do 
			p <- choose -< xs
			range -< p  
	  Range xs (Just a) -> do 
			 b <- toNum -< a 
			 repeatRandom rangeOne -< (xs,b) 
	  Group r Nothing -> do 
			x <- choose -< r  
			generate -< x
	  Group r (Just a) ->  do 
		x <- choose -< r 
		b <- toNum -< a
		repeatRandom  generate -< (x,b)  
	where rangeOne = proc xs -> do  
			p <- choose -< xs 
			range -< p 
	  
	  
toNum :: RandomArrow RegExpD Int 
toNum = proc x -> do 
		case x of 
		  Star -> arr abs <<< state randomR -< (1,50)   
		  StarPlus -> arr ((+1).abs) <<< state randomR  -< (1,50) 
		  From x -> state randomR -< (x,50)
		  Till x  -> state randomR -< (0, x)
		  FromTill x -> state randomR -< x 
	 		
repeatRandom :: RandomArrow a String -> RandomArrow (a,Int) String
repeatRandom  f = proc (x,b) -> do 
			case b of 
			   0 -> returnA -< []
			   n -> do  
				rest <- repeatRandom f -< (x, n - 1)
				p <- f -< x  	
				returnA -<  (p ++ rest) 

choose :: RandomArrow [a] a 
choose = proc xs -> do 
		p <- state (\xs -> randomR (0, length xs - 1)) -< xs
		returnA -< xs !! p
				

range :: RandomArrow (RegExpD,RegExpD) String 
range = proc x -> do 
	  case x of 	
	   (Character a, Character b) -> arr (\x -> [x]) <<< state randomR -< (a,b)
	  
		



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
