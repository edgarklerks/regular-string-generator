{-# LANGUAGE Arrows #-}
module Control.Arrow.Random where  

import Control.Category 
import Control.Arrow 
import System.Random 
import Prelude hiding ((.), id)


newtype RandomArrow g a b = RandomArrow {
		runRandomArrow :: g -> a -> (b,g)
	}	

instance Category (RandomArrow g) where 
	id = RandomArrow (\s a -> (a, s))
	(.) (RandomArrow f) (RandomArrow g) = RandomArrow (\s a -> let (b, s') = g s a 
								   in f s' b)
instance Arrow (RandomArrow g) where 
	arr f = RandomArrow (\s a -> (f a, s))
	first (RandomArrow f) = RandomArrow (\s (a,b) -> 
				let (c, s') = f s a 
			  	in ((c,b), s')
			)
	second (RandomArrow f) = RandomArrow (\s (a,b) ->
				let (c, s') = f s b 
				in ((a,c),s'))
instance ArrowChoice (RandomArrow g) where 
	left (RandomArrow f) = RandomArrow $ \s a -> case a of 
							Left a -> let (b, s') = f s a	
								  in (Left b, s')
							Right b -> (Right b, s)


state :: (a -> g -> (b, g)) -> RandomArrow g a b 
state f = RandomArrow (\s a -> f a s)  


choose :: RandomGen g => RandomArrow g [a] a 
choose = proc xs -> do 
		p <- state (\xs -> randomR (0, length xs - 1)) -< xs
		returnA -< xs !! p
				


range :: (RandomGen g, Enum a, Random a) => RandomArrow g (a,a) a
range = proc x -> do 
	  case x of 	
	   (a, b) -> arr (\x -> x) <<< state randomR -< (a,b)
	  
repeatRandom :: RandomArrow g a b -> RandomArrow g (a,Int) [b]
repeatRandom  f = proc (x,b) -> do 
			case b of 
			   0 -> returnA -< []
			   n -> do  
				rest <- repeatRandom f -< (x, n - 1)
				p <- f -< x  	
				returnA -<  (p : rest) 

repeatFold :: (b -> m -> m) -> m -> RandomArrow g a b -> RandomArrow g (a, Int) m 
repeatFold  fl e f = proc x -> do 
		xs <- repeatRandom f -< x 
		returnA -< foldr fl e xs 


