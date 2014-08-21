import Control.Applicative
import Data.List.Split
import Control.Monad
import Debug.Trace

data Gender = Male | Female
 deriving (Eq,Show)
 
main = do
    r <- getFile "t4"
    writeFile "t4.out" (unlines $ fmap action r)
    
getFile :: String -> IO [String]
getFile fp = (tail . lines) <$> readFile fp

action :: String -> String
action text = show $ parse
 where 
    parse = answer $ read <$> splitOn " " text  
    
answer :: [Int] -> Gender
answer (n:k:_) = kthChildNthGeneration1 n k

kthChildNthGeneration1 n k = trace ("kthChildNthGeneration " ++ show n ++ " " ++ show k) $ go
    where 
        go = case (lookupMorse (k-1)) of
            0 -> Male
            1 -> Female
                                                   
                                                   
--  The generation doesn't matter as teh series is the same for each generation, so k and ignore n
lookupMorse :: Int -> Int
lookupMorse 0 = 0;
lookupMorse n | even n    =     lookupMorse (div  n    2)
              | otherwise = 1 - lookupMorse (div (n-1) 2)
  

--------------------------
-------------------------- First Draft Brute force code below, was to slow for large results found in t4 test case
-------------------------  

kthChildNthGeneration :: Int -> Int -> Gender
kthChildNthGeneration n k = trace ("kthChildNthGeneration " ++ show n ++ " " ++ show k) $ generation n !! (k-1)


generation :: Int -> [Gender] 
generation 1 = [Male]
generation x = apply $ generation (x - 1)

apply :: [Gender] -> [Gender]
apply xs = join $ fmap childern xs 

childern :: Gender -> [Gender]
childern Male = [Male,Female]
childern Female = [Female, Male]
