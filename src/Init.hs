module Init where
import Output  
import System.Random
import HelpFunctions  

-- |  liefert eine Zufallszahl, die zwischen 0 und der Anzahl der Woerter in der Woerterliste liegt, zurueck     
randomNumberIO :: Int -> IO Int
randomNumberIO wordcount= do zahl <- randomRIO(0,wordcount)
                             return zahl
      
-- | liefert das durch die Zufallszahl ausgewaehlte Wort aus der Woerterliste zurueck
getWord :: (Num a, Monad m) => [a1] -> a -> a -> m a1
getWord (x:xs) init end= if init /= end
                            then getWord xs (init+1) end
                            else return(x)
 
-- | gibt ein zufaellig aus der Woerterliste ausgewaehltes Wort zurueck  
initGame :: IO String                 
initGame=do
          words <- readFile "Woerter"

          let wordlist = lines words
     
          let wordcount= (length wordlist)-1

          randomnumber<-randomNumberIO wordcount

          answer <- getWord wordlist 0 randomnumber
          
          return answer
    
      
                                   
