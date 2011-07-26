module Output where
import Ascii
import HelpFunctions

-- | Ausgabefunktion des aktuellen Spielstandes  
showOutput :: (Show a, Num a1) => [Char] -> a1 -> a -> IO ()
showOutput str mistakes usedletters=do
                                       resetScreen
                                       putStr "\n"
                                       print $insertSpace str
                                       putStr "\n"
                                       putStr "Verwendete Buchstaben: "
                                       print usedletters
                                       putStr "\n"
                                       paintHangman mistakes
                                       putStr "\n" 
                                       
-- | fuegt zwischen jedes Zeichen eines Uebergebenen Strings ein Leerzeichein ein
--  dient der besseren Lesbarkeit ( ______ -> _ _ _ _ _ )
insertSpace :: [Char] -> [Char]
insertSpace []=""
insertSpace (x:xs)=x:' ':insertSpace xs

  