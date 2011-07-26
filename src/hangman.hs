module Main where
import System.IO(hSetBuffering, stdout, BufferMode(NoBuffering))
import System.Random
import Data.Char(toUpper)
import System.Console.ANSI
import System.Directory
import Ascii


-- | getWord liefert das durch die Zufallszahl ausgewaehlte Wort aus der Woerterliste zurueck
getWord :: (Num a, Monad m) => [a1] -> a -> a -> m a1
getWord (x:xs) init end= if init /= end
                            then getWord xs (init+1) end
                            else return(x)
      
-- |  randomNumberIO liefert eine Zufallszahl die zwischen 0 und der Anzahl der Woerter in der Woerterliste liegt zurueck     
randomNumberIO :: Int -> IO Int
randomNumberIO wordcount= do zahl <- randomRIO(0,wordcount)
                             return zahl
                         
-- | searchLetter vergleicht die einzelnen Buchstaben des Loesungswortes mit dem eingegebnen Buchstabe 
--  sind die Buchstaben identisch (Gross-/Kleinschreibung wird nicht beachtet), wird der Buchstabe an den Rueckgabestring angehangen,
--  ansonsten ein Unterstrich _ 
searchLetter :: [Char] -> Char -> [Char]                    
searchLetter [] letter=""
searchLetter (x:xs) (letter)= if x /= letter && x/= toUpper(letter)
                         then '_': searchLetter xs letter
                         else letter: searchLetter xs letter   

                         
-- | getMistakes ueberprueft ob der eingegebene Buchstabe mindestens einmal im Loesungswort vorkommt
--  ist dass der Fall bleibt die Fehleranzahl unveraendert, anosonsten wird sie um eins erhoeht
getMistakes :: (Num a, Monad m) => [Char] -> Char -> a -> m a                      
getMistakes [] letter mistakes=return (mistakes+1)                        
getMistakes (x:xs) (letter) mistakes=if x == letter || x==toUpper(letter)
                                then return (mistakes)
                                else getMistakes (xs) (letter) mistakes
            

-- | completelyWord prueft ob das Wort vollstaendig geloest wurde            
completelyWord :: (Num a, Monad m) => [Char] -> m a                                
completelyWord []=return 1                               
completelyWord (x:xs)=if x=='_'
                  then return 0
                  else completelyWord xs
    

-- | convListToStr: Hilfsfunktion die eine Liste in einen String konvertiert
convListToStr :: Monad m => [Char] -> m Char
convListToStr []=convListToStr " "
convListToStr (x:xs) = return x    


-- | assembleStrings bildet einen gesamt Loesungsstring aus dem Loesungsstring der vorrunden und der aktuellen Runde
assembleStrings :: [Char] -> [Char] -> [Char]
assembleStrings [] [] ="" 
assembleStrings (a:a2)(b:b2)=if a /= b
                         then 
                           if a=='_' 
                             then b:assembleStrings(a2)(b2) 
                             else a:assembleStrings(a2)(b2)
                         else a: assembleStrings a2 b2

-- | resetScreen loescht das Konsolenfenster
resetScreen :: IO ()
resetScreen = clearScreen >> setSGR [Reset] >> setCursorPosition 0 0                         
                         

-- | checkLetterInUse ueberprueft ob der eingegebene Buchstabe schon verwendet wurde                         
--checkLetterInUse :: (Num a, Monad m, Eq a1) => [a1] -> a1 -> m a
checkLetterInUse [] b=return 1                     
checkLetterInUse (x:xs) b=if x == b 
                             then return 0
                             else checkLetterInUse xs b       
            
-- | inputLetter wird so lange wiederholt bist ein noch nicht verwendetes Zeichen eingegeben wurde            
inputLetter :: [String] -> IO String
inputLetter usedletters=
                        do putStr "Buchstabe: "
                           letter <- getLine
                           
                           --letter <- getChar
                           letterok<-checkLetterInUse usedletters letter
                           if letterok==1 
                              then return letter
                              else inputLetter usedletters
              
-- | playGame              
playGame :: String -> [Char] -> Integer -> [String] -> IO b                      
playGame answer oldstr mistake usedletters= 
                   do   
                       letter <- inputLetter usedletters
                       let newusedletters=letter:usedletters
                       letter <- convListToStr letter                     
                       let newstr=searchLetter answer letter
                       newmistake<-getMistakes answer letter mistake
                       let finalstr=assembleStrings newstr oldstr
                       resetScreen
                       putStr "\n"
                       print $insertSpace finalstr 

                       putStr "\n"
                       putStr "Verwendete Buchstaben: "
                       print newusedletters
                       putStr "\n"
 
                       paintHangman newmistake
                       putStr "\n"
                       g<-completelyWord finalstr                      
                       if g==1
                          then newGame answer "\nGewonnen!"
                          else startGame answer finalstr newmistake newusedletters
                       




                
                
     
-- | createEmptyString erstellt einen aus Unterstrichen bestehenden String, der genauso viele Zeichen wie das Loesungswort besitzt
--  (Auto -> ____)
createEmptyString :: [t] -> [Char]
createEmptyString []=""
createEmptyString (x:xs)= '_':createEmptyString xs   


-- | insertSpace fuegt zwischen jedes Zeichen eines Uebergebenen Strings ein Leerzeichein ein
--  dient der besseren Lesbarkeit ( ______ -> _ _ _ _ _ )
insertSpace :: [Char] -> [Char]
insertSpace []=""
insertSpace (x:xs)=x:' ':insertSpace xs

-- | startGame startet so lange wie das gesuchte Wort noch nicht vollstaendig geloest wurde und weniger als 10 Fehler gemacht wurden, 
--  eine neue Runde
startGame :: String -> [Char] -> Integer -> [String] -> IO b
startGame answer str mistake usedletters= if mistake < 10
                                             then playGame answer str mistake usedletters
                                             else newGame answer "\nVerloren!"

-- | newGame startet nachdem ein Spiel gewonnen/verloren wurde ein neues Spiel bzw. beendet das Programm                                             
newGame :: String -> [Char] -> IO b  
newGame answer message=do putStrLn message
                          putStr ("\nLoesung: ")
                          print $insertSpace answer
                          choice<-checkInput
                          if choice == 'J' || choice == 'j'
	                          then main
                           else error "Programm beendet!"
     
     
-- | checkInput    
checkInput :: IO Char                    
checkInput=do  putStr "\n\nNeue Runde starten? (J/N): "
               w <- getLine
               resetScreen
               w <- convListToStr w 
               if w /= 'J' && w /= 'j' && w /= 'N' && w/= 'n'
                then checkInput
                else return w
--doesFileExists
-- | main               
main :: IO b             
main = do hSetBuffering stdout NoBuffering
          words <- readFile "Woerter"

          let wordlist = lines words
     
          let wordcount= (length wordlist)-1

          randomnumber<-randomNumberIO wordcount

          answer <- getWord wordlist 0 randomnumber
            
          let emptystr=createEmptyString answer
          putStr "\n"
          print $insertSpace emptystr
          putStr "\n"
          putStr "Verwendete Buchstaben: "
          putStr "\n"
          paintHangman 0
          putStr "\n"
          startGame answer emptystr 0 []
          

          

          