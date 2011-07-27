module HelpFunctions where
import System.Console.ANSI
import Data.Char(toUpper)
import Data.Char(toLower)


-- | prueft ob der Benutzer ein richtiges Zeichen eingegeben hat   
checkInput :: IO Char                    
checkInput=do  putStr "\n\nNeue Runde starten? (J/N): "
               w <- getLine
               resetScreen
               w <- convListToStr w 
               if w /= 'J' && w /= 'j' && w /= 'N' && w/= 'n'
                then checkInput
                else return w 
                
-- | laesst den Benutzer so lange Buchstaben eingeben, bis er einen noch nicht verwendeten waehlt       
inputLetter :: [String] -> IO String
inputLetter usedletters=
                        do putStr "Buchstabe: "
                           letter <- getLine
                           
                           letterok<-checkLetterInUse usedletters letter
                           if letterok==1 
                              then return letter
                              else inputLetter usedletters
                              
-- | konvertiert eine Liste in einen String
convListToStr :: Monad m => [Char] -> m Char
convListToStr []=convListToStr " "
convListToStr (x:xs) = return $ toLower(x)  

-- | loescht das Konsolenfenster
resetScreen :: IO ()
resetScreen = clearScreen >> setSGR [Reset] >> setCursorPosition 0 0   

-- | erstellt einen aus Unterstrichen bestehenden String, der genauso viele Zeichen wie das Loesungswort besitzt
--  (Auto -> ____)
createEmptyString :: [t] -> [Char]
createEmptyString []=""
createEmptyString (x:xs)= '_':createEmptyString xs 
                         
-- | vergleicht die einzelnen Buchstaben des Loesungswortes mit dem eingegebnen Buchstabe 
--  sind die Buchstaben identisch (Gross-/Kleinschreibung wird nicht beachtet), wird der Buchstabe an den Rueckgabestring angehangen,
--  ansonsten ein Unterstrich _ 
searchLetter :: [Char] -> Char -> [Char]                    
searchLetter [] letter=""
searchLetter (x:xs) (letter)= if x /= letter && x/= toUpper(letter)
                         then '_': searchLetter xs letter
                         else letter: searchLetter xs letter   

                         
-- | ueberprueft ob der eingegebene Buchstabe mindestens einmal im Loesungswort vorkommt
--  ist dass der Fall bleibt die Fehleranzahl unveraendert, anosonsten wird sie um eins erhoeht
getMistakes :: (Num a, Monad m) => [Char] -> Char -> a -> m a                      
getMistakes [] letter mistakes=return (mistakes+1)                        
getMistakes (x:xs) (letter) mistakes=if x == letter || x==toUpper(letter)
                                then return (mistakes)
                                else getMistakes (xs) (letter) mistakes
            

-- | prueft ob das Wort vollstaendig geloest wurde            
completelyWord :: (Num a, Monad m) => [Char] -> m a                                
completelyWord []=return 1                               
completelyWord (x:xs)=if x=='_'
                  then return 0
                  else completelyWord xs
    




-- | bildet eine Gesamtloesung aus den vergangenen Runden und der aktuellen Runde, weil ansonsten immer nur der neuste richtig 
-- gewaehlte Buchstabe in der Loesung aufgedeckt waehre
assembleStrings :: [Char] -> [Char] -> [Char]
assembleStrings [] [] ="" 
assembleStrings (a:a2)(b:b2)=if a /= b
                         then 
                           if a=='_' 
                             then b:assembleStrings(a2)(b2) 
                             else a:assembleStrings(a2)(b2)
                         else a: assembleStrings a2 b2

                    
                         

-- | ueberprueft ob der eingegebene Buchstabe schon verwendet wurde                         
checkLetterInUse :: (Num a, Monad m, Eq a1) => [a1] -> a1 -> m a
checkLetterInUse [] b=return 1                     
checkLetterInUse (x:xs) b=if x == b 
                             then return 0
                             else checkLetterInUse xs b     