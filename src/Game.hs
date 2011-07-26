module Game where
import Output
import HelpFunctions
import Init(initGame)

-- | startet das Spiel
start :: IO b
start=do  answer<-initGame
          let emptystr=createEmptyString answer
          showOutput emptystr 0 ""
          nextRound answer emptystr 0 []  
            
            
            
-- |  startet nach jedem eingegebenen Buchstaben eine neue Runde, sofern das Spiel noch nicht gewonnen/verloren wurde
nextRound :: String -> [Char] -> Integer -> [String] -> IO b
nextRound answer str mistake usedletters= if mistake < 10
                                             then playRound answer str mistake usedletters
                                             else newGame answer "\nVerloren!"
                                             
          
-- | ist fuer den eigentlichen Spielablauf zustaendig
-- Buchstabe eingeben, pruefen ob er vorkommt, Ergebnis ausgeben, naechte Runde/neues Spiel starten              
playRound :: String -> [Char] -> Integer -> [String] -> IO b                      
playRound answer oldstr mistake usedletters= 
                   do   
                       letter <- inputLetter usedletters
                       let newusedletters=letter:usedletters
                       letter <- convListToStr letter                     
                       let newstr=searchLetter answer letter
                       newmistake<-getMistakes answer letter mistake
                       let finalstr=assembleStrings newstr oldstr

                       showOutput finalstr newmistake newusedletters
                       
                       alreadywon<-completelyWord finalstr                      
                       if alreadywon==1
                          then newGame answer "\nGewonnen!"
                          else nextRound answer finalstr newmistake newusedletters          

              

-- | startet nachdem ein Spiel gewonnen/verloren wurde ein neues Spiel bzw. beendet das Programm                                             
newGame :: String -> [Char] -> IO b  
newGame answer message=do putStrLn message
                          putStr ("\nLoesung: ")
                          print $insertSpace answer
                          choice<-checkInput
                          if choice == 'J' || choice == 'j'
	                            then start
                             else error "Programm beendet!"
     
     
