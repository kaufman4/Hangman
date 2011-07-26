module Main where
import System.IO(hSetBuffering, stdout, BufferMode(NoBuffering))
import Game

-- | main startet das Main-Programm              
main :: IO b             
main = do hSetBuffering stdout NoBuffering
          start
          

          

          