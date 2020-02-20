{--
David Dejmal, xdejma00 VUT FIT FLP 2020
--}
import System.Environment   
  
main = do  
   args <- getArgs     
-- parsovani argumentu ve tvaru bkg-2cnf volby [vstup]  --
   case args of
        ["-i"] -> putStrLn "param i"  
        ["-1"] -> putStrLn "param 1"  
        ["-2"] -> putStrLn "param 2"  
        ["-i",_] -> putStrLn "param i + file"  
        ["-1",_] -> putStrLn "param 1+ file"  
        ["-2",_] -> putStrLn "param 2+ file"  
        otherwise -> putStrLn "Usage: bkg-2cnf volby [vstup]"  
        
        
