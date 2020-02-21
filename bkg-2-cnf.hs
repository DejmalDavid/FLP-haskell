{--
David Dejmal, xdejma00 VUT FIT FLP 2020
--}
import System.Environment   
import Data.Char  

-- main funkce programu --
main :: IO ()
main = do  
    args <- getArgs  
    
    let parametry = parsovaniArgumentu(args) 
    putStrLn ( show (parametry))
    
    let obsah = nacti(parametry)
    
    str <- obsah
    putStrLn(str)
    
-- parsovani argumentu ve tvaru bkg-2cnf volby [vstup]  --    
-- parsovaniArgumentu :: [[Char]] -> (Char, [Char])
parsovaniArgumentu ["-i"] = ('i',"")
parsovaniArgumentu ["-1"] = ('1',"")
parsovaniArgumentu ["-2"] = ('2',"")
parsovaniArgumentu ["-i",file] = ('i',file)
parsovaniArgumentu ["-1",file] = ('1',file)
parsovaniArgumentu ["-2",file] = ('2',file)
parsovaniArgumentu _ = ('E',"Chyba")
    

nacti (_,"") = getContents
nacti (_,file)  = readFile file
        
    