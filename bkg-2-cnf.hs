{--
David Dejmal, xdejma00 VUT FIT FLP 2020
--}
import System.Environment   
import Data.Char  
import Data.List.Split
import System.Environment
import System.Exit
import Control.Monad
import Data.Typeable --typeOf
import Data.List.Split --splitOn
import Data.List -- \\
import qualified Data.Set as Set
import Debug.Trace

data Pravidlo = Pravidlo { neterminal :: String
        , produkt :: String
        } deriving (Eq, Show)

data Gramatika = Gramatika { neterminaly :: [String]
        , terminaly :: [String]
        , pocatecniNeterminal :: String
        , pravidla :: [Pravidlo]
        } deriving (Show)


-- main funkce programu --
main :: IO ()
main = do  
    args <- getArgs  
    
    let parametry = parsovaniArgumentu(args) 
    putStrLn ( show (parametry))    -- TODO 
    
    let obsah = nacti(parametry)
    str <- obsah
    
    doStufs parametry str


doStufs ('i',_) content = do
    let gramatika = parsujVstup(lines content)
    --putStrLn(show gramatika)
    let a = testPocatecni gramatika
    let b = testRulse gramatika

    if (a && b == True)
    then printGramatika gramatika
    else error "Semanticka chyba gramatiky!!"



doStufs ('1',_) content = putStrLn("volam jsem 1") >> fukce1 (content)  -- TODO
doStufs ('2',_) content = putStrLn("volam jsem 2") >> fukce2 (content)  -- TODO
doStufs _ content = error "Špatně uvedene parametry."

printGramatika:: Gramatika -> IO ()
printGramatika gramatika = do putStrLn (spoj (neterminaly gramatika))
                              putStrLn (spoj (terminaly gramatika))
                              putStrLn (pocatecniNeterminal gramatika)
                              putStrLn (spoj' (pravidla gramatika))

spoj :: [String] -> String
spoj [x]    = x
spoj (x:xs) = x ++ [','] ++ (spoj xs) 

spoj' :: [Pravidlo] -> String
spoj' [x]    = toStr x
spoj' (x:xs) = toStr x ++ ['\n'] ++ (spoj' xs)

toStr :: Pravidlo -> String
toStr x = neterminal x ++ ['-']++ ['>'] ++ produkt x



testPocatecni gramatika =  elem (pocatecniNeterminal gramatika) (neterminaly gramatika)
testRulse  gramatika = testRule gramatika (pravidla gramatika)

testRule:: Gramatika -> [Pravidlo] -> Bool
testRule gramatika [] = True
testRule gramatika (x:xs) = if (elem (neterminal x) (neterminaly gramatika) && (testRuleRight gramatika (produkt x)) )
                            then testRule gramatika xs
                            else False
                            
testRuleRight:: Gramatika -> String -> Bool    
testRuleRight gramatika [] = True                   
testRuleRight gramatika (x:xs) = if (elem  [x] (neterminaly gramatika) || elem  [x] (terminaly gramatika) )
                            then testRuleRight gramatika xs
                            else False
                            

-- parsovani argumentu ve tvaru bkg-2cnf volby [vstup]    
parsovaniArgumentu :: [String] -> (Char, [Char])
parsovaniArgumentu ["-i"] = ('i',"")
parsovaniArgumentu ["-1"] = ('1',"")
parsovaniArgumentu ["-2"] = ('2',"")
parsovaniArgumentu ["-i",file] = ('i',file)
parsovaniArgumentu ["-1",file] = ('1',file)
parsovaniArgumentu ["-2",file] = ('2',file)
parsovaniArgumentu _ = ('E',"Chyba")
    
-- nacteni vstupu 
nacti (_,"") = getContents
nacti (_,"Chyba") = error "Špatně uvedene parametry!"
nacti (_,file)  = readFile file
        

parsujVstup obsah = if ( length obsah < 4 ) 
                then error "Chybny vstup."
                else Gramatika { neterminaly = parsujNeterminaly obsah
                , terminaly = parsujTerminaly obsah
                , pocatecniNeterminal = parsujPocatecni obsah
                , pravidla = parsujPravidla obsah
                }

parsujNeterminaly  ( radek1 : _ ) =  map countOneNeterminal (splitOn "," radek1)
parsujTerminaly (_: radek2 : _) = map countOneTerminal (splitOn "," radek2)

--TODO kontrola ze pocatecni je v neterminalech
parsujPocatecni (_: _ : radek3 : _) = if (length radek3 == 1)
                                        then radek3
                                        else  error "Počáteční neterminal musi být pouze jeden!!"

--TODO kontrola zda byl nereminal(i produkt???) definovany

parsujPravidla (_: _ : _ : radek4) = parsujPravidlo radek4

parsujPravidlo seznam =  map (\t->createPravidlo t) seznam
createPravidlo t
    | length(rez)/=2 = error "Špatně zapsané pravidlo!!!"
    | otherwise = Pravidlo { neterminal=from, produkt=over}
    where   rez = splitOn "->" t
            from = rez!!0
            over = rez!!1


isStringUpper:: [Char] -> Bool
isStringUpper [] = True
isStringUpper (x:xs) = if (isUpper x)
                        then isStringUpper xs
                        else False

isStringLower:: [Char] -> Bool
isStringLower [] = True
isStringLower (x:xs) = if (isLower x)
                        then isStringLower xs
                        else False

countOneNeterminal:: [Char] -> [Char]
countOneNeterminal [] = error "Špatně uvedene Neterminaly! Čarka na konci řádku?"
countOneNeterminal x = if (isStringUpper x && (length x == 1))
             then x
             else error "Špatně uvedene Neterminaly! Neterminal musi obsahovat pouze velke znaky!"

countOneTerminal:: [Char] -> [Char]
countOneTerminal [] = error "Špatně uvedene Terminaly! Čarka na konci řádku?"
countOneTerminal x = if (isStringLower x  && (length x == 1))
             then x
             else error "Špatně uvedene Terminaly! Terminaly musi obsahovat pouze velke znaky!"



fukce1 content = putStrLn("volal jsem 1") >> putStrLn(content) -- TODO
fukce2 content = putStrLn("volal jsem 2") >> putStrLn(content) -- TODO