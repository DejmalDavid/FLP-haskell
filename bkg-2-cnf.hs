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

-- datova struktura pro gramatiku
data Gramatika = Gramatika { neterminaly :: [String] -- neterminaly
        , terminaly :: [String] --terminaly
        , pocatecniNeterminal :: String     --pocatecni neterminal
        , pravidla :: [(String, [String])] -- seznam pravidel
        } deriving (Show) --pro testovaci ucely 


-- main funkce programu --
main :: IO ()
main = do  
    args <- getArgs  
    
    let parametry = parsovaniArgumentu(args) --cteni argumentu
    putStrLn ( show (parametry))    -- TODO 
    
    let obsah = nacti(parametry) --nacteni obsahu podle prepinacu
    str <- obsah
    
    doStufs parametry str   --hlavni funkce programu

-- nacteni do vnitrni struktury a vypis
doStufs ('i',_) content = do
    let gramatika = parsujVstup(lines content)  --nacteni
    --putStrLn(show gramatika)
    --semanticka kontrola gramatiky
    let a = testPocatecni gramatika
    let b = testRulse gramatika

    if (a && b == True)
    then printGramatika gramatika   --vypis
    else error "Semanticka chyba gramatiky!!"


-- nacteni, odstaneni jednoduchych pravidel a vypis
doStufs ('1',_) content = do 
    --putStrLn("volam jsem 1") 
    let gramatika = parsujVstup(lines content)--nacteni
    putStrLn(show gramatika)
        --semanticka kontrola gramatiky
    let a = testPocatecni gramatika
    let b = testRulse gramatika

    if (a && b == False)
    then error "Semanticka chyba gramatiky!!"
    else do 
    let gramtika_bez = getGramatikaBez gramatika --odstraneni jednoduchych pravidel
    --putStrLn(show gramtika_bez)
    printGramatika gramtika_bez --vypis


doStufs ('2',_) content = do    
    putStrLn("volam jsem 2") 
    let gramatika = parsujVstup(lines content)--nacteni
    --putStrLn(show gramatika)
    --semanticka kontrola gramatiky
    let a = testPocatecni gramatika
    let b = testRulse gramatika

    if (a && b == False)
    then error "Semanticka chyba gramatiky!!"
    else do 
    let gramtika_bez = getGramatikaBez gramatika --odstraneni jednoduchych pravidel
    putStrLn(show gramtika_bez)
    let gramtika_CNF = getGramatikaCNF gramtika_bez --odstraneni jednoduchych pravidel
    printGramatika gramtika_CNF --vypis

doStufs _ content = error "Špatně uvedene parametry."


--getGramatikaCNF gramatika = gramatika
-----------------------START COPY PASTE ------------

{-|
    K jednomu pravidlu gramatiky vrátí seznam pravidel v chomského normální
    formě.
-}
split_rule_to_cnf::Gramatika->(String, [String])->[(String, [String])]
--pravidla A->a se vrací neupravená, jednoduché pravidlo způsobí chybu.
split_rule_to_cnf g r@(_, x:[])
    |(x`elem`(terminaly g))=[r]
    |otherwise=error "Simple rule."
--zpracování pravidla jehož pravá část se skládá ze dvou částí
split_rule_to_cnf g r@(a, b:c:[])
    |(b`elem`(terminaly g))&&(c`elem`(terminaly g))=[(a, [b++"'",c++"'"]),(b++"'",[b]),(c++"'",[c])]
    |(b`elem`(terminaly g))=[(a, [(b++"'"),c]),(b++"'",[b])]
    |(c`elem`(terminaly g))=[(a, [b,(c++"'")]),(c++"'",[c])]
    |otherwise=[r]
--zpracování pravidla jehož pravá část se skládá z více jak dvou částí
split_rule_to_cnf g (a, x:xs)
    --pravá strana začíná terminálen
    |(x`elem`(terminaly g))=
        (a, [(x++"\'"),nn]):(x++"\'", [x]):(split_rule_to_cnf g (nn, xs))
    --pravá strana začíná neterminálen
    |otherwise=
        (a, [x, nn]):(split_rule_to_cnf g (nn, xs))
    where nn="<"++(foldr1 (++) xs)++">"

{-|
    Získá neterminály z levých stran pravidel.
-}
get_nonterminals_from_rules::[(String, [String])]->[String]
get_nonterminals_from_rules l=foldl step [] l
    where step acc (n, _)=if n`elem`acc then acc else n:acc

{-|
    Převede gramatiku do chomského normální formy podle algoritmu 4.7 ze
    studijní opory předmětu TIN.
-}
getGramatikaCNF::Gramatika->Gramatika
getGramatikaCNF g=
    --pravidla nové gramatiky
    let rs=(foldl step [] (pravidla g))
    in
    Gramatika (terminaly g) ((neterminaly g)`union`(get_nonterminals_from_rules rs)) (pocatecniNeterminal g) rs
    where 
        step::[(String, [String])]->(String, [String])->[(String, [String])]
        step acc item=acc`union`(split_rule_to_cnf g item)

-----------------------END COPY PASTE ------------

-- tvorba nove gramatiky bez jednoduchych pravidel
getGramatikaBez:: Gramatika -> Gramatika
getGramatikaBez gramatika = Gramatika (terminaly gramatika) --stejne
                                      (neterminaly gramatika) --stejne
                                      (pocatecniNeterminal gramatika) --stejne
                                      (foldl (transform (neterminaly gramatika) (vytvorMnoziny gramatika)) [] (pravidla gramatika)) --nove vytvorene
                                      where 
                                        vytvorMnoziny gramatika = map (\x->(x, vytvorMnozinu gramatika [x])) (neterminaly gramatika) -- volani nad vsemi
                                        
                                        
-- jadro algoritmu 4.5, vypocet mnoziny N 
vytvorMnozinu::Gramatika->[String]->[String]
vytvorMnozinu gramatika n0 = if n0 /= nx
                             then vytvorMnozinu gramatika (nx) -- iteruje
                             else n0    --konec
                             where nx = union n0 (foldl (krok45 gramatika n0) [] (pravidla gramatika)) --sjednotit mnoziny a iterovat

-- prida prvekt do mnoziny N     
krok45::Gramatika->[String]->[String]->(String, [String])->[String]
krok45 gramatika n0 senznam (nonterm, produkt:[]) = if (elem produkt (neterminaly gramatika)) && (elem nonterm n0)
                                                    then (produkt:senznam)       
                                                    else senznam --konec

--transformace pravidel, tvorba novych 
transform nonterms nMnoziny seznam (nonterm, produkt)=
            if ((elem (head produkt) nonterms) && ((tail produkt) == [])) == False
            then seznam ++ (map (\x->(x, produkt)) (najdiMnoziny nMnoziny nonterm)) --iterace
            else seznam --konec

--vypis
printGramatika:: Gramatika -> IO ()
printGramatika gramatika = do putStrLn (spoj (neterminaly gramatika))
                              putStrLn (spoj (terminaly gramatika))
                              putStrLn (pocatecniNeterminal gramatika)
                              putStr (foldr1 (++) (map spojPravidla (pravidla gramatika)))

--priprava na vypis
spoj :: [String] -> String
spoj [x]    = x
spoj (x:xs) = x ++ [','] ++ (spoj xs) 

--hleda mnoziny a vytvori z nich seznam
najdiMnoziny::[(String,[String])]->String->[String]
najdiMnoziny [] _ = []
najdiMnoziny ((nonterm,produkt):xs) neterminal = if elem neterminal produkt
                                                 then nonterm:(najdiMnoziny xs neterminal)
                                                 else najdiMnoziny xs neterminal
--priprava na vypis pravidel
spojPravidla :: (String,[String]) -> String
spojPravidla (nonterm,term) = nonterm++'-':'>':(foldr1 (++) term)++['\n']

--sematicka kontrola
testPocatecni gramatika =  elem (pocatecniNeterminal gramatika) (neterminaly gramatika)

--sematicka kontrola pravidel
testRulse  gramatika = testRule gramatika (pravidla gramatika)

--kontrola jednotliveho pravidla
testRule:: Gramatika -> [(String, [String])] -> Bool
testRule gramatika [] = True
testRule gramatika ((non,term):xs)  = if (elem non (neterminaly gramatika) && (testRuleRight gramatika term)  )
                            then testRule gramatika xs
                            else False
                            
--test prave strany pravidla                            
testRuleRight:: Gramatika -> [String] -> Bool    
testRuleRight gramatika [] = True                   
testRuleRight gramatika (x:xs) = if (checkTerms gramatika x)  
                                 then testRuleRight gramatika xs
                                 else False

checkTerms:: Gramatika -> String -> Bool 
checkTerms gramatika [] = True
checkTerms gramatika (char:xs) = if (elem [char] (terminaly gramatika) || elem [char] (neterminaly gramatika) )
                                 then checkTerms gramatika xs
                                 else False
                                 

--elem [x] (terminaly gramatika) 
--elem [x] (terminaly gramatika)
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
parsujPocatecni (_: _ : radek3 : _) = if (length radek3 == 1)
                                        then radek3
                                        else  error "Počáteční neterminal musi být pouze jeden!!"
parsujPravidla (_: _ : _ : radek4) = parsujPravidlo radek4

--parsovani pravidla je slozitejsi
parsujPravidlo seznam = map (\t->createPravidlo t) seznam

--jednotlive pravidlo
createPravidlo t
    | length(rez)/=2 = error "Špatně zapsané pravidlo!!!"
    | otherwise = (non,[term])
    where   rez = splitOn "->" t
            non = rez!!0
            term = rez!!1

--pomocna funkce
isStringUpper:: [Char] -> Bool
isStringUpper [] = True
isStringUpper (x:xs) = if (isUpper x)
                        then isStringUpper xs
                        else False
                        
--pomocna funkce
isStringLower:: [Char] -> Bool
isStringLower [] = True
isStringLower (x:xs) = if (isLower x)
                        then isStringLower xs
                        else False
                        
--pomocna funkce
countOneNeterminal:: [Char] -> [Char]
countOneNeterminal [] = error "Špatně uvedene Neterminaly! Čarka na konci řádku?"
countOneNeterminal x = if (isStringUpper x && (length x == 1))
             then x
             else error "Špatně uvedene Neterminaly! Neterminal musi obsahovat pouze velke znaky!"
             
--pomocna funkce
countOneTerminal:: [Char] -> [Char]
countOneTerminal [] = error "Špatně uvedene Terminaly! Čarka na konci řádku?"
countOneTerminal x = if (isStringLower x  && (length x == 1))
             then x
             else error "Špatně uvedene Terminaly! Terminaly musi obsahovat pouze velke znaky!"


fukce2 content = putStrLn("volal jsem 2") >> putStrLn(content) -- TODO