data Pravidlo = Pravidlo { neterminal :: String
        , produkt :: String
        } deriving (Eq, Show)
        
parsujPravidlo list [] = list
parsujPravidlo list (x:y) = list ++ x ++ parsujPravidlo [] y
parsujPravidlo [] (x:y) = [] : x : parsujPravidlo [] y