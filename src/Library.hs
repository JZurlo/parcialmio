module Library where
import PdePreludat

doble :: Number -> Number
doble x = x*2

data Postre = Postre {
    sabores :: [Sabor],
    peso :: Number,
    temperatura :: Number
} deriving (Show, Eq)


data Sabor = Sabor {
    nombre :: String
} deriving (Show, Eq)

--A)
fruta = Sabor "Fruta"
crema = Sabor "Crema"
concentrado = Sabor "Concentrado"


bizcochoBorracho = Postre [fruta,crema] 100 25 

--B)
incendio :: Postre -> Postre
incendio postreA = Postre(sabores postreA) (0.95*peso postreA) (1+temperatura postreA)

immobulus :: Postre -> Postre
immobulus postreA = postreA{temperatura = 0}


wingardiumLeviosa :: Postre -> Postre
wingardiumLeviosa postreA = Postre(agregarSaborConcentrado postreA) (0.90*peso postreA) (temperatura postreA)

agregarSaborConcentrado :: Postre -> [Sabor]
agregarSaborConcentrado postreA = sabores postreA ++ [concentrado]
