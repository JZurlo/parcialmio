module Library where
import PdePreludat

doble :: Number -> Number
doble x = x*2

data Postre = Postre {
    sabores :: [Sabor],
    peso :: Number,
    temperatura :: Number
} deriving (Show, Eq)

type Sabor = String 

--A)

bizcochoBorracho = Postre ["Fruta","Crema"] 100 25 

--B)
incendio :: Postre -> Postre
incendio postreA = Postre(sabores postreA) (0.95*peso postreA) (1+temperatura postreA)

immobulus :: Postre -> Postre
immobulus postreA = postreA{temperatura = 0}


wingardiumLeviosa :: Postre -> Postre
wingardiumLeviosa postreA = Postre(agregarSaborConcentrado postreA) (0.90*peso postreA) (temperatura postreA)

agregarSaborConcentrado :: Postre -> [Sabor]
agregarSaborConcentrado postreA = sabores postreA ++ ["Concentrado"]

diffindo :: Number -> Postre -> Postre
diffindo porcentaje postreA = postreA{peso = (1-porcentaje/100) * peso postreA}

riddikulus :: Sabor -> Postre -> Postre
riddikulus unSabor postreA =  Postre(agregarUnSaborInvertido unSabor postreA) (peso postreA) (temperatura postreA)

agregarUnSaborInvertido:: Sabor -> Postre -> [Sabor]
agregarUnSaborInvertido unSabor postreA = sabores postreA ++ [reverse unSabor]

avadaKedavra :: Postre -> Postre
avadaKedavra postreA = immobulus postreA{sabores = []}

--C)
chocotorta = Postre ["Chocolinas","Casancrem","DulceDeLeche"] 300 20
budin = Postre ["Harina","Vainilla","Limon"] 50 30 
lemonPie = Postre ["Limon","Crema"] 200 10 

postres = [chocotorta,budin,lemonPie]

--postreQuedaListo :: Hechizo -> Postre -> Bool
--postreQuedaListo unHechizo


--D)
pesoPromedio :: [Postre] -> Number
pesoPromedio postres
    |postres == [] = 0
    |otherwise = foldl ($ peso (+)) 0(postres) / (length postres)


--E)
crearPostresInfinitos:: [Postre] -> [Postre]
crearPostresInfinitos (x:xs) = [x] ++ (crearPostresInfinitos [x] ++ xs)

-- ¿Existe alguna consulta que pueda hacer para que me sepa dar una respuesta? 
--Spec Library Spec> postreQuedaListo mesaDulce postres
mesaDulce = Postre (crearPostresInfinitos postres)