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

type Mesasa = [Postre]
type Hechizo = Postre -> Postre

mesasa1 :: Mesasa
mesasa1 = [chocotorta,budin,lemonPie]

postreQuedaListo :: Postre -> Bool
postreQuedaListo postreA = (peso postreA > 0) && (length (sabores postreA)> 0) && (temperatura postreA > 0)

mesaLista :: Mesasa -> Hechizo -> Bool
mesaLista mesaA hechizoA = all postreQuedaListo (map hechizoA mesaA)

--D)
soloListos :: Mesasa -> Mesasa
soloListos mesasaA = filter postreQuedaListo mesasaA -- Filtro solo los listos

pesoPromedio :: Mesasa -> Number
pesoPromedio mesasa1
    |mesasa1 == [] = 0 --Si no hay postres devuelvo 0
    |otherwise = (sum (map peso (soloListos mesasa1))) / (length (soloListos mesasa1))
--Filtro los listos
--Obtengo el peso de cada uno y dejo una lista de pesos (Numeric)
--Sumo la lista de pesos
--Divido por el Length de los postres listos


--E)
crearMesasaInfinita:: Mesasa-> Mesasa
crearMesasaInfinita (x:xs) = [x] ++ (crearMesasaInfinita [x] ++ xs)

mesaDulce = crearMesasaInfinita [chocotorta]
-- ¿Existe alguna consulta que pueda hacer para que me sepa dar una respuesta? 
--Spec Library Spec> mesaLista mesaDulce avadaKedavra
--False
--Por mas que la mesaDulce sea una mesa infinita de postres chocotorta (la cual contiene sabores),
--al aplicar el hechizo avadaKedavra vaciaré los sabores y debido a que Haskell aplica evaluacion diferida,
--no se queda realizando el loop infinito, dado que ya sabe que los nuevos Postres no tendran sabores