module Pertenencia where
import Tipos

--Teniendo en cuenta que:
----x es la lista de todos los estados del automata
----y es el alfabeto
----z es la lista de transiciones
----u es el estado inicial
----v es la lista de estados con doble circulo


--PRUEBAS REALIZADAS
--Hemos utilizado las pruebas de EGela
----AUTOMATAS D2,D3,D3s (AFD), N2,N3,N3s,N4(AFND)
-----Palabras:
-------aaa:    Esperado-> True, Resultado->True
-------baba:   Esperado-> True, Resultado->True
-------baaac:  Esperado-> False, Resultado->False
-------ca:     Esperado-> False, Resultado->False
-------b:      Esperado-> False, Resultado->False
----AUTOMATAS F1,F2
-----Palabras:
-------aab:    Esperado-> True, Resultado->True
-------aacbsb: Esperado-> False, Resultado->False
-------bcaacb: Esperado-> True, Resultado->True
-------aa:     Esperado-> False, Resultado->False
-------b:      Esperado-> False, Resultado->False
-------ab:     Esperado-> False, Resultado->False
----AUTOMATAS N6,N6a
-----Palabras:
-------a:      Esperado-> True, Resultado->True
-------aa:     Esperado-> True, Resultado->True
-------aab:    Esperado-> False, Resultado->False
-------ab:     Esperado-> True, Resultado->True
-------abscb:  Esperado-> False, Resultado->False
-------aaaba:  Esperado-> True, Resultado->True
-------cbb:    Esperado-> False, Resultado->False

--Funcion que dados un AF y una palabra, devuelve True si la palabra pertenece al lenguaje definido por el AF y devuelve F alse en caso contrario.
--COSTE COMPUTACIONAL DE PERTENENCIA
-----El coste seria n*m siendo n el numero de transiciones en total y siendo m el numero de letras de la palabra

pertenencia::Af->Palabra->Bool
pertenencia (x,y,z,u,v) a 
	|(null a) && (veces_rf u v)/=0 = True
	|(null a) && (veces_rf u v)==0 = False
	|otherwise = pertenencia_aux (x,y,z,u,v) a [] 0 

--En pertenencia_aux se anaden dos variables:
---- cruces : [(Int,Estado)] se guarda el estado y el indice de la palabra desde donde se puede avanzar ese Estado
---- ind    : Para saber que letra de la palabra utilzar
pertenencia_aux::Af->Palabra->[(Int,Estado)]->Int->Bool
pertenencia_aux (x,y,z,u,v) a cruces ind
	|((length a)==ind) && (veces_rf u v)/=0 = True --La palabra pertenece al lenguaje definido por el automata
	|(((length a)==ind && (veces_rf u v)==0) || (null (avanzar z (a!!ind) u ind []))) && (null cruces) = False --El lenguaje no pertenece
	|(((length a)==ind && (veces_rf u v)==0) || (null (avanzar z (a!!ind) u ind []))) && not(null cruces) = pertenencia_aux (x,y,z,snd(head cruces),v) a (tail cruces) ((fst(head cruces))+1) --No hay por donde avanzar, pero en la variable cruces todavia hay transiciones por las que avanzar
	|(null (tail (avanzar z (a!!ind) u ind []))) = pertenencia_aux (x,y,z,snd(head (avanzar z (a!!ind) u ind [])),v) a cruces (ind+1) --si solo hay un sitio por el que avanzar no se anade nada a cruces
	|otherwise = pertenencia_aux (x,y,z,snd(head (avanzar z (a!!ind) u ind [])),v) a ((tail(avanzar z (a!!ind) u ind []))++cruces) (ind+1) 
--Si hay mas de un sitio por el que avanzar, se anade el que no se utilice a los cruces


--Devuelve la transicion con la que se puede avanzar introduciendo las transiciones, el estado actual y el simbolo con el que se quiere avanzar
--En caso de no poder avanzar(cuando no existan transiciones para ese simbolo y estado) se devolvera una lista vacia
--COSTE: Tiene coste constante con respecto al numero de transiciones del automata.
avanzar::Transiciones->Simbolo->Estado->Int->[(Int,Estado)]->[(Int,Estado)]
avanzar t s q ind lis
	|(null t) = lis
	|(pri(head t))==q && (sec(head t))==s = avanzar (tail t) s q ind ((ind,ter(head t)):lis) --Transicion valida, se anade
	|otherwise = avanzar (tail t) s q ind lis --Transicion no valida, no se anade
	
--Para elegir el tercer elemento de una transicion --> coste constante
ter::Transicion->Estado 
ter (a,b,c) = c

--Para elegir el primer elemento de una transicion --> coste constante
pri::Transicion->Estado
pri(a,b,c) = a

--Para elegir el segundo elemento de una transicion --> conste constante
sec::Transicion->Simbolo
sec(a,b,c) = b


--En este caso veces_rf se utiliza para saber si un estado tiene doble circulo
--COSTE: Tiene coste n siendo n el numero de elementos de la lista
veces_rf::(Eq t)=>t->[t]->Int
veces_rf x s = veces_rf_aux x s 0
	
veces_rf_aux::(Eq t)=>t->[t]->Int->Int
veces_rf_aux x s y
	|s==[] = y
	|x==(head s) = veces_rf_aux x (tail s) y+1
	|otherwise = veces_rf_aux x (tail s) y
