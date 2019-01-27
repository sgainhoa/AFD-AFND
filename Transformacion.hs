module Transformacion where
import Pruebas
import Tipos

--Se crean estos nuevos tipos ya que al pasar a un AFD se juntan estados
type Transicion2 = (Estados,Simbolo,Estados)
type Transiciones2 = [Transicion2]
type EstadosN = [Estados]
type EstadosRen = [(Estados,Estado)]

----------------------------------------------------------------------------------------------------------------------------------------------------

--PRUEBAS:

----PRUEBAS CON AFND
-------Al transformar el Automata N6 tiene que dar el automata N6s
-------N6s = ([0,1,2,3,4,5],['a','b','c'],[(0,'a',1),(0,'b',2),(0,'c',2),(1,'a',3),(1,'b',1),(1,'c',1),(2,'a',4),(2,'b',2),(2,'c',2),(3,'a',3),(3,'b',5),(3,'c',5),(4,'a',3),(4,'b',1),(4,'c',1),(5,'a',3),(5,'b',5),(5,'c',5)],0,[1,3,4])

-------Nuestro resultado: ([0,5,4,3,2,1],"abc",[(0,'a',4),(0,'b',5),(0,'c',5),(5,'a',3),(5,'b',5),(5,'c',5),(4,'a',2),(4,'b',4),(4,'c',4),(3,'a',2),(3,'b',4),(3,'c',4),(2,'a',2),(2,'b',1),(2,'c',1),(1,'a',2),(1,'b',1),(1,'c',1)],0,[4,3,2])

------Es el mismo automata solo que los nombres de los estados son distintos

------Al transformar el Automata N7 tiene que dar el automata N7s
-------N7s = ([0,1,2,3,4,5],['a','b','c'],[(0,'a',2),(0,'b',1),(0,'c',1),(1,'a',1),(1,'b',1),(1,'c',1),(2,'a',2),(2,'b',3),(2,'c',4),(3,'a',5),(3,'b',3),(3,'c',1),(4,'a',1),(4,'b',1),(4,'c',4),(5,'a',5),(5,'b',3),(5,'c',4)],0,[2,4,5])

-------Nuestro resultado: ([0,5,4,3,2,1],"abc",[(0,'a',4),(0,'b',5),(0,'c',5),(5,'a',5),(5,'b',5),(5,'c',5),(4,'a',4),(4,'b',2),(4,'c',3),(3,'a',5),(3,'b',5),(3,'c',3),(2,'a',1),(2,'b',2),(2,'c',5),(1,'a',1),(1,'b',2),(1,'c',3)],0,[4,3,1])

-------Es el mismo automata solo que los nombres de los estados son distintos, es decir, las palabras que pertenecen a N7s también pertenecen al nuestro


----PRUEBAS CON AFD
-------Al transformar el automataD AFD nos da el mismo resultado ya que D ya es determinista

-------Al transformar el automataD3 AFD el resultado es el siguiente:
-------Resultado: ([0,5,4,3,2,1],"abc",[(0,'a',4),(0,'b',0),(0,'c',5),(5,'a',3),(5,'b',3),(5,'c',5),(4,'a',2),(4,'b',0),(4,'c',5),(3,'a',5),(3,'b',5),(3,'c',3),(2,'a',1),(2,'b',0),(2,'c',5),(1,'a',1),(1,'b',1),(1,'c',5)],0,[4,2,1])

-------El automataD3 es determinista y al pasar por transformacion se simplifica pero sigue siendo el mismo automata, es decir las palabras que pertenecen a D3 tambien pertenecen al obtenido por transformacion

----------------------------------------------------------------------------------------------------------------------------------------------------


--COSTE DE TRANSFORMACION: 
---TRANSFORMACION_AUX:
----Tiene un coste de n*m*p siendo:
--------- n el numero de estados en el cada estado nuevo
--------- m el numero de simbolos en el alfabeto
--------- p el numero de estados nuevos
------Es decir que por cada estado nuevo que creamos tenemos que mirar todas las transiciones de todos los estados que esten dentro del estado nuevo

---RENOMBRAR:
-----Tiene un coste de l siendo l el numero de estados nuevos + el numero de transiciones nuevas + numero de estados de doble circulo

---EL COSTE TOTAL de transformacion es el coste de la funcion renombrar y transformacion_aux, es decir, (m*m*p)+l

----------------------------------------------------------------------------------------------------------------------------------------------------

--Teniendo en cuenta que:
----x es la lista de todos los estados del automata
----y es el alfabeto
----z es la lista de transiciones
----u es el estado inicial
----v es la lista de estados con doble circulo

transformacion::Af->Af
transformacion (x,y,z,u,v) = renombrar (transformacion_aux (x,y,z,u,v) [[u]] [] [[u]] [])

	
--La variable p se utiliza de pila para almacenar los estados conjuntos no examinados, es decir, para ir examinando las transiciones de los estados conjuntos
--La variable l se utiliza para ir almacenando las transiciones
--La variable e se utiliza para ir almacenando los estados creados
--La variable d se utiliza para ir guardando los estados de doble circulo

transformacion_aux::Af->[Estados]->Transiciones2->[Estados]->[Estados]->([Estados],Alfabeto,Transiciones2,Estado,[Estados])
transformacion_aux (x,y,z,u,v) p l e d
	|(null p) = (e,y,l,u,d)
	|(null(head p)) = transformacion_aux (x,y,z,u,v) (tail p) ((anadir_transiciones_estado_vacio y [])++l) e d --Estado vacio, pasa cuando no hay transicion posible con un simbolo en el automata original
	|((doble_circulo (head p) v) == True) && ((esta (head p) d)==False) = transformacion_aux (x,y,z,u,v) ((tail p)++(estados_sin_examinar e (nuevos_estados(juntar_transiciones z (head p) y []) e []) [])) ((juntar_transiciones z (head p) y [])++l) ((nuevos_estados (juntar_transiciones z (head p) y []) e [])++e) ((head p):d)
	|otherwise = transformacion_aux (x,y,z,u,v) ((tail p)++(estados_sin_examinar e (nuevos_estados(juntar_transiciones z (head p) y []) e []) [])) ((juntar_transiciones z (head p) y [])++l) ((nuevos_estados (juntar_transiciones z (head p) y []) e [])++e) d
	
-------------------------------------------------------------------------------------------------------------------------------------------------
renombrar::(EstadosN,Alfabeto,Transiciones2,Estado,EstadosN)->Af
renombrar (x,y,z,u,v) = ((estados_renombrados x u),y,(renombrar_transiciones (renombrar_estados x u) z),u,(renombrar_doble_circulo (renombrar_estados x u) v))

------------------------------------------------------------------
--Utilizamos este metodo para encontrar los estados de doble circulo en los estados ya renombrados, para asi devolver una lista del tipo Estados
renombrar_doble_circulo::EstadosRen->EstadosN->Estados
renombrar_doble_circulo x s = renombrar_doble_circulo_aux x s []

renombrar_doble_circulo_aux::EstadosRen->EstadosN->Estados->Estados
renombrar_doble_circulo_aux x s r
	|(null s) = r
	|otherwise = renombrar_doble_circulo_aux x (tail s) ((nuevo_nombre x (head s)):r)

	
------------------------------------------------------------------

renombrar_transiciones::EstadosRen->Transiciones2->Transiciones
renombrar_transiciones er t2 = renombrar_transiciones_aux er t2 []

--En la variable tn se almacenan las transiciones nuevas
renombrar_transiciones_aux::EstadosRen->Transiciones2->Transiciones->Transiciones
renombrar_transiciones_aux er ((e1,sim,e2):t2) tn  
	|(null t2) = ((nuevo_nombre er e1),sim,(nuevo_nombre er e2)):tn
	|otherwise = renombrar_transiciones_aux er t2 (((nuevo_nombre er e1),sim,(nuevo_nombre er e2)):tn)

------------------------------------------------------------------

--Introduciendo la lista de tuplas de estados renombrados y un estado del tipo Estados, devuelve el nuevo nombre del tipo Estado que se le ha asignado al renombrarlo
nuevo_nombre::EstadosRen->Estados->Estado
nuevo_nombre ((x,s):er) e 
	|(son_iguales x e) = s
	|otherwise = nuevo_nombre er e

------------------------------------------------------------------
--Al estado inicial le llamamos 0 y a los demas estados le asignamos otro numero integer
--Devuelve una lista de los estados relacionados en tuplas, es decir que en una tupla va a estar el mismo estado, en la primera posicion con estados conjuntos y en la segunda posicion renombrado con el tipo Estado
renombrar_estados::EstadosN->Estado->EstadosRen
renombrar_estados x u = renombrar_estados_aux x u [] 1

renombrar_estados_aux::EstadosN->Estado->EstadosRen->Int->EstadosRen
renombrar_estados_aux x u l cont
	|(null x) = l
	|(es_inicial (head x) u) = renombrar_estados_aux (tail x) u (((head x),0):l) cont
	|otherwise = renombrar_estados_aux (tail x) u (((head x),cont):l) (cont+1)
	
------------------------------------------------------------------

--La diferencia con el anterior metodo es que devuelve la lista de estados renombrados directamente, es decir una lista del tipo Estados
estados_renombrados::EstadosN->Estado->Estados
estados_renombrados x u = estados_renombrados_aux x u [] 1

estados_renombrados_aux::EstadosN->Estado->Estados->Int->Estados
estados_renombrados_aux x u s cont
	|(null x) = s
	|(es_inicial (head x) u) = estados_renombrados_aux (tail x) u (0:s) cont
	|otherwise = estados_renombrados_aux (tail x) u (cont:s) (cont+1)
	
-------------------------------------------------------------------

es_inicial::Estados->Estado->Bool
es_inicial x u
	|(null x) = False
	|((head x) == u) && ((length x) == 1) = True
	|otherwise = False


----------------------------------------------------------------------------------------------------------------------------------------------------
	
anadir_transiciones_estado_vacio::Alfabeto->Transiciones2->Transiciones2
anadir_transiciones_estado_vacio a l
	|(null a) = l
	|otherwise = anadir_transiciones_estado_vacio (tail a) (([],(head a),[]):l)
	
----------------------------------------------------------------------------------------------------------------------------------------------------
--Los estados conjuntos tendran doble circulo si alguno de ellos contiene un estado que en el automata original tenia doble circulo.
--Entra una lista de estados conjuntos para saber si alguno de ellos tenia doble circulo en el automata original y en caso de que alguno lo tuviera, devuelve true
doble_circulo::Estados->Estados->Bool
doble_circulo x s 
	|(null x) = False
	|(head x) `elem` s = True
	|otherwise = doble_circulo (tail x) s 

----------------------------------------------------------------------------------------------------------------------------------------------------

--Devuelve los estados que todavia no han sido examinados, si ya lo han sido no los anade a la lista que devuelve
estados_sin_examinar::EstadosN->EstadosN->EstadosN->EstadosN
estados_sin_examinar e s l
	|(null s) = l
	|(esta (head s) e) == True = estados_sin_examinar e (tail s) l
	|otherwise = estados_sin_examinar e (tail s) ((head s):l)

----------------------------------------------------------------------------------------------------------------------------------------------------
	
--Devuelve la lista de los estados que no esten dentro de la lista introducida como parametro
nuevos_estados::Transiciones2->EstadosN->EstadosN->EstadosN
nuevos_estados tr e s
	|(null tr) = s
	|((esta (ter2(head tr)) e) == True) || ((esta (ter2(head tr)) s) == True) = nuevos_estados (tail tr) e s
	|otherwise = nuevos_estados (tail tr) e ((ter2(head tr)):s)

----------------------------------------------------------------------------------------------------------------------------------------------------

--Se utiliza para saber si un estado conjunto esta dentro de la lista
esta::Estados->EstadosN->Bool
esta y x 
	|(null x) = False
	|(son_iguales (head x) y)==True = True
	|otherwise = esta y (tail x) 

----------------------------------------------------------------------------------------------------------------------------------------------------

--Hace una lista de transiciones2 (definido anteriormente) de modo que solo haya una transicion desde un estado para cada letra del alfabeto
juntar_transiciones::Transiciones->Estados->Alfabeto->Transiciones2->Transiciones2
juntar_transiciones z q y l 
	|(null y) = l  
	|otherwise = juntar_transiciones z q (tail y) ((q,(head y),(juntar_estados z q (head y) [])):l)

----------------------------------------------------------------------------------------------------------------------------------------------------

--Introduciendo un conjunto de estados(que en el automata a crear solo es uno), genera la lista de los estados a los que hay transiciones mediante los anteriores	
juntar_estados::Transiciones->Estados->Simbolo->Estados->Estados
juntar_estados z q s l
	|(null q) = sin_repeticion l [] --Por si hay elementos repetidos
	|otherwise = juntar_estados z (tail q) s ((buscar_destinos z (head q) s [])++l)

----------------------------------------------------------------------------------------------------------------------------------------------------

--Devuelve todos los estados a los que se puede llegar con el simbolo introducido desde un estado 	
buscar_destinos::Transiciones->Estado->Simbolo->Estados->Estados
buscar_destinos z q s r
	|(null z) = r
	|((pri(head z))==q) && ((sec(head z))==s) = buscar_destinos (tail z) q s ((ter(head z)):r)
	|otherwise = buscar_destinos (tail z) q s r

-----------------------------------------------------------------------------------------------------------------------------------------------------
--Se utiliza para mirar si dos listas tienen los mismos elementos, aunque sea en distinto orden
--En nuestro caso se utiliza para comprobar que dos estados son los mismos, ya que al transformar a AFD se juntan estados en listas
son_iguales::Estados->Estados->Bool
son_iguales x y 
	|((son_iguales_aux x y) == True) && ((length x) == (length y)) = True
	|otherwise = False

-----------------------------------------------------------------------------------------------------------------------------------------------------
--Devuelve True si todos los elementos de x estan en y
son_iguales_aux::Estados->Estados->Bool
son_iguales_aux x y
	|(null x) = True 
	|(head x) `notElem` y = False
	|(head x) `elem` y = son_iguales_aux (tail x) y

----------------------------------------------------------------------------------------------------------------------------------------------------
--Devuelve una lista sin repeticiones
sin_repeticion::Estados->Estados->Estados
sin_repeticion s r
	|(null s) = r
	|((head s) `elem` r) == True = sin_repeticion (tail s) r
	|otherwise = sin_repeticion (tail s) ((head s):r)

----------------------------------------------------------------------------------------------------------------------------------------------------
--Para elegir el primer elemento de una transicion 
pri::Transicion->Estado
pri(a,b,c) = a

--Para elegir el segundo elemento de una transicion 
sec::Transicion->Simbolo
sec(a,b,c) = b

--Para elegir el tercer elemento de una transicion
ter::Transicion->Estado 
ter (a,b,c) = c

ter2::Transicion2->Estados
ter2 (a,b,c) = c
