module Clasificacion where
import Data.List
import Pruebas
import Tipos

--Pruebas:
--Clasificacion Af
---------Afd
------Clasificacion automata D1 -> esperado:Afd->resultado= Afd
------Clasificacion automata D2 -> esperado:Afd->resultado= Afd
------Clasificacion automata D3 -> esperado:Afd->resultado= Afd
------Clasificacion automata D3s -> esperado:Afd->resultado= Afd
------Clasificacion automata N7a -> esperado:Afd->resultado= Afd
------Clasificacion automata N6a -> esperado:Afd->resultado= Afd
---------Afnd
------Clasificacion automata N1 -> esperado:Afnd->resultado= Afnd
------Clasificacion automata N6 -> esperado:Afnd->resultado= Afnd
------Clasificacion automata N7 -> esperado:Afnd->resultado= Afnd
------Clasificacion automata N4 -> esperado:Afnd->resultado= Afnd

--Simplificacion AFD
--- Simplificación D3: resultado esperado:([0,1,2,3,4,9],['a','b','c'],[(0,'a',1),(0,'b',0),(0,'c',4),(1,'a',2),(1,'b',0),(1,'c',4),(2,'a',3),(2,'b',0 ),(2,'c',4),(3,'a',3),(3,'b',3),(3,'c',4),(4,'a',9),(4,'b',9),(4,'c',4 ),(9,'a',4),(9,'b',4),(9,'c',9)],0,[1,2,3])
		--   resultado obtenido:([0,1,2,3,4,9],"abc",[(0,'a',1),(0,'b',0),(0,'c',4),(1,'a',2),(1,'b',0),(1,'c',4),(2,'a',3),(2,'b',0),(2,'c',4),(3,'a',3),(3,'b',3),(3,'c',4),(4,'a',9),(4,'b',9),(4,'c',4),(9,'a',4),(9,'b',4),(9,'c',9)],0,[1,2,3])

-----Simplificacion AFND
--- Simplificación N3: resultado esperado:([0,3,4,5,8],['a','b','c'],[(0,'a',0),(0,'a',3),(0,'a',8),(0,'b',0),(3,'a',4),(3,'a',8),(4,'a',5),(4,'a',8 ),(5,'a',5),(5,'b',5)],0,[5,8])
		--   resultado obtenido:([0,3,4,5,8],"abc",[(0,'a',0),(0,'a',3),(0,'a',8),(0,'b',0),(3,'a',4),(3,'a',8),(4,'a',5),(4,'a',8),(5,'a',5),(5,'b',5)],0,[5,8])



-- haciendo uso de otras funciones decide si el Af es AFD O AFND y escribe un mensaje por pantalla
-- coste: tiene el mismo coste que su función auxiliar:e(número de estados del autómata)* n(número de letras del alfabeto)*m(número de símbolos con los que tiene transiciones un estado. En el peor de los casos=n)
determinista:: Af->Bool
determinista (est,alf,tr,ini,dob)
	|(clasificacionAf_aux (est,alf,tr,(head est),dob) est ) == True = True
	|otherwise = False
	
--decide si un Af es un AFD en el caso de que lo sea devuelve true en caso contrario devuelve false (en caso de false el AF sería un AFND)
--coste:por cada uno de los estados del autómata llama a la función mirarSiAfd cuyo coste es n*m siendo : n(número de letras del alfabeto) y m(número de símbolos con los que tiene transiciones un estado). Al llamar a esta función con cada uno de los estados del autómata el coste en el peor de los casos es e(número de estados del autómata)* n(número de letras del alfabeto)*m(número de símbolos con los que tiene transiciones un estado. En el peor de los casos=n)
clasificacionAf_aux:: Af->Estados-> Bool
clasificacionAf_aux (est,alf,tr,ini,dob) []= True
clasificacionAf_aux (est,alf,tr,ini,dob) estPorMirar 
	|((mirarSiAfd tr ini est alf)== True) = clasificacionAf_aux (est,alf,tr,((head(tail(estPorMirar)))) ,dob) (tail estPorMirar )
	|otherwise=False

-- La funcion mirarSiAfd mira si el estado actual tiene transiciones con todas las letras hacia los demás estados
--coste: en el peor de los casos mira por cada estado los simbolos que tiene en la lista de transiciones por lo que tendrá que recorrer la lista de transiciones entera, posteriormente llama a la función vistasTodasLetras con el alfabeto del autómata y los símbolos almacenados en la lista. Por lo tanto su coste será t(número de transiciones)+ (n(número de letras del alfabeto)*m(número de símbolos almacenados en la lista letrasVistas)). En general coste n*m.
mirarSiAfd:: Transiciones->Estado->Estados->Alfabeto->Bool 
mirarSiAfd listaTrans estAct listaTodosEstados alfab = mirarSiAfd_aux listaTrans estAct listaTodosEstados alfab []
	
mirarSiAfd_aux:: Transiciones->Estado->Estados->Alfabeto->Alfabeto->Bool
mirarSiAfd_aux [] estAct listaTodosEstados alfab letrasVistas
	|(vistasTodasLetras alfab letrasVistas)==True =True
	|otherwise=False
mirarSiAfd_aux ((est1, sim, est2):trs) estAct listaEstados alfab letrasVistas
	|(est1==estAct)=mirarSiAfd_aux trs estAct listaEstados alfab (sim:letrasVistas)--mira por cada estado en la lista de transiciones cuales son las letras con las que tiene transiciones el estado y las guarda en una lista.
	|otherwise= mirarSiAfd_aux trs estAct listaEstados alfab letrasVistas

--vistasTodasLetras mira las letras de las transiciones desde un estado del autómata y comprueba que sean todas las del alfabeto
--tiene en cuenta que las letras pueden estar repetidas
--coste: en el peor de los casos n+m (coste de la funcion length)+ n*m siendo n el número de letras del primer alfabeto y m el número de letras del segundo alfabeto que recibe como parámetros. Se puede decir que el coste es n*m.
vistasTodasLetras:: Alfabeto->Alfabeto->Bool
vistasTodasLetras (sim:alf) letrasVistas
	|(length (sim:alf)) /= (length letrasVistas) = False
	|otherwise = vistasTodasLetras_aux alf letrasVistas

--coste:en el peor de los casos n*m siendo n el número de letras del primer alfabeto y m el número de letras del segundo
vistasTodasLetras_aux:: Alfabeto->Alfabeto->Bool
vistasTodasLetras_aux [] letrasVistas=True
vistasTodasLetras_aux (sim:alf) letrasVistas
	|((elem) sim letrasVistas)==True = vistasTodasLetras_aux alf letrasVistas
	|otherwise=False


--devuelve todos los estados alcanzables desde un estado,es decir, todos aquellos estados a los que se puede llegar pasando por los estados a los que se tienen transiciones
--coste: coste n² siendo n el número de elementos de la lista que da como resultado. Esto se debe a que realiza dos llamadas a alcanzables_aux2 .
alcanzables :: Af -> Estado -> Estados
alcanzables (est,alf,tr,ini,dob) estado = alcanzables_aux2 (est,alf,tr,ini,dob) (alcanzables_aux2 (est,alf,tr,ini,dob) (alcanzables_aux (est,alf,tr,ini,dob) estado tr))


-- devuelve los estados alcanzables directamente de la lista estados que recibe como parámetro
--coste: cuadrático respecto a la lista de estados que da como resultado
alcanzables_aux2:: Af->Estados->Estados
alcanzables_aux2 (est,alf,tr,ini,dob) []=[]
alcanzables_aux2 (est,alf,tr,ini,dob) alcanzables= (alcanzables_aux (est,alf,tr,ini,dob) (head alcanzables) tr)++ alcanzables_aux2 (est,alf,tr,ini,dob) (tail alcanzables)
	
-- devuelve los estados alcanzables mediante una transicion, es decir aquellos a los que se llega directamente desde un estado
--coste: lineal O(n) siendo n el número de elementos de la lista de transiciones
alcanzables_aux::Af-> Estado->Transiciones->Estados
alcanzables_aux (est,alf,tr,ini,dob) estado []=[]
alcanzables_aux (est,alf,tr,ini,dob) estado ((est1, sim, est2):trs)
	|(est1==estado)=est2: alcanzables_aux (est,alf,tr,ini,dob) estado trs
	|otherwise= alcanzables_aux (est,alf,tr,ini,dob) estado trs

--decide que estados de un AF llevan doble círculo o tienen estados de doble círculo en su lista de alcanzables
--coste: (mismo para aceptacion y aceptacion_aux) mira si hay estados de doble círculo en los estados alcanzables de cada uno de los estados del autómata. Por lo tanto el coste es: e (número de estados del autómata)*n^2(coste de la funcion alcanzables n es el número de elementos de esta lista)*m(número de estados de doble círculo). Se podría decir que su coste es cuadrático.
aceptacion:: Af-> Estados
aceptacion (est,alf,tr,ini,dob) = aceptacion_aux (est,alf,tr,ini,dob) (tail(est)) (head(est)) dob

aceptacion_aux:: Af->Estados->Estado->Estados->Estados
aceptacion_aux (est,alf,tr,ini,dob) [] estadoAct estadosDoble
	|dobleCirculo (estadoAct: (alcanzables (est,alf,tr,ini,dob) (estadoAct))) (estadosDoble)= estadoAct:[]
	|otherwise=[]
aceptacion_aux (est,alf,tr,ini,dob) listaEstados estadoAct estadosDoble
	|dobleCirculo (estadoAct: (alcanzables (est,alf,tr,ini,dob) (estadoAct))) (estadosDoble)==True = estadoAct: aceptacion_aux (est,alf,tr,ini,dob) (tail listaEstados) (head listaEstados) estadosDoble
	|otherwise=aceptacion_aux (est,alf,tr,ini,dob) (tail listaEstados) (head listaEstados) estadosDoble

--dada una lista de estados y la lista de doble círculo decide si en la lista recibida hay algun estado de doble círculo
--coste:n*m siendo n el número de estados de la primera lista de estados que recibe como parámetro y m el número de estados de la segunda lista que recibe como parámetro.
dobleCirculo:: Estados->Estados->Bool
dobleCirculo [] estadosDoble = False
dobleCirculo listaAlcanzables estadosDoble
	|(elem) (head listaAlcanzables) estadosDoble = True
	|otherwise= dobleCirculo (tail listaAlcanzables) estadosDoble

--dado un Af devuelve el Af simplificado
--coste: cuadrático ,ya que, tanto la funcion de simplificacion de Afnd como la del Afd son de coste cuadrático
simplificacion:: Af->Af
simplificacion (est,alf,tr,ini,dob)
	|(clasificacionAf_aux (est,alf,tr,ini,dob) est)== True = simplificacion_Afd (est,alf,tr,ini,dob) (alcanzables (est,alf,tr,ini,dob) ini) 
	|otherwise=simplificacion_Afnd (est,alf,tr,ini,dob) (alcanzables (est,alf,tr,ini,dob) ini)
--dado un Afnd devuelve el Afnd simplificado
--coste: cuadrático ya que llama a la función simplificación_Afd de coste cuadrático y a aceptación cuyo coste también es cuadrático.
simplificacion_Afnd:: Af->Estados->Af
simplificacion_Afnd (est,alf,tr,ini,dob) []= simplificacion_Afnd_aux (est,alf,tr,ini,dob) (aceptacion (est,alf,tr,ini,dob))
simplificacion_Afnd (est,alf,tr,ini,dob) alcanzables=  simplificacion_Afnd_aux (simplificacion_Afd(est,alf,tr,ini,dob) alcanzables) (aceptacion (est,alf,tr,ini,dob))

simplificacion_Afnd_aux:: Af->Estados->Af
simplificacion_Afnd_aux (est,alf,tr,ini,dob) aceptacion = ((eliminarEstados (sobranEstados est aceptacion) est),alf,(eliminarTransiciones (sobranEstados est aceptacion) tr),ini,(eliminarEstados (sobranEstados est aceptacion) dob))



--dado un Afd devuelve el Afd simplificado
--coste: tiene el coste de eliminarEstados*sobranEstados + coste eliminarTransiciones*sobranEstados + coste eliminarEstados*sobranEstados, es decir, coste cuadrático.
simplificacion_Afd:: Af->Estados->Af
simplificacion_Afd (est,alf,tr,ini,dob) alcanzables 
	|(sobranEstados est alcanzables)==[] = (est,alf,tr,ini,dob)
	|otherwise= ((eliminarEstados (sobranEstados est alcanzables) est),alf,(eliminarTransiciones (sobranEstados est alcanzables) tr),ini,(eliminarEstados (sobranEstados est alcanzables) dob))

--dada la lista de aceptacion devuelve los estados que no se encuentran en la lista de aceptacion/alcanzables
--coste: n*m siendo n el número de estados de la primera lista y m el número de estados de la segunda lista
sobranEstados:: Estados->Estados->Estados
sobranEstados [] alcanzables=[]
sobranEstados est alcanzables
	|((elem) (head est) alcanzables)==True= sobranEstados (tail est) alcanzables
	|otherwise= (head est):sobranEstados (tail est) alcanzables

--dada una lista de estados y una de transiciones elimina las transiciones desde y hacia los estados de la lista
--coste:n*m siendo n el número de transiciones de la primera lista y m el número de transiciones de la segunda lista
eliminarTransiciones:: Estados->Transiciones->Transiciones
eliminarTransiciones [] transiciones = transiciones
eliminarTransiciones estadosSobra transiciones= eliminarTransiciones (tail estadosSobra) (eliminarTransicion (head estadosSobra) transiciones)

--dado un estado y una lista de transiciones elimina las transiciones desde y hacia el estados indicado
--coste:lineal respecto al número de elementos de la lista de transiciones
eliminarTransicion:: Estado->Transiciones->Transiciones
eliminarTransicion estado [] = []
eliminarTransicion  estado ((est1, sim, est2):trs)
	|(est1==estado)|| (est2==estado)= eliminarTransicion estado trs
	|otherwise=(est1, sim, est2):eliminarTransicion estado trs

--dadas dos listas de estados elimina los estados que hay en la primera lista de la segunda
--coste: n*m siendo n el número de estados de la primera lista y m el número de estados de la segunda lista
eliminarEstados:: Estados->Estados->Estados
eliminarEstados [] estados = estados
eliminarEstados estadosBorrar estados= eliminarEstados (tail estadosBorrar) (eliminarEstado (head(estadosBorrar)) estados)
--dado un estado y una lista de estados elimina el estado de la lista de estados
--coste: lineal respecto al número de elementos de la lista estados
eliminarEstado:: Estado->Estados->Estados
eliminarEstado estado []= []
eliminarEstado estado estados
	|estado== (head estados) = tail estados
	|otherwise= (head estados): (eliminarEstado estado (tail estados))


