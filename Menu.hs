import Transformacion
import Pertenencia
import Tipos
import Clasificacion


menu::IO()
menu = do
	putStrLn "***Menu principal***"
	putStrLn "Elige una opcion:"
	putStrLn "1. Pertenencia de una palabra al lenguaje asociado a un AF."
	putStrLn "2. Clasificacion de un AF como AFD o AFND."
	putStrLn "3. Simplificacion de un AF."
	putStrLn "4. Transformacion de un AF en AFD."
	putStrLn "5. Terminar"
	n<-getLine
	let num = (read n :: Integer)
	decidir_opcion_menu num
	

decidir_opcion_menu::Integer->IO()
decidir_opcion_menu n 
	|n==1 = opcion_1
	|n==2 = opcion_2
	|n==3 = opcion_3
	|n==4 = opcion_4
	|n==5 = putStrLn"Programa finalizado."
	|otherwise = do
		putStrLn "Opcion no valida, vuelva a intentarlo"
		menu
		
submenu_pertenencia::IO()
submenu_pertenencia = do
	putStrLn "***Submenu de pertenencia***"
	putStrLn "Elige una opcion:"
	putStrLn "1. Comprobar la pertenencia de otra palabra al lenguaje asociado al AF."
	putStrLn "2. Volver al menu principal"
	n<-getLine
	let num = (read n :: Integer)
	decidir_opcion_submenu num
	
decidir_opcion_submenu::Integer->IO()
decidir_opcion_submenu n 
	|n==1 = opcion_1
	|n==2 = menu 
	|otherwise = do
		putStrLn "Opcion no valida, vuelva a intentarlo"
		submenu_pertenencia
		
opcion_1::IO()
opcion_1 = do
	putStrLn"Introduzca el Af"
	x<-getLine
	let af = (read x :: Af)
	putStrLn"Introduzca la palabra"
	y<-getLine
	let p = (read y :: Palabra)
	putStrLn(show (pertenencia af p))
	putStrLn""
	submenu_pertenencia
	
opcion_2::IO()
opcion_2 = do
	putStrLn"Introduzca el Af"
	x<-getLine
	let af = (read x :: Af)
	putStrLn(show (determinista af))
	putStrLn""
	menu
	
opcion_3::IO()
opcion_3 = do 
	putStrLn"Introduzca el Af"
	x<-getLine
	let af = (read x :: Af)
	putStrLn(show(simplificacion af))
	putStrLn""
	menu
	
opcion_4::IO()
opcion_4 = do 
	putStrLn"Introduzca el Af"
	x<-getLine
	let af = (read x :: Af)
	if ((determinista af) == False)
	then
		putStrLn(show(transformacion af))
	else
		putStrLn(show(af))
	putStrLn""
	menu
	
	
	
	
