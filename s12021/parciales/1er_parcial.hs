-- Ejercicio 1
{- 
Definir la función isValidT :: TCommand -> Bool, que determina si un programa de gráficos de tortuga es válido. Las condiciones que deben cumplirse para que sea válido es que no haya comandos Go con valores menores o iguales a 0.

ATENCIÓN: recordar que NO se puede retroceder entre preguntas. Una vez que haya aceptado la entrega de esta pregunta, lo escrito se considerará toda la entrega, sin posibilidad de revisión. Asegurarse de haber resuelto todo a satisfacción ANTES de avanzar.

-}

isValidT :: TCommand -> Bool
isValidT (Go d) = d > 0
isValidT (Turn an) = true
isValidT (GrabPen pen) = true
isValidT (t1 :#: t2) = isValidT t1 && isValidT t2



-- Ejercicio 2
{-Definir la función compileT :: TCommand -> Turtle -> (LineAssembly,Turtle), que compila un programa de gráficos de tortuga válido, en el assembler de líneas (teniendo en cuenta la modificación de estado de la tortuga), de tal forma que ninguna de las líneas que se dibujen en el resultado tenga un cabezal sin color.

Para representar programas en este nuevo lenguaje utilizaremos los siguientes tipos y
funciones:

data Line = Line Pen Point Point
-- Dibuja una línea del 1er punto hasta el 2do, con el color dado

data LineAssembly = LA [Line]

endPoint :: Point -> Angle -> Distance -> Point
-- Calcula el punto final al arrancar el dibujo en el punto dado,
-- apuntando en el ángulo dado, y moverse la distancia dada

AYUDA: para girar, simplemente sumar el ángulo al actual, y para dibujar, usar endPoint.
AYUDA 2: tener en cuenta que la secuencia de comandos requiere modificar el estado de la tortuga entre ambas partes.
AYUDA 3: que las líneas del resultado no se dibujen sin color NO significa que la Tortuga no pueda moverse sin dibujar...

ATENCIÓN: recordar que NO se puede retroceder entre preguntas. Una vez que haya aceptado la entrega de esta pregunta, lo escrito se considerará toda la entrega, sin posibilidad de revisión. Asegurarse de haber resuelto todo a satisfacción ANTES de avanzar.-}


compileT :: TCommand -> Turtle -> (LineAssembly,Turtle)
compileT (Go d) tur         = compileGo d tur
compileT (Turn an) tur      = compileTurn an tur
compileT (GrabPen pen) tur  = compilePen pen tur
compileT (t1 :#: t2) tur    = compilePairJoin (compileT t1 tur) (compileT t2 tur)

compileGo::Distance -> Turtle -> (LineAssembly,Turtle)
compileGo d (T pen angle point) = let p1 = endPoint point angle d 
                                     in (LA [Line pen angle point p1], (T pen angle p1))

compileTurn::Angle -> Turtle -> (LineAssembly,Turtle)
compileTurn an (T pen angle point) = let p1 = endPoint point an 0 
                                        in (LA [Line pen angle point p1], (T pen (an+angle) p1))

compilePen::Pen -> Turtle -> (LineAssembly,Turtle)
compilePen (GrabPen pen) (T pen1 angle point) = let p1 = endPoint point 0 0 
                                                in (LA [Line pen1 angle point p1], (T pen1 angle p1))

compilePairJoin:: (LineAssembly,Turtle) -> (LineAssembly,Turtle) -> (LineAssembly,Turtle)						
compilePairJoin (xs t1) (ys t2) = (LA xs ++ ys, t2)




-- Ejercicio 3
{-Definir, utilizando recursión estructural sobre el tipo del argumento, la función optimizeT :: TCommand -> TCommand, que dado un programa de gráficos de tortuga válido, lo optimiza a otro programa válido equivalente (o sea, con el mismo significado) que no tenga dos o más apariciones seguidas del comando Go ni dos o más apariciones seguidas del comando Turn, y todos las secuencias están asociadas a derecha (o sea, el argumento de la izquierda de una secuencia NO es una secuencia a su vez).
-}

optimizeT :: TCommand -> TCommand
optimizeT (Go d)        = Go d
optimizeT (Turn an)     = Turn an
optimizeT (GrabPen pen) = GrabPen pen
optimizeT (t1 :#: t2)   = optimizett (optimizeT t1) (optimizeT t1)

optimizett :: TCommand -> TCommand -> TCommand
optimizett (Go d1) (Go d2)     = Go (d1+d2)
optimizett (Turn a1) (Turn a2) = Turn (a1+a2)
optimizett (Go d1) t2          = (Go d1) :#: t2
optimizett (Turn a1) t2        = (Turn a1) :#: t2
optimizett (GrabPen pen) t2    = (GrabPen pen) :#: t2
optimizett t1 t2               = t1 :#: t2


-- Ejercicio 4
{- Demostrar que para todo p :: TCommand.
        si  validT p = True
         entonces   validT (optimizeT p) = True

AYUDA: Prestar atención a que esta demostración incluye una Hipótesis, además de una propiedad a afirmar.
-}

Por ppio de inducción en la estructura de p 
- Caso base1 = Go d
	¿validT (optimizeT (Go d)) = True ?

- Caso base2 = Turn an
	¿validT (optimizeT (Turn an)) = True ?
	
- Caso base3 = GrabPen pen
	¿validT (optimizeT (GrabPen pen)) = True ?
	
- Caso ind = (t1 :#: t2)
	TI: ¿validT (optimizeT (t1 :#: t2)) = True ?
	HI1: ¡validT (optimizeT t1) = True !
	HI2: ¡validT (optimizeT t2) = True !
	
Demostración

- Caso base1
	LADO IZQ
	
	validT (optimizeT (Go d))
	= optimizeT.1
	validT (Go d)
	
	Caso validT (Go d) = True
	
		validT (Go d)
		= por hipotesis propuesta
		= True
		
		Llegué
		
	Caso validT (Go d) = False
		validT (Go d)
		= por definición de implicación
		True
		
		Llegué
	
- Caso base2
	LADO IZQ
	
	validT (optimizeT (Turn an))
	= optimizeT.2
	validT (Turn an)
	
	Caso validT (Turn an) = True
	
		validT (Turn an)
		= por hipotesis propuesta
		= True
		
		Llegué
		
	Caso validT (Turn an) = False
		validT (Turn an)
		= por definición de implicación
		True
	
		Llegué
		
- Caso base3
	LADO IZQ

	validT (optimizeT (GrabPen pen)) 
	= optimizeT.3
	validT (GrabPen pen)
	
		Caso validT (GrabPen pen) = True
	
		validT (Turn an)
		= por hipotesis propuesta
		= True
		
		Llegué
		
	Caso validT (GrabPen pen) = False
		validT (GrabPen pen)
		= por definición de implicación
		True
	
		Llegué
		
- Caso ind
	LADO IZQ
	
	validT (optimizeT (t1 :#: t2))
	=optimizeT.3
	validT (optimizett (optimizeT t1) (optimizeT t2))
	= lema validT-optimizeT
	validT (optimizeT t1) && validT (optimizeT t2)
	= HI1
	True && validT (optimizeT t2)
	= HI2
	True && True
	= def (&&)
	True
	
	Llegué
	
	
	Lema validT-optimizeT
		validT (optimizett t1 t2) = validT t1 && validT t2
		
		Caso t1 = Go d1 y t2 = Go d2	
			Caso hipotesis validT p = True
				LADO IZQ
	
				validT (Go d1) && validT (Go d2)
				= por hipotesis propuesta
				True && True
				= def &&
				True
					
				LADO DER
				
				validT (optimizett (Go d1) (Go d2)) 
				= optimizett.1
				validT (Go (d1+d2))
				= por hipotesis propuesta
				True
					
				Llegué
				
			Caso hipotesis validT p = False
				LADO IZQ
	
				validT (Go d1) && validT (Go d2)
				= por implicación, antecedente falso implicación verdadera
				True && True
				= def &&
				True
					
				LADO DER
				
				validT (optimizett (Go d1) (Go d2)) 
				= optimizett.1
				validT (Go (d1+d2))
				= por implicación, antecedente falso implicación verdadera
				True
					
				Llegué

			
		Caso t1 = (Turn a1) y t2 = (Turn a2)	
			Caso hipotesis validT p = True
				LADO IZQ
	
				validT (Turn a1) && validT (Turn a2)
				= por hipotesis propuesta
				True && True
				= def &&
				True
					
				LADO DER
				
				validT (optimizett (Turn a1) (Turn a2)) 
				= optimizett.1
				validT (Turn (a1+a2))
				= por hipotesis propuesta
				True
					
				Llegué
				
			Caso hipotesis validT p = False
				LADO IZQ
	
				validT (Turn a1) && validT (Turn a2)
				= por implicación, antecedente falso implicación verdadera
				True && True
				= def &&
				True
					
				LADO DER
				
				validT (optimizett (Turn a1) (Turn a2)) 
				= optimizett.1
				validT (Turn (a1+a2))
				= por implicación, antecedente falso implicación verdadera
				True
					
				Llegué
				
				
		Caso t1 = (Go d1) y cualquier t2
			Caso hipotesis validT p = True
				LADO IZQ
	
				validT (Go d1) && validT t2
				= por hipotesis propuesta
				True && True
				= def &&
				True
					
				LADO DER
				
				validT (optimizett (Go d1) t2) 
				= optimizett.1
				validT ((Go d1) :#: t2)
				= por hipotesis propuesta
				True
					
				Llegué
				
			Caso hipotesis validT p = False
				LADO IZQ
	
				validT (Go d1)  && validT t2
				= por implicación, antecedente falso implicación verdadera
				True && True
				= def &&
				True
					
				LADO DER
				
				validT (optimizett (Go d1) t2) 
				= optimizett.1
				validT ((Go d1) :#: t2)
				= por implicación, antecedente falso implicación verdadera
				True
					
				Llegué
				
		Caso t1 = (Turn a1) y cualquier t2
			Caso hipotesis validT p = True
				LADO IZQ
	
				validT (Turn a1) && validT t2
				= por hipotesis propuesta
				True && True
				= def &&
				True
					
				LADO DER
				
				validT (optimizett (Turn a1) t2) 
				= optimizett.1
				validT ((Turn a1) :#: t2)
				= por hipotesis propuesta
				True
					
				Llegué
				
			Caso hipotesis validT p = False
				LADO IZQ
	
				validT (Turn a1) && validT t2
				= por implicación, antecedente falso implicación verdadera
				True && True
				= def &&
				True
					
				LADO DER
				
				validT (optimizett (Turn a1) t2) 
				= optimizett.1
				validT ((Turn a1) :#: t2)
				= por implicación, antecedente falso implicación verdadera
				True
					
				Llegué
				
		Caso t1 = (GrabPen pen) y cualquier t2
			Caso hipotesis validT p = True
				LADO IZQ
	
				validT (GrabPen pen) && validT t2
				= por hipotesis propuesta
				True && True
				= def &&
				True
					
				LADO DER
				
				validT (optimizett (GrabPen pen) t2) 
				= optimizett.1
				validT ((GrabPen pen) :#: t2)
				= por hipotesis propuesta
				True
					
				Llegué
				
			Caso hipotesis validT p = False
				LADO IZQ
	
				validT (GrabPen pen) && validT t2
				= por implicación, antecedente falso implicación verdadera
				True && True
				= def &&
				True
					
				LADO DER
				
				validT (optimizett (GrabPen pen) t2) 
				= optimizett.1
				validT ((GrabPen pen) :#: t2)
				= por implicación, antecedente falso implicación verdadera
				True
					
				Llegué
				
		Caso cualquier t1 y cualquier t2 
			Caso hipotesis validT p = True
				LADO IZQ
	
				validT t1 && validT t2
				= por hipotesis propuesta
				True && True
				= def &&
				True
					
				LADO DER
				
				validT (optimizett t1 t2) 
				= optimizett.1
				validT (t1 :#: t2)
				= por hipotesis propuesta
				True
					
				Llegué
				
			Caso hipotesis validT p = False
				LADO IZQ
	
				validT t1 && validT t2
				= por implicación, antecedente falso implicación verdadera
				True && True
				= def &&
				True
					
				LADO DER
				
				validT (optimizett t1 t2) 
				= optimizett.1
				validT (t1 :#: t2)
				= por implicación, antecedente falso implicación verdadera
				True
					
				Llegué

	


