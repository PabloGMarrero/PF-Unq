Solución esperada

isValidT (Go d)      = d > 0
isValidT (c1 :#: c2) = isValidT c1 && isValidT c2
isValidT _           = True

compileT c turtle = 
   let (lines, turtle1) = compileTLines c turtle0
    in (LA lines, turtle1)

compileTLines (Go d)      (T pen0 a0 p0) = 
   let p1 = endpoint p0 a0 d
    in ( if hasColour pen0 then [ Line pen0 p0 p1 ] else [ ]
       , T pen0 a0 p1
       )
compileTLines (Turn a)    (T pen0 a0 p0) = ([], T pen0 (a0+a) p0)
compileTLines (Grab pen)  (T _    a0 p0) = ([], T pen  a0     p0)
compileTLines (c1 :#: c2) turtle0        = 
   let (lines1, turtle1) = compileTLines c1 turtle0
       (lines2, turtle2) = compileTLines c2 turtle1
    in (lines1++lines2, turtle2)

hasColour :: Pen -> Bool
hasColour NoColour = False
hasColour _        = True

optimizeT (Go d)      = Go d
optimizeT (Turn a)    = Turn a
optimizeT (Grab pen)  = Grab pen
optimizeT (c1 :#: c2) = optSeqT (optimizeT c1) (optimizeT c2)

optSeqT (Go d1)     c = addGo d1 c
optSeqT (Turn a1)   c = addTurn a1 c
optSeqT (Grab pen)  c = Grab pen :#: c
optSeqT (c1 :#: c2) c = optSeqT c1 (optSeqT c2 c)

addGo d1   (Go d2)         = Go (d1+d2)
addGo d1   (Go d2 :#: c)   = Go (d1+d2) :#: c
addGo d1   c               = Go d1 :#: c

addTurn a1 (Turn a2)       = Turn (a1+a2)
addTurn a1 (Turn a2 :#: c) = Turn (a1+a2) :#: c
addTurn a1 c               = Turn a1 :#: c

PROP: ¿ para todo p :: TCommand . si validT p = True 
           entonces validT (optimizeT p) = True ?

DEM: Sea p un TCommand. Por principio de inducción en la estructura de p.

Hip) ¡ validT p = True !

Caso base 1: p=Go d) 
	¿ validT (optimizeT (Go d)) = True ?

Caso base 2: p=Turn a) 
	¿ validT (optimizeT (Turn a)) = True ?

Caso base 3: p=Grab pen) 
	¿ validT (optimizeT (Grab pen)) = True ?

Caso ind.: p=c1 :#: c2) 
	HI1) ¡ si validT c1 = True entonces validT (optimizeT c1) = True !
	HI2) ¡ si validT c2 = True entonces validT (optimizeT c2) = True !
	TI) ¿ validT (optimizeT (c1 :#: c2)) = True ?

Caso base 1)
	validT (optimizeT (Go d))
 = 			(por optimizeT.1)
	validT (Go d)
= 			(por Hip)
	True

Caso base 2)
	validT (optimizeT (Turn a))
 = 			(por optimizeT.2)
	validT (Turn a)
= 			(por Hip)
	True

Caso base 3)
	validT (optimizeT (Grab pen))
 = 			(por optimizeT.3)
	validT (Grab pen)
= 			(por Hip)
	True

Caso ind.)
	validT (optimizeT (c1 :#: c2))
 = 			(por optimizeT.4)
	validT (optSeqT (optimizeT c1) (optimizeT c2))
 = 			(por Lema 1, HI1 e HI2, donde las hipótesis son implicadas
                  por la hip. general por definición de validT (c1:#:c2))
	True

Lema 1) Si validT c1’ = True y validT c2’ = True 
        entonces validT (optSeqT c1’ c2’) = True