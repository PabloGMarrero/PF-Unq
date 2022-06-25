--1) Dar el tipo y escribir el esquema de recursión primitiva de TCommand.
--2) Dar el tipo y escribir la función que expresa el esquema de recursión estructural sobre TCommand, sin utilizar recursión explícita.
data Point = P Float Float deriving Show
data Pen = NoColour | Colour Float Float Float deriving Show
type Angle = Float
type Distance = Float
data Turtle = T Pen Angle Point deriving Show
data TCommand = Go Distance | Turn Angle | GrabPen Pen | TCommand :#: TCommand deriving Show -- :#: es un constructor INFIJO


foldTC :: (Distance -> b) -> (Angle -> b) -> (Pen -> b) -> (b -> b -> b) -> TCommand -> b
foldTC fg ft fp fc (Go d)      = fg d
foldTC fg ft fp fc (Turn a)    = ft a
foldTC fg ft fp fc (GrabPen p) = fp p
foldTC fg ft fp fc (t1 :#: t2) = fc (foldTC fg ft fp fc t1) (foldTC fg ft fp fc t2)

recTC :: (Distance -> b) -> (Angle -> b) -> (Pen -> b) -> (TCommand -> b -> TCommand -> b -> b) -> TCommand -> b
recTC fg ft fp fc (Go d)      = fg d
recTC fg ft fp fc (Turn a)    = ft a
recTC fg ft fp fc (GrabPen p) = fp p
recTC fg ft fp fc (t1 :#: t2) = fc t1 (recTC fg ft fp fc t1) t2 (recTC fg ft fp fc t2)

triangle = Go 30 :#: Turn 120 :#: Go 30 :#: Turn 120 :#: Go 30 :#: Turn 120

--3) Definir sin usar recursión explícita la función cantSeq :: TCommand -> Int, que dado un programa de gráficos de tortuga, describe la cantidad de constructores (:#:) del mismo.

cantSeq :: TCommand -> Int
cantSeq = foldTC (const 0) (const 0) (const 0) j
 where j t1 t2 = 1 + t1 + t2 
 
--4) Definir sin usar recursión explícita la función cantSeqsALaIzqDeSeq :: TCommand -> Int, que dado un programa de gráficos de tortuga, describe la cantidad total de constructores (:#:) del mismo que están en algún argumento izquierdo de un (:#:).

cantSeqsALaIzqDeSeqBis :: TCommand -> Int
cantSeqsALaIzqDeSeqBis (Go d)      = 0
cantSeqsALaIzqDeSeqBis (Turn a)    = 0
cantSeqsALaIzqDeSeqBis (GrabPen p) = 0
cantSeqsALaIzqDeSeqBis (t1 :#: t2) = if esSeq t1 then 1 else 0 + cantSeqsALaIzqDeSeqBis t1 + cantSeqsALaIzqDeSeqBis t2

cantSeqsALaIzqDeSeq :: TCommand -> Int
cantSeqsALaIzqDeSeq = recTC (const 0) (const 0) (const 0) j
 where j t1 rt1 t2 rt2 = if esSeq t1 then 1 else 0 + rt1
 
esSeq :: TCommand -> Bool	
esSeq (t1 :#: t2) = True
esSeq t           = False


-- 5) Definir sin usar recursión explícita la función assocDer :: TCommand -> TCommand, que dado un programa de gráficos de tortuga, describe uno equivalente que no tiene argumentos a la izquierda de un (:#:) que estén formados por (:#:).

--SUGERENCIA: Considerar definir una función auxiliar para el caso inductivo.

assocDerBis :: TCommand -> TCommand
assocDerBis (Go d)      = (Go d)
assocDerBis (Turn a)    = (Turn a)
assocDerBis (GrabPen p) = (GrabPen p)
assocDerBis (t1 :#: t2) = if esSeq t1 then assocDerBis t2 else assocDerBis t1 :#: assocDerBis t2

assocDer :: TCommand -> TCommand
assocDer = recTC Go Turn GrabPen j
 where j t1 rt1 t2 rt2 = if esSeq t1 then rt2 else rt1 :#: rt2
 
-- 6) Definir la función scaleTC :: TCommand -> Float -> TCommand, expresada como recursión estructural implícita sobre TCommand. Esta función escala cada comando Go de su argumento según un factor dado. Por ejemplo, scaleTC(Go 10 :#: Go 30) 2 = (Go 20 :#: Go 60). 

scaleTC :: TCommand -> Float -> TCommand
scaleTC = foldTC g h i j 
 where g d = (\n -> Go (d * n))
       h a = (\n -> Turn a)
       i p = (\n -> GrabPen p)
       j t1 t2 = (\n -> t1 n :#: t2 n)
	   
-- 7) Dar una versión de la función cantSeqs dada a continuación, sin utilizar recursión explicita
{-
cantSeqs (c1 :#: c2) = let (cs1, csis1) = cantSeqs c1
                           (cs2, csis2) = cantSeqs c2
                        in (1+cs1+cs2, cs1+csis1+csis2)
cantSeqs c           = (0,0)
-}

cantSeqs :: TCommand -> (Int, Int)
cantSeqs = foldTC (const (0,0)) (const (0,0)) (const (0,0)) j
 where j (cs1, csis1) (cs2, csis2) = (1+cs1+cs2, cs1+csis1+csis2)
 
-- 8) Demostrar que la versión dada de cantSeqs en el ejercicio anterior, efectivamente lo es.
{-
¿cantSeqs' = cantSeqs?

Por ppio de extensionalidad sea c un TCommand cualquiera

	¿cantSeqs' c = cantSeqs c?

Por ppio de inducción en la estrucura de c
- Caso base1 c = Go d
	¿cantSeqs' (Go d) = cantSeqs (Go d)?
- Caso base2 c = Turn a
	¿cantSeqs' (Turn a) = cantSeqs (Turn a)?
- Caso base3 c = GrabPen p
	¿cantSeqs' (GrabPen p) = cantSeqs (GrabPen p)?
- Caso ind c = (c1 :#: c2)
	TI : ¿cantSeqs' (c1 :#: c2) = cantSeqs (c1 :#: c2)?
	HI1: !cantSeqs' c1         = cantSeqs c1!
	HI2: !cantSeqs' c2         = cantSeqs c2!
	
Demostración

- Caso base 1
	Lado izquierdo

	cantSeqs' (Go d) 
	= cantSeqs'.2
	(0, 0)
	
	Lado derecho
	
	cantSeqs (Go d)
	= def cantSeqs
	foldTC (const (0,0)) (const (0,0)) (const (0,0)) j (Go d)
	= foldTC.1
	const (0,0) d
	= def const
	(0,0)
	
	LLEGUÉ

- Caso base 2
	Lado izquierdo

	cantSeqs' (Turn a) 
	= cantSeqs'.2
	(0, 0)
	
	Lado derecho
	
	cantSeqs (Turn a)
	= def cantSeqs
	foldTC (const (0,0)) (const (0,0)) (const (0,0)) j (Turn a)
	= foldTC.2
	const (0,0) a
	= def const
	(0,0)
	
	LLEGUÉ
	
- Caso base 3
	Lado izquierdo

	cantSeqs' (GrabPen p) 
	= cantSeqs'.2
	(0, 0)
	
	Lado derecho
	
	cantSeqs (GrabPen p)
	= def cantSeqs
	foldTC (const (0,0)) (const (0,0)) (const (0,0)) j (GrabPen p)
	= foldTC.3
	const (0,0) p
	= def const
	(0,0)
	
	LLEGUÉ

- Caso ind c 
	Lado izquierdo
	
	cantSeqs' (c1 :#: c2) 
	= cantSeqs'.1
	let (cs1, csis1) = cantSeqs' c1
        (cs2, csis2) = cantSeqs' c2
	in (1+cs1+cs2, cs1+csis1+csis2)
	= def let in
	(1+cs1+cs2, cs1+csis1+csis2)
	
	
	Lado derecho
	
	cantSeqs (c1 :#: c2)
	= def cantSeqs
	foldTC (const (0,0)) (const (0,0)) (const (0,0)) j (c1 :#: c2)
	= foldTC.4
	j (foldTC (const (0,0)) (const (0,0)) (const (0,0)) c1) 
	  (foldTC (const (0,0)) (const (0,0)) (const (0,0)) c2)
	= def cantSeqs
	j (cantSeqs c1)
	   (foldTC (const (0,0)) (const (0,0)) (const (0,0)) c2)
	= def cantSeqs
	j (cantSeqs c1) (cantSeqs c2)
	= HI1
	j (cantSeqs' c1) (cantSeqs c2)
	= HI2
	j (cantSeqs' c1) (cantSeqs' c2)
	= def j
	(1+cs1+cs2, cs1+csis1+csis2)
	
-}