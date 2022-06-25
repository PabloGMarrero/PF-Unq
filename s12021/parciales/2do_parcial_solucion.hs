-- Solución esperada
data Point = P Float Float deriving Show
data Pen = NoColour | Colour Float Float Float deriving Show
type Angle = Float
type Distance = Float
data Turtle = T Pen Angle Point deriving Show
data TCommand = Go Distance | Turn Angle | GrabPen Pen | TCommand :#: TCommand deriving Show -- :#: es un constructor INFIJO

recTC :: (Distance -> b) -> (Angle -> b) -> (Pen -> b) -> (b -> b -> TCommand -> TCommand -> b) -> TCommand -> b
recTC g t gp s (Go d)      = g d
recTC g t gp s (Turn a)    = t a
recTC g t gp s (GrabPen p) = gp p
recTC g t gp s (c1 :#: c2) =
       s (recTC g t gp s c1) (recTC g t gp s c2) c1 c2

foldTC ::(Distance -> b) -> (Angle -> b) -> (Pen -> b) 
      -> (b -> b -> b) -> TCommand -> b
foldTC g t gp s = recTC g t gp (\r1 r2 _ _ -> s r1 r2)

cantSeq = foldTC (const 0) (const 0) (const 0)
                 (\n1 n2 -> 1 + n1 + n2)

cantSeqsALaIzqDeSeq = recTC (const 0) (const 0) (const 0) 
                            (\n1 n2 c1 c2 -> cantSeq c1 + n1 + n2)

assocDer = foldTC Go Turn GrabPen seqDer

seqDer = foldTC (\d c -> Go d :#: c) 
                (\a c -> Turn a :#: c)
                (\p c -> GrabPen p :#: c)
                (\h1 h2 c -> h1 (h2 c))

-- seqDer (c11:#:c12) c2 = seqDer c11 (seqDer c12 c2)
-- seqDer c1          c2 = c1 :#: c2

scaleTC = foldTC (\d n -> Go (d*n)) (\a n -> Turn a) (\p n -> GrabPen p)
                 (\h1 h2 n -> h1 n :#: h2 n)

cantSeqs = foldTC (const (0,0)) (const (0,0)) (const (0,0))
              (\(cs1, csis1) (cs2, csis2) -> (1+cs1+cs2, cs1+csis1+csis2))

-- seqs (c1 :#: c2) = let (cs1, csis1) = seqs c1
--                        (cs2, csis2) = seqs c2
--                     in (1+cs1+cs2, cs1+csis1+csis2)
-- seqs c           = (0,0)
