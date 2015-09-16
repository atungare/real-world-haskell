-- file: ch03/ShapeUnion.hs
type Vector = (Double, Double)

type Center = Vector
type Radius = Double

data Shape = Circle Center Radius
           | Poly [Vector]