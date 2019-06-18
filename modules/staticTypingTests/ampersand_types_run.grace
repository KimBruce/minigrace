dialect "StaticTyping"

type Point = {
    x -> Number
}

type ColorPoint = Point & interface {
    color -> String
    m -> ColorPoint
}

type FatPoint = Point & interface {
    radius -> Number
    m -> FatPoint
}

type Combo = ColorPoint & FatPoint

class combo -> Combo {
    method color -> String { "Color" }
    method x -> Number { 47 }
    method radius -> Number { 0 }
    method m -> FatPoint { fat }
}

class fat -> FatPoint {
    method radius -> Number { 1 }
    method m -> FatPoint { fat }
    method x -> Number { 2 }
    method color -> String { "Color" }
}

def d: Combo = combo

print (combo.m.color)
print (combo.m.radius)