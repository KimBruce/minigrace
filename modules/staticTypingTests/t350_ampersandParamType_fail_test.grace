dialect "StaticTyping"

type A = {
    a(b : Boolean) -> Boolean
}
type B = {
    a(n : Number)-> Boolean
}
type C = A & B

class ampersandA -> C {
    method a(b: Boolean) -> Boolean { b }
}

def x : C = ampersandA
