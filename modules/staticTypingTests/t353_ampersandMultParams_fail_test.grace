dialect "StaticTyping"

type A = {
    a(b : Boolean, n: Number) -> Boolean
}
type B = {
    a(b : Boolean)-> Boolean
}
type C = A & B

//This class raises an error because it needs to have a second method a
//that would take a second parameter in order for the body of the class
//to be a subtype of A & B.
class ampersandA -> C {
    method a(b: Boolean) -> Boolean { b }
}

def x : C = ampersandA
