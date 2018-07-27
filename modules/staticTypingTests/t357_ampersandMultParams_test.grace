dialect "StaticTyping"

type A = {
    a(b : Boolean, n: Number) -> Boolean
}
type B = {
    a(b : Boolean)-> Boolean
}
type C = A & B

class ampersandA -> C {
    method a(b: Boolean) -> Boolean { b }
    method a(b: Boolean, n: Number) -> Boolean { b.not }
}

def x : C = ampersandA

print (x.a(true))
print (x.a(true, 47))
