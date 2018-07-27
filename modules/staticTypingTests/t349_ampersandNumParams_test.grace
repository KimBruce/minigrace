dialect "StaticTyping"

type A = {
    a -> Boolean
}

type B = {
    a(bool: Boolean) -> Boolean
}

type C = A & B

class ampersandA -> C {
    method a -> Boolean { true }
    method a(bool : Boolean) -> Boolean { bool }
}

def x: C = ampersandA
print(x.a)
print(x.a(false))
