dialect "StaticTyping"

type A = {
    a -> Boolean
}

type B = {
    a -> Number
}

class ampersandA -> A & B {
    method a -> Boolean { true }
}

def x : A & B = ampersandA
