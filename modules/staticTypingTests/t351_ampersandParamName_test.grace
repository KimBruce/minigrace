dialect "StaticTyping"
//Tests that the names given to parameters in a method do not affect those
//methods' compatibility to be ampersanded together.

type A = {
    a(name1: String) -> String
}
type B = {
    a(name2: String) -> String
}
type C = A & B

class ampersandA -> C {
    method a (anyName: String) -> String { anyName }
}

def x : C = ampersandA
print (x.a("test succeeded"))
