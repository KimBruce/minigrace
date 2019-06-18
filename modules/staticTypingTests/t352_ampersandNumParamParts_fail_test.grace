dialect "StaticTyping"

type A = {
    part1(b : Boolean) part2(n: Number) -> Boolean
}
type B = {
    part1(b : Boolean) -> Boolean
}
type C = A & B

class ampersandA -> C {
    method part1(b: Boolean) -> Boolean { b }
}

def x : C = ampersandA
