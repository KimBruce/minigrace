dialect "StaticTyping"

type A = interface { m -> String }
type B = A & interface { q(_:Number)→String }

class b → B {
    method m → String { "Hello " }
    method q(_:Number) → String { "World" }
}

method r -> A & interface { q(_:Number)→String } { b }

def x : B = r
print (x.m ++ x.q(47))
