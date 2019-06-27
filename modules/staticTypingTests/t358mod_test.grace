dialect "StaticTyping"

type A = {
    m → A
}
type C = {
    m -> B
    n -> String
    r -> A
}
type B = A & C 
class b -> B {
    method m -> B { self }
    method n -> String { "Hello" }
    method r -> A { self }
}

def d: B = b.m

print(d.n)
print(b.m.n)