dialect "StaticTyping"

type A = {
    m(b:B) → A
    p → String
}

type B = A & interface {
    m(b:A) -> B
    n -> String
}

class b -> B {
    method m(b':A) -> B { self }
    method n -> String { "Hello" }
    method p → String {"There"}
}

def d:B = b.m(b)
print (d.p)

print(d.n)
print (b.m(b).n)