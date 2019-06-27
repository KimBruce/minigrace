dialect "StaticTyping"

type A = { m -> String }

type B = A & interface {
    n -> B
}

class b -> B {
   method m -> String { "hello" }
   method n -> B { self }
}

print(b.n.m)