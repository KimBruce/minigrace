dialect "StaticTyping"

type B' =  interface {
    m -> B'
    n -> String
    c -> A 
}

type A = {
    d -> B'
}

class b → B' {
    method m → B' {self}
    method n → String {"Hello World"}
    method c -> A {a}
}

class a -> A {
    method d -> B' {b}
}

print (b.m.m.c.d.n)