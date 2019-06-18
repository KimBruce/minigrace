dialect "StaticTyping"

type A = {
    m → A
}

type B = A & interface {
    m -> B
    n -> String
}

type T = B & interface {
    m -> T
    p -> Number
}

class t -> T {
    method m -> T { self }
    method p -> Number { 47 }
    method n -> String {"Hello World"}
}

print (t.m.p)