dialect "StaticTyping"

type A = {
    c → Number
}

type B = A & interface {
    m -> B
    n -> String
}

class b → B {
    method m → B {self}
    method n → String {"Hello World"}
    method c -> Number { 47 }
}

print (b.m.c)