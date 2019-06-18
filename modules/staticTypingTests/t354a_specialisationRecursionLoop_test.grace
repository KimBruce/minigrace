dialect "StaticTyping"

type A = {
    a → Number 
}

type B = A & interface {
    m -> B
    n -> String
}

type C = interface {
    m → B
    n → String
} & A

class b → B {
    method m → B {self}
    method n → String {"Hello World"}
    method a -> Number {47}
}

class c → C {
    method m → C {self}
    method n → String {"Goodbye World"}
    method a -> Number {47}
}

print (b.m.n)
print (c.m.n)
