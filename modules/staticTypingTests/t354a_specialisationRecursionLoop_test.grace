dialect "StaticTyping"

type A = {
    m → A
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
}

class c → C {
    method m → C {self}
    method n → String {"Goodbye World"}
}

print (b.m.n)
print (c.m.n)
