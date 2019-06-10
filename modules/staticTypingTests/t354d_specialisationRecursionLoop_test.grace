dialect "StaticTyping"

type A = {
    c → String
}

type B = A & interface {
    m -> B
    n -> String
} 

type C = A & B


class b → B {
    method m → B {self}
    method n → String {"Hello World"}
    method c -> String {"Bye"}
}

class e -> C {
    method m → B {self}
    method n → String {"Hello World"}
    method c -> String {"Bye"}
}

print (e.m.c)