dialect "StaticTyping"

type A = {
    c → String
}

type B = A & interface {
    m -> B
    n -> String
} 
type D =  interface {
    b -> String 
    
} 
type E = A & interface {
    f -> Number
} 

type C = (A|E) & D & B


class b → B {
    method m → B {self}
    method n → String {"Hello World"}
    method c -> String {"Bye"}
}

class e -> C {
    method m → B {self}
    method n → String {"Hello World"}
    method c -> String {"Bye"}
    method b -> String {"hey"} 
}

print (e.m.c)