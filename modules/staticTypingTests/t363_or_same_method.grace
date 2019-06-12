dialect "StaticTyping"

type A = {
    k → String
}

type B = A & interface {
    m -> B
    n -> String
}

type D =  interface {
    m -> String   
} 

type C = B|D

class c -> C {
    method m → B {self}
    method n → String {"Hello World"}
    method k -> String {"Bye"}
}

print (c.m.k)