dialect "StaticTyping"

type A = {
    m(n: Number) → Number
}

class a(i: Number) → A {
    method m(n: Number) → Number {
        n+i
    }
    def x: Number = 13
}

class b → A {
    inherit a(47)
    def y: Number = m(x)
}

print (b.m(3))
2 + "A"