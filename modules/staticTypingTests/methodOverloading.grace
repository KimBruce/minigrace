dialect "StaticTyping"

type A = {
    num (n: Number) -> Number
    num (n: Number, m: Number) -> Number
    num (s: String) -> Number
}

class a -> A {
    method num (n: Number) -> Number {n}
    method num (n: Number, m: Number) -> Number {n+m}
    method num (s: String) -> Number {47}
}

print(a.num(47))
print(a.num(4,1))
print(a.num("0"))