dialect "StaticTyping"


type A = {
    id(n: Number) → Number
}

class a(m: Number) → A {
    method id(n: Number) → Number {n + m}
}
def b: A = object  {
    inherit a(3)
}
print(b.id(12))
