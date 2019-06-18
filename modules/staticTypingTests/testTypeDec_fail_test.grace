dialect "StaticTyping"

// Example where block error not caught!
type A = {
    p(n:Number) -> Number
}

type B = A & type {
    q(_: Number) → String
}

type F = {apply(n: Number) → Number}

method m(x:F) → Number {x.apply(7)}

def yok: Number = m {n: Number → n + 40}

print(yok)

//def y: Number = m{x: String → x ++ "a"}

//print(y)
//2 + "a"
