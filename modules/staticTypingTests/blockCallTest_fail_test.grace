dialect "StaticTyping"

type F = {apply(n: Number) → Number}

method m(x:F) → Number {x.apply(7)}

// should be type error here as block should start with {x: Number → ...}
def y: Number = m{x: String → x ++ "a"}

print(y)
2 + "a"
