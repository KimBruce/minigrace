dialect "StaticTyping"
dialect "minitest"

method m(s: Number) → Number {s + 1}

def a: Number = m("A")

print (a)
