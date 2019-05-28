dialect "StaticTyping"

type A = {
    y → Number
    y:= (s: Number) → Done
}

def a: A = object {
    var y: Number is public := 0
    y:= 1
}

a.y := 3
print (a.y)

var x: Number := 1
self.x := self.x + 22
print(x)