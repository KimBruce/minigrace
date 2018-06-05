dialect "gradualTypesND"

type A = {
    n → Number
    type B = {s → String}
}

def o: A = object {
    method n → Number {17}
    type B = {s → String}
}

def b: o.B = object {
    method s → String {"Hello"}
}

print (b.s)

print (o.n)

//9 + "A"