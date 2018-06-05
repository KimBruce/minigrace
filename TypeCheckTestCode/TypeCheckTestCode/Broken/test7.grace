dialect "gradualTypesND"

// INNER TYPE DEFINITION FAILS
type A = {
    type B = Number
    m(n:B) → B
}

//def x: A.B = 3
//print(x)

def o:A = object {
    type B = String
    method m(n: B) → B {n++"!"}
}

def y: o.B = "abc"
print(y)
print(o.m("a"))
