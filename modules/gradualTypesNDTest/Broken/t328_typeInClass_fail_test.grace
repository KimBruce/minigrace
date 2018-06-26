dialect "gradualTypesND"
//This test is broken. It only works if lines 9 and 15 are
//changed to say T.U instead of just U

type T = {
    type U = {n -> Number}
    m -> String
    u -> U
}

class t -> T {
    type U = {n -> Number}
    method m -> String {"Hello World!"}
    class u -> U {method n -> Number {47}}
}

def x : U = t.u
def y : String = x.n
print "{y}"
print "Test should have failed"
