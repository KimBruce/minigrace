dialect "gradualTypesND"

type SuperA = { type A = { a -> Number } }
type SuperB = { type B = { b -> String } }

def obj1 : SuperA = object {
    type A = { a -> Number }
}

def obj2 : SuperB = object {
    type B = { b -> String}
}

type SubtypeOfA = { a -> Number }
class subOfA -> SubtypeOfA {
    method a -> Number { 47 }
}

type NotSubtypeOfB = { incompatibleMethod -> String }
class notSubOfB -> NotSubtypeOfB {
    method incompatibleMethod -> String { "Grace" }
}

def test1 : obj1.A | obj2.B = subOfA
def test2 : obj1.A | obj2.B = notSubOfB

//TypeError at test2 before this print
print "test should fail"
