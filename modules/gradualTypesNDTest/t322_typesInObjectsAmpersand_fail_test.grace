dialect "gradualTypesND"


type SuperA = {
    type A = { a -> Number }
}

type SuperB = {
    type B = { b -> String}
}

def obj1 : SuperA = object {
    type A = { a -> Number }
}

def obj2 : SuperB = object {
    type B = { b -> String}
}

type CD = {
    c -> Number
    d -> String
}

class cd -> CD {
    method c -> Number { 47 }
    method d -> String { "Grace" }
}


def test : obj1.A & obj2.B = cd

//TypeError at test before this print
print "{test.c} {test.d}"
