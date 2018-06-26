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


type AB = {
    a -> Number
    b -> String
}

class ab -> AB {
    method a -> Number { 47 }
    method b -> String { "Grace" }
}


def test : obj1.A & obj2.B = ab

//printing "47 Grace"
print "{test.a} {test.b}"
