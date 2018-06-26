dialect "gradualTypesND"

type U = {
    internalObject -> V
}

type V = {
    type T = Number
}

def o : U = object {
    def internalObject : V is public = object {
        type T = Number
    }
}

def testDec: o.internalObject.T = 3

print "test succeeded"
