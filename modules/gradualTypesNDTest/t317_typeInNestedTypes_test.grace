dialect "gradualTypesND"

type U = {
    type V = {
        type T = Number
    }
}

def o : U = object {
    type V = {
        type T = Number
    }
}

def testDec: o.V.T = 3

print "test succeeded"
