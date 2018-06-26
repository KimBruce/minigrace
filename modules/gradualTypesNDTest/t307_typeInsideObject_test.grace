dialect "gradualTypesND"

type U = {
    type T = Number
}

def o : U = object {
    type T = Number
}

def testDec: o.T = 3

print "test succeeded"
