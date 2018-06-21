dialect "gradualTypesND"

def o : Object = object {
    type T = Number
}

def testDec: o.T = 3

print "test succeeded"
