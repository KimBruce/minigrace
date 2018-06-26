dialect "gradualTypesND"

type U = {
    type T = Number
}

def o : U = object {
    type T = Number
}

def testDec : o.T = "producing TypeError"

print "should error before this"
