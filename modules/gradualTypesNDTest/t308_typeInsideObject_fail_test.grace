dialect "gradualTypesND"

def o : Object = object {
    type T = Number
}

def testDec : o.T = "producing TypeError"

print "should error before this"
