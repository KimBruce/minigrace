dialect "gradualTypesND"

type T = {
    type InternalType = Number
}

type U = {
    type InternalType = Number
}

def testDec : (T|U).InternalType  = "producing TypeError"

print "should error before this"
