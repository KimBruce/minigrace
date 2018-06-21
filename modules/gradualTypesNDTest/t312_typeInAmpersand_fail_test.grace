dialect "gradualTypesND"

type T = {
    type InternalType = Number
}

type U = {
    type InternalType = String
}

def testDec : (T&U).InternalType = 47

print "should error before this"
