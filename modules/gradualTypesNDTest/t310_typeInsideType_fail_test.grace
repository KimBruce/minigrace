dialect "gradualTypesND"

type T = {
    type InternalType = Number
}

def testDec : T.InternalType  = "producing TypeError"

print "should error before this"
