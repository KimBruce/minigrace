dialect "gradualTypesND"

type T = {
    type InternalType = Number
}

type U = {
    type InternalType = Number
}

def testDec : (T&U).InternalType = 47

print "test succeeded"
