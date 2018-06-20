dialect "gradualTypesND"

type T = {
    type InternalType = Number
}

def testDec : T.InternalType = 47

print "test succeeded"
