dialect "gradualTypesND"

type T = {
    type InternalType1 = Number
}

type U = {
    type InternalType2 = String
}

def testDec1 : (T&U).InternalType1 = 47
def testDec2 : (T&U).InternalType2 = "Grace"

print "test succeeded"
