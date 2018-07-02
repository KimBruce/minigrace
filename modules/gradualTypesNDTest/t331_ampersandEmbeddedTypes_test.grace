dialect "gradualTypesND"

type T = {
    type internalType1 = Number
}

type U = {
    type internalType2 = Number
}

def x: T.internalType1 & U.internalType2 = 47

print "{x}"
print "Test Succeeded"
