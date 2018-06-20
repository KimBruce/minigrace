dialect "gradualTypesND"

def o : Object = object {
    def internalObject : Object is public = object {
        type T = Number
    }
}

def testDec: o.internalObject.T = 3

print "test succeeded"
