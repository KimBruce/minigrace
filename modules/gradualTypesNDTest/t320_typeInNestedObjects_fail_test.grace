dialect "gradualTypesND"


type R = {
    middleObj -> S
}

type S = {
    innerObj -> T
}

type T = {
    type V = Number
}

def outerObj : R = object {
    def middleObj : S is public = object {
        def innerObj : T is public = object {
            type V = Number
        }
    }
}

def testDec: outerObj.middleObj.innerObj.V = "producing TypeError"

print "test should fail"
