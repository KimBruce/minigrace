dialect "gradualTypesND"
//This test is broken. Only works if line 19 is changed
//to say U.V instead of o.V

type T = {
    o -> U
    m -> Number
}

type U = {
    type V = Number
}


def myObject : T = object {
    def o : U is public= object {
        type V = Number
    }
    method m -> o.V {47}
}

def x : Number = myObject.m
print "{x}"
print "Test Succeeded"
