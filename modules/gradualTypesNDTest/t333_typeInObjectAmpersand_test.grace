dialect "gradualTypesND"

type T = {
    type InnerT = Number
}

type U = {
    type InnerU = Number
}

def o1 : T = object {
    type InnerT = Number
}

def o2 : U = object {
    type InnerU = Number
}

def x : o1.InnerT & o2.InnerU = 47
print "{x}"
print "Test Succeeded"
