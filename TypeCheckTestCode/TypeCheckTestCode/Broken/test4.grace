dialect "gradualTypesND"

// Currently crashes when type checking match statement!
// MATCH NOT CURRENTLY IMPLEMENTED
var y: Number
var x: Number | String

x := 3 + 7

print(x)

type A = {
    m → Number
}

type B = {
    s → String
}

def o: A | B = object {
//    method m → Number {77}
    method s → String {"hello"}
}

def os: String = match(x) 
    case {oa: Number → "a"}
    case {ob: String → "b"}
    
print(os)
