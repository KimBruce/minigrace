dialect "gradualTypesND"

// PROBLEM HERE IS THAT RETURN TYPE OF R RESOLVES TO UNKNOWN
type TC = {
    s → String
    type t = type { a(x: Number) -> Done }
}

class testClass → TC{
    def s: String is public = "hi there"
    type t = type { a(x: Number) -> Done }
    print "during initialization, s = \"{s}\"" // and t = {t}"
    method r → t {
        object{
            method a(x: Number) → Done {"r"}
        }
    }
    //print("r.a(5) gives {r.a(5)}")
}

def to: TC = testClass
//print (to.s)
//print "next"
//print (to.r.a(5))
