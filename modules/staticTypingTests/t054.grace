dialect "gradualTypesND"

// FAILS:  OUTER NOT TREATED PROPERLY?

// This is a test of a module with a name that's not an id.
// It also tests that `self` is correctly bound at the
// top level, and in a top-level method, to the module
// object.  It also checks that outer from a nested
// object also refers to the module object.

//inherit prelude.methods
    // the implementation of inheit in the C backend
    // replaces the object under construction by the
    // inherited object.  So this inherit statment
    // is here to check that self is still correctly
    // bound

type GObject = {}
type Slf = {
    three → String
    this → Slf
//    count → GObject
}

type Two = {
    thisModule → Slf
}

method three → String{ "three " }
method this → Slf { self }
method asString → String { "the t054 module object" }
def two: Two = object  {
    method asString → String { "two " }
    method thisModule → Slf { outer.this }  // compiles to outer.this
}
//class count → GObject {
//    method asString → String { "one " ++ two ++ three }
//}
//print(self)
//print(two.thisModule)
//print(count)
