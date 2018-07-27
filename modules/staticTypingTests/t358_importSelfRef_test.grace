dialect "StaticTyping"

import "t354_specialisationRecursionLoop_test" as im

type T = im.B & interface {
    m -> T
    p -> Number
}

class t -> T {
    method m -> T { self }
    method p -> Number { 47 }
    method n -> String {"Hello World"}
}

print (t.m.p)
