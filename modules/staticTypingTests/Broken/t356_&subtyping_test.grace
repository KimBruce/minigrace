dialect "StaticTyping"
//This test fails because when the current implementation of subtyping sees
// (S & T) <: U, it checks it by evaluating (S <: U) || (T <: U).
//This subtyping rule is not quite correct, as seen in this test.

type S = {
    s -> S
}

type T = {
    t -> String
}

type U = {
    s -> U
    t -> String
}

class st -> U {
    method s -> S & T { self }
    method t -> String { "Hello World" }
}

print "This test should succeed"
