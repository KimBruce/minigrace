dialect "StaticTyping"

type S = {
    s -> Number
}

type T = {
    s -> Number
    t -> String
}

type U = {
    s -> Number
    t -> String
}

class st -> S & T {
    method s -> Number { 47 }
    method t -> String { "Hello World" }
}

method q(param : S & T) -> U {param}

print (q(st).t)
