dialect "gradualTypesND"

type A = {
    m -> Number
}

type B = {
    p -> String
}

type C = {
    n->Boolean
}


type D = A & B

type ComplicatedType = (A  & (C | B)) |  D
