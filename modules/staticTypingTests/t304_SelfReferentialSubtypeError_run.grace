dialect "StaticTyping"

type Foo = {
    a -> Foo
    b -> Number
}

type Bar = {
    a -> Bar
}

class bar -> Bar {
    method a -> Bar { self }
}

def test : Foo = bar