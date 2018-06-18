dialect "gradualTypesND"

type Foo = { a -> Foo}
type Bar = {
    a -> Bar
    b -> Number
}

class bar -> Bar {
    method a -> Bar {self}
    method b -> Number {0}
}

def test : Foo = bar

print "test succeeded"
