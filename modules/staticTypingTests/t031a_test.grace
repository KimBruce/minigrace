dialect "StaticTyping"

var s: String

type A = {
    new (arg: String) → Object
}

def t: String = "b"

s := t
        
def a:A = object {
    method new (arg: String) → Object {
        object {
            s := s ++ "object a (" ++ arg ++")"
        }
    }
    method asString → String { "a" }
}

s := ""
a.new "in constructor"

s := ""

object {
            inherit a.new "in inherit"
            s := s ++ " in body of constructor"
}

print(s)
