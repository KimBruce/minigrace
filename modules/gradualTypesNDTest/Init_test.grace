dialect "gradualTypesND"

var s: String := "a"

type Object = {}

type A = {
    new(arg:String) → Object
}

def aa: Object = object {
    method new → Number {4}
}

def b: Object = object {
    s := s ++ "object b "
}

method new(arg:String) → Object {
    object {
        s := s ++ "object a ({arg})"
    }
}

def a:A = object {
    method new(arg:String) → Object {
        object {
            s := s ++ "object a ({arg})"
        }
    }
    method asString → String { "a" }
}

print "test succeeded"
