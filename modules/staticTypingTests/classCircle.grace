dialect "StaticTyping"

type A = {
    print → Done
}

class a → A {
    var x: Number := 0
    method print -> Done {print "in class a with {x} using {z}"}
}

class b -> A {
    inherit a 
        alias aprint = print
    var y: Number := 99
    method print -> Done {
        aprint
        print "in class b with {x} and {y}"
    }
}

def z: Number = 0
    
b.print