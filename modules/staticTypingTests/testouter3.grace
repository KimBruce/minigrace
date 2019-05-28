dialect "StaticTyping"

type C = {
    m(t:Number) → Done
}

type D = {
    n(t:Number) → Done
}

def z: Number = 12

def x: C = object {
    def y: Number = 2
    method m(t:Number) → Done {
        def w: Number = 3
        print "in mm"
    }
}
