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
        var a: D := object {
            method n(t':Number) → Done {
                print (outer.y)
                print (outer.outer.z+t')
            }
        } 
        a.n(3)
        print "in mm"
    }
}


x.m(5)
print ("done")