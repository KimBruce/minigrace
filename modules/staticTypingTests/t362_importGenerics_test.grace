dialect "StaticTyping"
import "t361_generics_test" as im

class a → im.D⟦String⟧ {
    method m → String { "Hello World" }
}

def x : im.D⟦Number⟧ = im.d
def y : Number = x.m
def b : im.D⟦String⟧ = a
def c : String = a.m

print (y)
print (c)
