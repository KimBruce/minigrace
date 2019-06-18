dialect "StaticTyping"
type D⟦K⟧ = {
    m(k:K) → K
}
class d → D⟦Number⟧ {
    method m(n:Number) → Number { n + 47 }
}

def x : D⟦Number⟧ = d
print (x.m(3))
