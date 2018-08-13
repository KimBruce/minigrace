dialect "StaticTyping"
type D⟦K⟧ = {
    m → K
}
class d → D⟦Number⟧ {
    method m → Number { 47 }
}

def x : D⟦Number⟧ = d
print (x.m)


method q⟦V⟧ (p : V) → V {
    p
}

print (q⟦String⟧("Hello World"))
