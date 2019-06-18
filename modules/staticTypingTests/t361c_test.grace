dialect "StaticTyping"

method q⟦V⟧ (p : V) → V {
    p
}

print (q⟦String⟧("Hello World"))
