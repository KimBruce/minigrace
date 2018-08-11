dialect "StaticTyping"
//Tests method 'replaceGenericsWith' in MethodType and tests method 'apply' in
//GenericMethod. 

method m⟦T⟧ (p:T) → T {
    p
}

print (m⟦String⟧("test succeeded"))
