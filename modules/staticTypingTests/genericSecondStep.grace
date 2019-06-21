dialect "StaticTyping"

method m⟦T⟧(a:T) -> T{
    a
}

//def s:String = m⟦String⟧("hello")

//print(s)

print (m⟦String⟧("hello"))