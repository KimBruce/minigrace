dialect "StaticTyping"

type A = {
    nm (l:String, k: Number) → Object
    p(s:String) → String
}

type B = {
        nm (l:String, k: Number) → Number
}

class a (n: Number) → A {
    method nm(l:String, k: Number) → Object {47}
    method p(s:String) → String {"bye"}
}

class b → B{
    inherit a(2)
        alias oldnm(l: String, k: Number) = nm(l, k)
        exclude p(s:String)
    method nm(l:String,k: Number) → Number {
        oldnm("hello",3) + 2
    }
    print(nm("s",47))
}
b
b.p("s")

//3 + "s"
    