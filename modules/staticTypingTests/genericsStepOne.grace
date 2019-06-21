dialect "StaticTyping"
//Tests method 'replaceGenericsWith' in MethodType and tests method 'apply' in
//GenericMethod. 

type F⟦T⟧ = interface {
    m → T
}

def o: F⟦Number⟧ = object {
    method m → Number {7}
}

print(o.m)

type G⟦T⟧ = interface {
    m(n:T) → T
    n(m: T, d: T) -> Number
}

def p: G⟦String⟧ = object {
    method m(k: String) → String {"hello"}
    method n(m: String, d: String ) -> Number {3}
}

def r: String = p.m("test")

print(r)