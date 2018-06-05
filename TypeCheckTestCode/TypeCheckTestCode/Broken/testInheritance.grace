dialect "gradualTypesND"

//INHERITANCE IN OBJECT WORKS, BUT NOT IN CLASS!
type A = {
    p(n:Number) -> Number
}

type B = A & type {
    q(_: Number) → String
}

type F = {apply(n: Number) → Number}

def s:F = {n: Number → n + 1}

print (s.apply (12))

//def ss:String = match(3)
//    case {3 → "yes"}
//    case {_ → "no"}
//print ("ss = {ss}")

method z → A {
    object {
        method p(n:Number) -> Number {n}
    }
}

class zz (x:Number) → A {
    method p (n:Number) -> Number {x + n}
}

def oz: B = object {
    inherit zz(5)
    method q(n: Number) → String {"q"}
}

class oz2 → B {
    inherit zz(5)
    method q(n: Number) → String {"break"}
}

//method oz2 → B {object {
//    inherit zz(5)
//    method q(n: Number) → String {"q"}
//}}

print "z.p is {z.p(10)}"
print "zz.p() is {zz(3).p(5)}"
print "oz.p is {oz.p(12)}"
print "oz.q is {oz.q(12)}"

//3+"a"

