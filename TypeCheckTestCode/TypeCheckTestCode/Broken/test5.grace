dialect "gradualTypesND"

// INHERITANCE BREAKS, BUT IN STRANGE WAY
type A = {
//    p(n:Number) -> Number
    q → String
//    sf → Number
//    sf:= (n: Number) → Done
}

//def a:A = object {
////    var sf: Number is public := 12
////    method p(n: Number) → String {n + 1}  // should give type-checking error
////    method p(n: Number) → Number {n + 1}
//    method q → String {"a"}
//}

class cc (pp:Number) → A {
//    var sf: Number is public := 12
//    method sf → Number {12}
//    method sf:=(n:Number) → Done {}
//    method p(n: Number) → Number {n + 2}
    method q → String {"cc"}
}

//print (cc(12).q)

class dd (r:Number) → A {
    inherit cc(r)
//    method p(n: Number) → Number {n+3}
}

//print (dd(12).p("abc"))
//print (dd(12).p(3))

//def m: Number = 14

print "Pattern \{ \}"