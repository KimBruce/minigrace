dialect "gradualTypesND"

method d(aa:Number) → Number {aa}

print(d(5))
type A = {
    p(n:Number) -> Number
    q → String
    sf → Number
    sf:= (n: Number) → Done
}

var y:Number is public := 16

def obj: A = object {
        var sf: Number is public := 12
        method p(m: Number) → Number {m}
        method q → String {"d"}
}

obj.sf

obj.p(23)

method c (pp:Number) → A {
    object {
        var sf: Number is public := 12
        method p(n: Number) → Number {n + 1}
        method q → String {"abc"}
    }
}

class cc (pp:Number) → A {
    var sf: Number is public := 12
    method p(n: Number) → Number {n + 1}
    method q → String {"d"}
}

def co: A = cc(47)
co.sf
co.sf := 5
// co.s   // should give no such method error
print "co.sf = {co.sf}"

print "{c(47).q}"

print (c(47).p (17))

def d: A = c (47)
print(d.sf)
d.sf := 12
def xy: Number = d.sf - 47
print(xy)

print (1 + c(47).p(11))

var n: Number := 6

while {n > 0} do {
    print (n)
    n := n - 1
}

for (1..4) do {nn: Number →
    print(nn)
}

var zz: Number | String := 5

match(zz)
    case {zzn: Number → print "number {zzn+1}"}
    case {zzs: String → print "string {zzs++"!"}"}
    case {_ : Object → print "no match"}
