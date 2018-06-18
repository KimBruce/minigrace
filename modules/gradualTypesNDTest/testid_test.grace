dialect "gradualTypesND"

def n: Number = 12

var m: String := "abc"

m := "def"

method q(k: Number) → Number {k + 1}

q(4)

def ifval: Number = if (false) then {n} else {12}
print (ifval)

type A = {
    p(np:Number) -> Number
    const → Number
    r → String
    r:=(r':String) → Done
}

def z : A = object {
    var r: String is public := "zzz"
    def const: Number is public = 12
    method p(nz:Number) -> Number {nz - const}   
}

print ("z.p(3) is {z.p(3)}")
print (z.const)
print (z.r)
z.r := "aaa"
print (z.r)

if (z.const > 0) then {print "positive"}

