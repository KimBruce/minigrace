dialect "StaticTyping"

method f(hh: Number) → Number {
    if (hh == 0) then {
        return 12}
    hh*9
}

print (f(0))
print "f(1) is {f(1)}"
