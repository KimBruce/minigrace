dialect "StaticTyping"

var x: Number := 0

if ((x == 0) || {(2/x) > 0}) then {
    print "OK"
} else {
    print "not OK"
}