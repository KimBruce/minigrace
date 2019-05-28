dialect "StaticTyping"

def a: String | Done = print(4)

print ("a"++a)
2+"a"
method m (b: String | Number) -> Done {
    print(b)
}

m("hello")
m(5)

var n: Number := 0

if ((n!= 0) && ((6/n) > 3)) then {
    print ("yes")
} else {
    print "no"
}

print (6/n)
