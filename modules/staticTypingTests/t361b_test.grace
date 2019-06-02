dialect "StaticTyping"
type D = {
    m → Number
}
class d → D {
    method m → Number { 47 }
}

def x : D = d
print (x.m)
