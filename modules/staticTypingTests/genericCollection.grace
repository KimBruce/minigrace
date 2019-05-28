dialect "StaticTyping"
print "hello"
def x: Collection[[Number]] = [1,2,3]
for(x) do {i: Number -> print(i)}
