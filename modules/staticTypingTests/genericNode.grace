//dialect "StaticTyping"
type Node[[T]] = {
   val -> T
   next -> Node[[T]]
}

class emptyNode[[T]](v:Number) -> Node[[T]] {
    method next -> Node[[T]] {
       self
    }
    method val -> T {
       ProgrammingError.raise "call next on empty list"
    }
}

def n: Node[[Number]] = emptyNode[[Number]](2)
print (n.val)