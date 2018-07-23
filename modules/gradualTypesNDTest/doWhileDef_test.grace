dialect "StaticTyping"

type BoolBlock = {
    apply → Boolean
}

type Proc0 = {
    apply → Done
}

method do (block:Proc0) while (cond: BoolBlock) → Done {
    block.apply
    while (cond) do (block)
}

var i: Number := 1
do {
    print(i)
    i := i + 1
} while {i < 10}
