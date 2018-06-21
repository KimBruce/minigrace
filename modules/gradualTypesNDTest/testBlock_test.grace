dialect "gradualTypesND"

type Block = {apply(x: Number) → Number}

def blk:Block = {x: Number → 2*x}

print (blk.apply(5))
