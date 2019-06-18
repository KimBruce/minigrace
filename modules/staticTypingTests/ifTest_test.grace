dialect "StaticTyping"

type O = {
    asString → String
}

def z: O = if (true) then {12} else {"s"}
def y: Number | String = if (true) then {12} else {"s"}

print (z)

def x: Number = if (true) then {4} else {5}

print(x)
