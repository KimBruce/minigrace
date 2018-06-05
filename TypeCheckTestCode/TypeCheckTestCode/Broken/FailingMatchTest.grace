dialect "gradualTypesND"

// Currently crashes when type checking match statement!
var x: Number | String := 5

def os: String = match(x) 
    case {oa: Number → "is number"}
    case {ob: String → "is string"}
    
print(os)