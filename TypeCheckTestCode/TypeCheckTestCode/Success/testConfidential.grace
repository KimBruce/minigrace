dialect "gradualTypesND"

// Need to put in a second SelfType (that is not public)
// to provide the type of "self".  It will include "isMe"
// and all other confidential methods of "self"
type A = {
    m → Number
}

def a: A = object {
    method m → Number {n}
    method n → Number is confidential {
        if (isMe(a)) then {47} else {-20}
    }
}

// a.isMe(a)  //Properly not recognized 

print (a.m)