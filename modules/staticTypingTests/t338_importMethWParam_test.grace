dialect "StaticTyping"
import "t335A_basicImportee_test" as im

type T = {
    m(param : im.MyType) -> Number
    n -> String
}

class t → T{
    method m(param: im.MyType) → Number { param.a }
    method n → String {"Hello World"}
}

print (im.myMethWithParam(t))
