dialect "gradualTypesND"

type Base = {
    a → Boolean
}

type ReturnType = {
    a → Number
}

type Param = {
    a (bool : Boolean) → Boolean
}

type ParamType = {
    a (num : Number) → Boolean
}

type ParamSubtype = {
    a (bool: (Boolean & Base)) → Boolean
}

type Fail = Base & ReturnType
