dialect "gradualTypesND"
//This file is not a test. It exists only for other tests to import it to test
//type-checking imports in gradualTypesND.

type EmbType = {
    type Inner = {
        a -> Number
    }
    b -> Boolean
}

def myObj : EmbType is public = object {
    type Inner = {
        a -> Number
    }
    method b -> Boolean { true }
}
