dialect "minitest"
import "lexer" as lexer
import "parser" as parser
import "ast" as ast
import "util" as util
import "io" as io
import "SharedTypes" as sh
import "ObjectTypeModule" as ot
import "identifierresolution" as ir

//  *****************************
//  **   start of test suite   **
//  *****************************
type ObjectType = ot.ObjectType

testSuiteNamed "ampersand with variant types" with {
    def typePattern     : ObjectType = ot.anObjectType.pattern
    def typeIterator    : ObjectType = ot.anObjectType.iterator
    def typeBoolean     : ObjectType = ot.anObjectType.boolean
    def typeNumber      : ObjectType = ot.anObjectType.number
    def typeString      : ObjectType = ot.anObjectType.string
    def typeListTp      : ObjectType = ot.anObjectType.listTp
    def typeDictionary  : ObjectType = ot.anObjectType.dictionary
    def typePoint       : ObjectType = ot.anObjectType.point
    def typeBinding     : ObjectType = ot.anObjectType.binding
    def typeObject      : ObjectType = ot.anObjectType.base

    test "Pattern & Object" by {
        def patternAmpObject : ObjectType = typePattern & typeObject
        assert ((patternAmpObject).isSubtypeOf(typePattern))
        assert ((typePattern).isSubtypeOf(patternAmpObject))
    }

    test "typeIterator & Object" by {
        def iteratorAmpObject : ObjectType = typeIterator & typeObject
        assert ((iteratorAmpObject).isSubtypeOf(typeIterator))
        assert ((typeIterator).isSubtypeOf(iteratorAmpObject))
    }

    test "typeBoolean & Object" by {
        def booleanAmpObject : ObjectType = typeBoolean & typeObject
        assert ((booleanAmpObject).isSubtypeOf(typeBoolean))
        assert ((typeBoolean).isSubtypeOf(booleanAmpObject))
    }

    test "typeNumber & Object" by {
        def numberAmpObject : ObjectType = typeNumber & typeObject
        assert ((numberAmpObject).isSubtypeOf(typeNumber))
        assert ((typeNumber).isSubtypeOf(numberAmpObject))
    }

    test "typeString & Object" by {
        def stringAmpObject : ObjectType = typeString & typeObject
        assert ((stringAmpObject).isSubtypeOf(typeString))
        assert ((typeString).isSubtypeOf(stringAmpObject))
    }

    test "typeListTp & Object" by {
        def listTpAmpObject : ObjectType = typeListTp & typeObject
        assert ((listTpAmpObject).isSubtypeOf(typeListTp))
        assert ((typeListTp).isSubtypeOf(listTpAmpObject))
    }

    test "typePoint & Object" by {
        def pointAmpObject : ObjectType = typePoint & typeObject
        assert ((pointAmpObject).isSubtypeOf(typePoint))
        assert ((typePoint).isSubtypeOf(pointAmpObject))
    }

    test "typeBinding & Object" by {
        def bindingAmpObject : ObjectType = typeBinding & typeObject
        assert ((bindingAmpObject).isSubtypeOf(typeBinding))
        assert ((typeBinding).isSubtypeOf(bindingAmpObject))
    }
}
