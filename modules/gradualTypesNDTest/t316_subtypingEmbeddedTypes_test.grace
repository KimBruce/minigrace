dialect "minitest"
import "lexer" as lexer
import "parser" as parser
import "ast" as ast
import "util" as util
import "io" as io
import "gradualTypesND" as gt
import "identifierresolution" as ir

//Declare types such that types C and D are subtypes of type A
def input = sequence [
    "type A = \{",
    "   type InnerA = \{",
    "       one -> Number",
    "   \}",
    "\}",
    "type A' = \{",
    "   type InnerA = \{",
    "       one -> Number",
    "   \}",
    "\}",
    "type A'' = \{",
    "   type InnerA = \{",
    "       one -> Number",
    "       two -> Boolean",
    "       type InnerMostA = Boolean",
    "   \}",
    "\}",
    "type B = \{",
    "   type InnerB = \{",
    "       one -> Number",
    "   \}",
    "\}",
    "type B' = \{",
    "   type InnerA = Number",
    "\}",
    ""
]

util.lines.addAll(input)

//Turns input into an abstract syntax tree (ast)
def tokens = lexer.new.lexinput(input)
def module = parser.parse(tokens)
def inputTree = ir.resolve(module)

//Returns a list of AstNodes corresponding to each type
def nodes  = inputTree.value

//Turns type nodes into ObjectTypes so the type checker can process them
def typeA  : ast.AstNode = nodes.at(1)
def typeA' : ast.AstNode = nodes.at(2)
def typeA'': ast.AstNode = nodes.at(3)
def typeB  : ast.AstNode = nodes.at(4)
def typeB' : ast.AstNode = nodes.at(5)

def objTypeA  : gt.ObjectType = gt.anObjectType.fromDType(typeA)
def objTypeA' : gt.ObjectType = gt.anObjectType.fromDType(typeA')
def objTypeA'': gt.ObjectType = gt.anObjectType.fromDType(typeA'')
def objTypeB  : gt.ObjectType = gt.anObjectType.fromDType(typeB)
def objTypeB' : gt.ObjectType = gt.anObjectType.fromDType(typeB')

//  *****************************
//  **   start of test suite   **
//  *****************************

testSuiteNamed "isSubtypeOf embedded types tests" with {
    test "self subtyping" by{
        assert(objTypeA.isSubtypeOf(objTypeA)) description
            ("Type A should have evaluated as a subtype of itself")
    }

    test "different type name" by {
        assert(objTypeA.isSubtypeOf(objTypeA')) description
            ("Type A should have evaluated as a subtype of Type A'")

        assert(objTypeA'.isSubtypeOf(objTypeA)) description
            ("Type A' should have evaluated as a subtype of Type A")
    }

    test "one-directional subtyping" by {
        assert(objTypeA''.isSubtypeOf(objTypeA)) description
            ("TypeA'' should have evaluated as a subtype of Type A")

        deny(objTypeA.isSubtypeOf(objTypeA'')) description
            ("TypeA should not have evaluated as a subtype of Type A''")
    }

    test "incompatible embedded type name" by {
        deny(objTypeB.isSubtypeOf(objTypeA)) description
            ("TypeB should not have evaluated as a subtype of Type A")
    }

    test "incompatible embedded type definition" by {
        deny(objTypeB'.isSubtypeOf(objTypeA)) description
            ("TypeB' should not have evaluated as a subtype of Type A")

        deny(objTypeA.isSubtypeOf(objTypeB')) description
            ("TypeA should not have evaluated as a subtype of Type B'")
    }
}
