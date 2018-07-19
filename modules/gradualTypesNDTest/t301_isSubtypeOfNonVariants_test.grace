dialect "minitest"
import "lexer" as lexer
import "parser" as parser
import "ast" as ast
import "util" as util
import "io" as io
import "gradualTypesND" as gt
import "identifierresolution" as ir

//Declare types such that types C and D are subtypes of type A
def input : String =
    "type A = \{one -> Boolean\}\n" ++
    "type A' = \{\n" ++
    "    one -> Boolean\n" ++
    "    two -> Number\n" ++
    "\}\n" ++
    "\n" ++
    "type B = \{one(b:Boolean) -> Boolean\}\n" ++
    "type B' = \{\n" ++
    "    one(b:Boolean) -> Boolean\n" ++
    "    two(n:Number) -> Number\n" ++
    "\}\n" ++
    "type B'' = \{\n" ++
    "    one(b:Boolean) -> Boolean\n" ++
    "    two(n1:Number, n2:Number) -> Number\n" ++
    "\}\n" ++
    ""

//Turns input into an abstract syntax tree (ast)
def tokens = lexer.new.lexString(input)
def module = parser.parse(tokens)
def inputTree = ir.resolve(module)

//Returns a list of AstNodes corresponding to each type
def nodes  = inputTree.value

//Turns type nodes into ObjectTypes so the type checker can process them
def typeA  : ast.AstNode = nodes.at(1)
def typeA' : ast.AstNode = nodes.at(2)
def typeB  : ast.AstNode = nodes.at(3)
def typeB' : ast.AstNode = nodes.at(4)
def typeB'': ast.AstNode = nodes.at(5)

def objTypeA  : gt.ObjectType = gt.anObjectType.fromDType(typeA.value)
def objTypeA' : gt.ObjectType = gt.anObjectType.fromDType(typeA'.value)

def objTypeB  : gt.ObjectType = gt.anObjectType.fromDType(typeB.value)
def objTypeB' : gt.ObjectType = gt.anObjectType.fromDType(typeB'.value)
def objTypeB'': gt.ObjectType = gt.anObjectType.fromDType(typeB''.value)

//  *****************************
//  **   start of test suite   **
//  *****************************

testSuiteNamed "isSubtypeOf non-variant tests" with {
    test "self subtyping" by{
      assert(objTypeA.isSubtypeOf(objTypeA)) description
            ("Type A should have evaluated as a subtype of itself")
    }

    test "non-variant subtyping" by{
      assert(objTypeA'.isSubtypeOf(objTypeA)) description
            ("Type A' should have evaluated as a subtype of Type A")

      deny(objTypeA.isSubtypeOf(objTypeA')) description
            ("Type A should not have evaluated as a subtype of Type A'")
    }

    test "self subtyping with parameters" by{
      assert(objTypeB.isSubtypeOf(objTypeB)) description
            ("Type B should have evaluated as a subtype of itself")
    }

    test "non-variant subtyping with parameters" by{
      assert(objTypeB'.isSubtypeOf(objTypeB)) description
            ("Type B' should have evaluated as a subtype of Type B")

      deny(objTypeA.isSubtypeOf(objTypeB)) description
            ("Type A should not have evaluated as a subtype of Type B")

      deny(objTypeB.isSubtypeOf(objTypeA)) description
            ("Type B should not have evaluated as a subtype of Type A")

      deny(objTypeB''.isSubtypeOf(objTypeB')) description
            ("Type B'' should not have evaluated as a subtype of Type B'")
    }
}
