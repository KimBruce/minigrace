dialect "minitest"
import "lexer" as lexer
import "parser" as parser
import "ast" as ast
import "util" as util
import "io" as io
import "SharedTypes" as sh
import "ObjectTypeModule" as ot
import "identifierresolution" as ir

//Declare types such that types C and D are subtypes of type A
def input : String =
    "type A = \{one -> Boolean\}\n" ++
    "type B = \{two -> Number\}\n" ++
    "type C = \{\n" ++
    "    one -> Boolean\n" ++
    "    three -> Number\n" ++
    "\}\n" ++
    "type D = \{\n" ++
    "    one -> Boolean\n" ++
    "    four -> String\n" ++
    "\}\n" ++
    ""

//Turns input into an abstract syntax tree (ast)
def tokens = lexer.new.lexString(input)
def module = parser.parse(tokens)
def inputTree = ir.resolve(module)

//Returns a list of AstNodes corresponding to each type
def nodes  = inputTree.value

//Turns type nodes into ObjectTypes so the type checker can process them
def typeA : ast.AstNode = nodes.at(1)
def typeB : ast.AstNode = nodes.at(2)
def typeC : ast.AstNode = nodes.at(3)
def typeD : ast.AstNode = nodes.at(4)

def objTypeA : sh.ObjectType = ot.anObjectType.fromDType(typeA.value)
def objTypeB : sh.ObjectType = ot.anObjectType.fromDType(typeB.value)
def objTypeC : sh.ObjectType = ot.anObjectType.fromDType(typeC.value)
def objTypeD : sh.ObjectType = ot.anObjectType.fromDType(typeD.value)

//  *****************************
//  **   start of test suite   **
//  *****************************

testSuiteNamed "isSubtypeOf variant tests" with {

    test "non-variant self, variant other" by {
      def objTypeAorB : sh.ObjectType = objTypeA | objTypeB
      def objTypeBorC : sh.ObjectType = objTypeB | objTypeC

      assert(objTypeC.isSubtypeOf(objTypeAorB)) description
            ("Type C should have evaluated as a subtype of Type A | B")

      deny(objTypeA.isSubtypeOf(objTypeBorC)) description
            ("Type A should not have evaluated as a subtype of Type B | C")
    }

    test "variant self, non-variant other" by {
      def objTypeCorD : sh.ObjectType = objTypeC | objTypeD
      def objTypeBorC : sh.ObjectType = objTypeB | objTypeC

      assert(objTypeCorD.isSubtypeOf(objTypeA)) description
            ("Type C | D should have evaluated as a subtype of Type A")

      deny(objTypeBorC.isSubtypeOf(objTypeA)) description
            ("Type B | C should not have evaluated as a subtype of Type A")

    }

    test "variant self, variant other" by {
      def objTypeCorD : sh.ObjectType = objTypeC | objTypeD
      def objTypeBorA : sh.ObjectType = objTypeB | objTypeA

      def objTypeCorB : sh.ObjectType = objTypeC | objTypeB
      def objTypeAorC : sh.ObjectType = objTypeA | objTypeC

      assert(objTypeCorD.isSubtypeOf(objTypeBorA)) description
            ("Type C | D should have evaluated as a subtype of Type B | A")

      deny(objTypeCorB.isSubtypeOf(objTypeAorC)) description
            ("Type C | B should not have evaluated as a subtype of Type A | C")
    }
}
