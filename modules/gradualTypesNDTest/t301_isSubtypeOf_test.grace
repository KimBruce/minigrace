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
    "type A = \{one -> Boolean\}",
    "type B = \{two -> Number\}",
    "type C = \{",
    "    one -> Boolean",
    "    three -> Number",
    "\}",
    "type D = \{",
    "    one -> Boolean",
    "    four -> String",
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

testSuiteNamed "isSubtypeOf tests" with {

    //Turns type nodes into ObjectTypes so the type checker can process them
    def typeA : ast.AstNode = nodes.at(1)
    def objTypeA : gt.ObjectType = fromTypeNode(typeA)

    def typeB : ast.AstNode = nodes.at(2)
    def objTypeB : gt.ObjectType = fromTypeNode(typeB)

    def typeC : ast.AstNode = nodes.at(3)
    def objTypeC : gt.ObjectType = fromTypeNode(typeC)

    def typeD : ast.AstNode = nodes.at(4)
    def objTypeD : gt.ObjectType = fromTypeNode(typeD)

    test "self subtyping" by{
      //assert A <: A
      assert(objTypeA.isSubtypeOf(objTypeA)) description
            ("Type A should have evaluated as a subtype of itself")
    }

    test "non-variant self and other" by{
      //assert C <: A
      assert(objTypeC.isSubtypeOf(objTypeA)) description
            ("Type C should have evaluated as a subtype of Type A")

      //deny A <: C
      deny(objTypeA.isSubtypeOf(objTypeC)) description
            ("Type A should not have evaluated as a subtype of Type C")
    }

    test "non-variant self, variant other" by {
      def objTypeAorB : gt.ObjectType = objTypeA | objTypeB
      def objTypeBorC : gt.ObjectType = objTypeB | objTypeC

      //assert C <: A|B
      assert(objTypeC.isSubtypeOf(objTypeAorB)) description
            ("Type C should have evaluated as a subtype of Type A | B")

      //deny A <: B|C
      deny(objTypeA.isSubtypeOf(objTypeBorC)) description
            ("Type A should not have evaluated as a subtype of Type B | C")
    }

    test "variant self, non-variant other" by {
      def objTypeCorD : gt.ObjectType = objTypeC | objTypeD
      def objTypeBorC : gt.ObjectType = objTypeB | objTypeC

      //assert C|D <: A
      assert(objTypeCorD.isSubtypeOf(objTypeA)) description
            ("Type C | D should have evaluated as a subtype of Type A")

      //deny B|C <: A
      deny(objTypeBorC.isSubtypeOf(objTypeA)) description
            ("Type B | C should not have evaluated as a subtype of Type A")

    }

    test "variant self, variant other" by {
      def objTypeCorD : gt.ObjectType = objTypeC | objTypeD
      def objTypeBorA : gt.ObjectType = objTypeB | objTypeA

      def objTypeCorB : gt.ObjectType = objTypeC | objTypeB
      def objTypeAorC : gt.ObjectType = objTypeA | objTypeC

      //assert C|D <: B|A
      assert(objTypeCorD.isSubtypeOf(objTypeBorA)) description
            ("Type C | D should not have evaluated as a subtype of Type B | A")

      //deny C|B <: A|C
      deny(objTypeCorB.isSubtypeOf(objTypeAorC)) description
            ("Type C | B should not have evaluated as a subtype of Type A | C")
    }
}
