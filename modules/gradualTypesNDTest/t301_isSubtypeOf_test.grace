dialect "minitest"
import "lexer" as lexer
import "parser" as parser
import "ast" as ast
import "util" as util
import "io" as io
import "gradualTypesND" as gt
import "identifierresolution" as ir

def input = sequence [
    "def testBlock1: Object = object \{",
    "   type A = \{one -> Boolean\}",
    "   type B = \{two -> Number\}",
    "   type C = \{",
    "       one -> Boolean",
    "       three -> Number",
    "   \}",
    "   type D = \{",
    "       one -> Boolean",
    "       four -> String",
    "   \}",
    "\}",
    "",
    "def testBlock2: Object = object \{",
    "   type A = \{one -> Boolean\}",
    "   type B = \{",
    "     one -> Boolean",
    "     two -> Number",
    "   \}",
    "\}",
    ""

]

util.lines.addAll(input)

def tokens = lexer.new.lexinput(input)
def module = parser.parse(tokens)
def inputTree = ir.resolve(module)
def nodes  = inputTree.value

method fromTypeNode(node: ast.AstNode) -> gt.ObjectType {
    def typeMethods: Collection[[ast.AstNode]] = node.value.methods
    def methodTypeList: List[[gt.MethodType]] = emptyList
    for (typeMethods) do {m : ast.AstNode ->
        methodTypeList.add(gt.aMethodType.fromNode(m))
    }

    gt.anObjectType.fromMethods(methodTypeList)
}



testSuiteNamed "isSubtypeOf tests" with {

    def typeA : ast.AstNode = nodes.at(1).value.value.at(1)
    def objTypeA : gt.ObjectType = fromTypeNode(typeA)

    def typeB : ast.AstNode = nodes.at(1).value.value.at(2)
    def objTypeB : gt.ObjectType = fromTypeNode(typeB)

    def typeC : ast.AstNode = nodes.at(1).value.value.at(3)
    def objTypeC : gt.ObjectType = fromTypeNode(typeC)

    def typeD : ast.AstNode = nodes.at(1).value.value.at(4)
    def objTypeD : gt.ObjectType = fromTypeNode(typeD)


    test "self subtyping" by{

      assert(objTypeA.isSubtypeOf(objTypeA)) description
            ("Type A should have evaluated as a subtype of itself")
    }

    test "non-variant self and other" by{

      assert(objTypeC.isSubtypeOf(objTypeA)) description
            ("Type C should have evaluated as a subtype of Type A")
      deny(objTypeA.isSubtypeOf(objTypeC)) description
            ("Type A should not have evaluated as a subtype of Type C")
    }

    test "non-variant self, variant other" by {


      def objTypeAorB : gt.ObjectType = objTypeA | objTypeB
      def objTypeBorC : gt.ObjectType = objTypeB | objTypeC

      assert(objTypeC.isSubtypeOf(objTypeAorB)) description
            ("Type C should have evaluated as a subtype of Type A | B")
      deny(objTypeA.isSubtypeOf(objTypeBorC)) description
            ("Type A should not have evaluated as a subtype of Type B | C")
    }

    test "variant self, non-variant other" by {

      def objTypeCorD : gt.ObjectType = objTypeC | objTypeD
      def objTypeBorC : gt.ObjectType = objTypeB | objTypeC

      assert(objTypeCorD.isSubtypeOf(objTypeA)) description
            ("Type C | D should have evaluated as a subtype of Type A")
      deny(objTypeBorC.isSubtypeOf(objTypeA)) description
            ("Type B | C should not have evaluated as a subtype of Type A")

    }

    test "variant self, variant other" by {

      //defs for assert statement
      def objTypeCorD : gt.ObjectType = objTypeC | objTypeD
      def objTypeBorA : gt.ObjectType = objTypeB | objTypeA

      //defs for deny statement
      def objTypeCorB : gt.ObjectType = objTypeC | objTypeB
      def objTypeAorC : gt.ObjectType = objTypeA | objTypeC

      assert(objTypeCorD.isSubtypeOf(objTypeBorA)) description
            ("Type C | D should not have evaluated as a subtype of Type B | A")
      deny(objTypeCorB.isSubtypeOf(objTypeAorC)) description
            ("Type C | B should not have evaluated as a subtype of Type A | C")

    }


}
