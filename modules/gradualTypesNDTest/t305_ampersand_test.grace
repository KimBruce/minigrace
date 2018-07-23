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
    "type A = \{a -> Boolean\}\n" ++
    "type B = \{b -> Boolean\}\n" ++
    "type C = \{c -> Boolean\}\n" ++
    "type D = \{d -> Boolean\}\n" ++
    "type AB = \{\n" ++
    "   a -> Boolean\n" ++
    "   b -> Boolean\}\n" ++
    "type AC = \{\n" ++
    "   a -> Boolean\n" ++
    "   c -> Boolean\}\n" ++
    ""

//Turns input into an abstract syntax tree (ast)
def tokens = lexer.new.lexString(input)
def module = parser.parse(tokens)
def inputTree = ir.resolve(module)

//Returns a list of AstNodes corresponding to each type
def nodes  = inputTree.value

//Turns type nodes into ObjectTypes so the type checker can process them
def typeNodeA : ast.AstNode = nodes.at(1)
def typeNodeB : ast.AstNode = nodes.at(2)
def typeNodeC : ast.AstNode = nodes.at(3)
def typeNodeD : ast.AstNode = nodes.at(4)
def typeNodeAB: ast.AstNode = nodes.at(5)
def typeNodeAC: ast.AstNode = nodes.at(6)

def A : sh.ObjectType = ot.anObjectType.fromDType(typeNodeA.value)
def B : sh.ObjectType = ot.anObjectType.fromDType(typeNodeB.value)
def C : sh.ObjectType = ot.anObjectType.fromDType(typeNodeC.value)
def D : sh.ObjectType = ot.anObjectType.fromDType(typeNodeD.value)
def AB: sh.ObjectType = ot.anObjectType.fromDType(typeNodeAB.value)
def AC: sh.ObjectType = ot.anObjectType.fromDType(typeNodeAC.value)

//  *****************************
//  **   start of test suite   **
//  *****************************

testSuiteNamed "ampersand with variant types" with {

    test "self & self" by {
        assert({A & A}) shouldntRaise (Exception)
    }

    test "self & literal" by {
        //type-checker fails to find method 'isDynamic' inside 5
        assert({A & 5}) shouldRaise (NoSuchMethod)
    }

    test "&-operator on simple types" by {
        //'==' currently not working
        //assert((A & B) == (AB))

        assert((A & B).isSubtypeOf(AB))

        assert(AB.isSubtypeOf(A & B))
    }

    test "non-variant self, variant other" by {
        def BorC : sh.ObjectType = B | C

        assert((A & BorC).isSubtypeOf(AB | AC)) description
            ("Type A & (B|C) should equal (A&B)|(A&C)")

        assert((AB | AC).isSubtypeOf(A & BorC)) description
            ("Type (A&B)|(A&C) should equal A & (B|C)")
    }

    test "variant self, non-variant other" by {
        def BorC : sh.ObjectType = B | C

        assert((BorC & A).isSubtypeOf(AB | AC)) description
            ("Type A & (B|C) should equal (A&B)|(A&C)")

        assert((AB | AC).isSubtypeOf(BorC & A)) description
            ("Type (A&B)|(A&C) should equal A & (B|C)")
    }

    test "variant self, variant other" by {
        def AorB : sh.ObjectType = A | B
        def CorD : sh.ObjectType = C | D

        assert((AorB & CorD).isSubtypeOf((A&C)|(A&D)|(B&C)|(B&D))) description
            ("(A|B) & (B|C) should equal (A&C)|(A&D)|(B&C)|(B&D)")

        assert(((A&C)|(A&D)|(B&C)|(B&D)).isSubtypeOf(AorB & CorD)) description
            ("(A&C)|(A&D)|(B&C)|(B&D) should equal (A|B) & (B|C)")
    }
}
