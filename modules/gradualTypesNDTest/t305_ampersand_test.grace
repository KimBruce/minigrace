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
    "type A = \{a -> Boolean\}",
    "type B = \{b -> Boolean\}",
    "type C = \{c -> Boolean\}",
    "type D = \{d -> Boolean\}",
    "type AB = \{",
    "   a -> Boolean",
    "   b -> Boolean\}",
    "type AC = \{",
    "   a -> Boolean",
    "   c -> Boolean\}",
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
def typeNodeA : ast.AstNode = nodes.at(1)
def typeNodeB : ast.AstNode = nodes.at(2)
def typeNodeC : ast.AstNode = nodes.at(3)
def typeNodeD : ast.AstNode = nodes.at(4)
def typeNodeAB: ast.AstNode = nodes.at(5)
def typeNodeAC: ast.AstNode = nodes.at(6)

def A : gt.ObjectType = gt.anObjectType.fromDType(typeNodeA)
def B : gt.ObjectType = gt.anObjectType.fromDType(typeNodeB)
def C : gt.ObjectType = gt.anObjectType.fromDType(typeNodeC)
def D : gt.ObjectType = gt.anObjectType.fromDType(typeNodeD)
def AB: gt.ObjectType = gt.anObjectType.fromDType(typeNodeAB)
def AC: gt.ObjectType = gt.anObjectType.fromDType(typeNodeAC)

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
        def BorC : gt.ObjectType = B | C

        assert((A & BorC).isSubtypeOf(AB | AC)) description
            ("Type A & (B|C) should equal (A&B)|(A&C)")

        assert((AB | AC).isSubtypeOf(A & BorC)) description
            ("Type (A&B)|(A&C) should equal A & (B|C)")
    }

    test "variant self, non-variant other" by {
        def BorC : gt.ObjectType = B | C

        assert((BorC & A).isSubtypeOf(AB | AC)) description
            ("Type A & (B|C) should equal (A&B)|(A&C)")

        assert((AB | AC).isSubtypeOf(BorC & A)) description
            ("Type (A&B)|(A&C) should equal A & (B|C)")
    }

    test "variant self, variant other" by {
        def AorB : gt.ObjectType = A | B
        def CorD : gt.ObjectType = C | D

        assert((AorB & CorD).isSubtypeOf((A&C)|(A&D)|(B&C)|(B&D))) description
            ("(A|B) & (B|C) should equal (A&C)|(A&D)|(B&C)|(B&D)")

        assert(((A&C)|(A&D)|(B&C)|(B&D)).isSubtypeOf(AorB & CorD)) description
            ("(A&C)|(A&D)|(B&C)|(B&D) should equal (A|B) & (B|C)")
    }
}
