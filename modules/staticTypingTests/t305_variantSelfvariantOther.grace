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
def tokens = lexer.lexString(input)
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

def A : sh.ObjectType = ot.anObjectType.fromDType(typeNodeA.value) with (emptyList)
def B : sh.ObjectType = ot.anObjectType.fromDType(typeNodeB.value) with (emptyList)
def C : sh.ObjectType = ot.anObjectType.fromDType(typeNodeC.value) with (emptyList)
def D : sh.ObjectType = ot.anObjectType.fromDType(typeNodeD.value) with (emptyList)
def AB: sh.ObjectType = ot.anObjectType.fromDType(typeNodeAB.value) with (emptyList)
def AC: sh.ObjectType = ot.anObjectType.fromDType(typeNodeAC.value) with (emptyList)

def ABorAC: sh.ObjectType = AB | AC
def BorC: sh.ObjectType = B | C
def AandBorC: sh.ObjectType = A & BorC

print("\nResults: {(AB | AC).isSubtypeOf(BorC & A)}")
// print("\nsomtrue.isSubtypeOf(somfalse): {somtrue.isSubtypeOf(somfalse)}")
