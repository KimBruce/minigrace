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
def tokens = lexer.lexString(input)
def module = parser.parse(tokens)
def inputTree = ir.resolve(module)

//Returns a list of AstNodes corresponding to each type
def nodes  = inputTree.value

//Turns type nodes into ObjectTypes so the type checker can process them
def typeA : ast.AstNode = nodes.at(1)
def typeB : ast.AstNode = nodes.at(2)
def typeC : ast.AstNode = nodes.at(3)
def typeD : ast.AstNode = nodes.at(4)

def objTypeA : sh.ObjectType = ot.anObjectType.fromDType(typeA.value) with (emptyList)
def objTypeB : sh.ObjectType = ot.anObjectType.fromDType(typeB.value) with (emptyList)
def objTypeC : sh.ObjectType = ot.anObjectType.fromDType(typeC.value) with (emptyList)
def objTypeD : sh.ObjectType = ot.anObjectType.fromDType(typeD.value) with (emptyList)

def objTypeBorC : sh.ObjectType = objTypeB | objTypeC

print("\nResult: {objTypeBorC.isSubtypeOf(objTypeA)}")