dialect "minitest"
import "lexer" as lexer
import "parser" as parser
import "ast" as ast
import "util" as util
import "io" as io
import "StaticTyping" as st
import "ObjectTypeModule" as ot
import "identifierresolution" as ir
import "SharedTypes" as sh
import "standardGrace" as sg

def input : String = 
    "type Something = \{\n" ++
    "   x -> String\n" ++
    "\}\n" ++
    "type A = \{\n" ++
    "   p -> String\n" ++
    "\}\n" ++
    "type B = \{\n" ++
    "   n -> Number\n" ++
    "   m -> Something\n" ++
    "\}\n" ++
    "type C = \{\n" ++
    "   m -> Something\n" ++
    "\}\n" ++
    "type D = \{\n" ++
    "   n -> Number\n" ++
    "   p -> String \n" ++
    "\}\n"

// Turns input into an abstract syntax tree (ast)
def tokens = lexer.lexString(input)
def module = parser.parse(tokens)
def inputTree = ir.resolve(module)

// Returns a list of AstNodes corresponding to each testBlock
def nodes = inputTree.value

// Makes sure that the scope is built
inputTree.accept(st.astVisitor)

def typeSomething: ast.AstNode = nodes.at(1)
def typeA: ast.AstNode = nodes.at(2)
def typeB: ast.AstNode = nodes.at(3)
def typeC: ast.AstNode = nodes.at(4)
def typeD: ast.AstNode = nodes.at(5)

def objTypeSomething: sh.ObjectType = ot.anObjectType.fromDType(typeSomething.value) with (emptyList)
def objTypeA: sh.ObjectType = ot.anObjectType.fromDType(typeA.value) with (emptyList)
def objTypeB: sh.ObjectType = ot.anObjectType.fromDType(typeB.value) with (emptyList)
def objTypeC: sh.ObjectType = ot.anObjectType.fromDType(typeC.value) with (emptyList)
def objTypeD: sh.ObjectType = ot.anObjectType.fromDType(typeD.value) with (emptyList)

def objTypeAandB: sh.ObjectType = ot.anObjectType.makeWithOp("&", objTypeA, objTypeB)
def objTypeCandD: sh.ObjectType = ot.anObjectType.makeWithOp("&", objTypeC, objTypeD)

def objTypeAandBorD: sh.ObjectType = ot.anObjectType.makeWithOp("|", objTypeAandB, objTypeD)

print("{objTypeAandB.isSubtypeOf(objTypeCandD)}")