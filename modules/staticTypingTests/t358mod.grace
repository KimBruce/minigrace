dialect "minitest"
import "lexer" as lexer
import "parser" as parser
import "ast" as ast
import "util" as util
import "io" as io
import "SharedTypes" as sh
import "ObjectTypeModule" as ot
import "identifierresolution" as ir
import "StaticTyping" as st
import "ScopeModule" as sm

//Declare types such that types C and D are subtypes of type A

def input : String =
    "type A = \{\n"++
    "    m → A\n"++
    "\}\n"++
    "type C = \{\n"++
    "    m -> B\n"++
    "    n -> String\n"++
    "    r -> A\n"++
    "\}\n"++
    "type B = A & C\n"++
    "class b -> B \{\n"++
    "    method m -> B \{ self \}\n"++
    "    method n -> String \{ \"Hello\" \}\n"++
    "    method r -> A \{ self \}\n"++
    "\}\n" ++
    "def d: B = b.m"

//Turns input into an abstract syntax tree (ast)
def tokens = lexer.lexString(input)
def module = parser.parse(tokens)
def inputTree = ir.resolve(module)

//Returns a list of AstNodes corresponding to each type
def nodes = inputTree.value

//Turns type nodes into ObjectTypes so the type checker can process them
def typeNodeA : ast.AstNode = nodes.at(1)
def typeNodeC : ast.AstNode = nodes.at(2)
def typeNodeB : ast.AstNode = nodes.at(3)
def typeNodeb : ast.AstNode = nodes.at(4)
def typeNoded : ast.AstNode = nodes.at(5)

inputTree.accept(st.astVisitor)


def B : ot.ObjectType = sm.scope.types.stack.last.at("B")
def A : ot.ObjectType = sm.scope.types.stack.last.at("A")

print("\nB.normalMeths: {B.normalMeths}")

print("\nTEST 50: scope.types = {sm.scope.types.stack}")
print("\ntypeOf(typeNoded.value): {st.typeOf(typeNoded.value)}")
print("\nB.isSubtypeOf(A): {B.isSubtypeOf(A)}")
print("\nA.isSubtypeOf(B): {A.isSubtypeOf(B)}")