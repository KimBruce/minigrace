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

def input : String =
    "type A = \{\n"++
    "    m(b:B) → A\n"++
    "    p → String\n"++
    "\}\n"++
    "type C = \{\n"++
    "    m(b:A) -> B\n"++
    "    n -> String\n"++
    "\}\n"++
    "type B = A & C\n"++
    "class b -> B \{\n"++
    "    method m(b':A) -> B \{ self }\n"++
    "    method n -> String \{ \"Hello\" \}\n"++
    "    method p → String \{ \"There\"\} \n"++
    "\}\n"++
    "def d: A = b.m(b)\n"++
    "type G = A | B"

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
def typeNodeG : ast.AstNode = nodes.at(6)

inputTree.accept(st.astVisitor)

def A : ot.ObjectType = sm.scope.types.stack.last.at("A")
def C : ot.ObjectType = sm.scope.types.stack.last.at("C")
def B : ot.ObjectType = sm.scope.types.stack.last.at("B")
def G : ot.ObjectType = sm.scope.types.stack.last.at("G")

// print("\nB.normalMeths: {B.normalMeths}")

print("\ntypeOf(typeNoded.value): {st.typeOf(typeNoded.value)}")


print("\nA.isSubtypeOf(B): {A.isSubtypeOf(B)} (false)")
print("\nB.isSubtypeOf(A): {B.isSubtypeOf(A)} (true)")

print("\nC.isSubtypeOf(B): {C.isSubtypeOf(B)} (false)")
print("\nB.isSubtypeOf(C): {B.isSubtypeOf(C)} (true)")

print("\nB.normalMeths: {B.normalMeths}")
print("\nC.normalMeths: {C.normalMeths}")
print("\nG: {G}")
print("\nG.normalMeths: {G.normalMeths}")