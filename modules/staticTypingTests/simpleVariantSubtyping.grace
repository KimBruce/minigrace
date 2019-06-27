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
    "type A = String | Number\n"++
    "type B = String | Boolean\n"

//Turns input into an abstract syntax tree (ast)
def tokens = lexer.lexString(input)
def module = parser.parse(tokens)
def inputTree = ir.resolve(module)

//Returns a list of AstNodes corresponding to each type
def nodes = inputTree.value

//Turns type nodes into ObjectTypes so the type checker can process them
def typeNodeA : ast.AstNode = nodes.at(1)
def typeNodeB : ast.AstNode = nodes.at(2)

inputTree.accept(st.astVisitor)

def A : ot.ObjectType = sm.scope.types.stack.last.at("A")
def B : ot.ObjectType = sm.scope.types.stack.last.at("B")

print("\nA.isSubtypeOf(B): {A.isSubtypeOf(B)}")
print("\nB.isSubtypeOf(A): {B.isSubtypeOf(A)}")

print("\nA.isSubtypeOf(B): {A.isSubtypeOf(B)}")
print("\nB.isSubtypeOf(A): {B.isSubtypeOf(A)}")