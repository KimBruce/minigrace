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
    "    m → Number\n"++
    "}\n"++
    "def a': A = object \{\n"++
    "    method m -> Number \{15\}\n"++
    "}\n"++
    "def b: Object = a' \n"++
    "def a: A = object \{\n"++
    "    method m → Number \{n\}\n"++
    "    method n → Number is confidential \{\n"++
    "        if (isMe(b)) then \{47\} else \{-20\}\n"++
    "    }\n"++
    "}\n"++
    "print (a.m)\n"

//Turns input into an abstract syntax tree (ast)
def tokens = lexer.lexString(input)
def module = parser.parse(tokens)
def inputTree = ir.resolve(module)

//Returns a list of AstNodes corresponding to each type
def nodes = inputTree.value

//Turns type nodes into ObjectTypes so the type checker can process them
def typeNodeA : ast.AstNode = nodes.at(1)
def defNodea' : ast.AstNode = nodes.at(2)
def defNOdeb : ast.AstNode = nodes.at(3)
def defNOdea : ast.AstNode = nodes.at(4)

def A : ot.ObjectType = ot.anObjectType.fromDType(typeNodeA.value) with (emptyList)
print("\nA: {A}")