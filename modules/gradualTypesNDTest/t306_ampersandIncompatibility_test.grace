dialect "minitest"
import "lexer" as lexer
import "parser" as parser
import "ast" as ast
import "util" as util
import "io" as io
import "gradualTypesND" as gt
import "identifierresolution" as ir

//Declare types such that types C and D are subtypes of type A
def input : String =
    "type Base = \{a -> Boolean\}\n" ++
    "type ReturnType = \{a -> Number\}\n" ++
    "type Param = \{a(bool: Boolean) -> Boolean\}\n" ++
    "type ParamType = \{a(num: Number) -> Boolean\}\n" ++
    ""

//Turns input into an abstract syntax tree (ast)
def tokens = lexer.new.lexString(input)
def module = parser.parse(tokens)
def inputTree = ir.resolve(module)

//Returns a list of AstNodes corresponding to each type
def nodes  = inputTree.value

//Turns type nodes into ObjectTypes so the type checker can process them
def typeNodeBase        : ast.AstNode = nodes.at(1)
def typeNodeRetType     : ast.AstNode = nodes.at(2)
def typeNodeParam       : ast.AstNode = nodes.at(3)
def typeNodeParamType   : ast.AstNode = nodes.at(4)

def Base        : gt.ObjectType = gt.anObjectType.fromDType(typeNodeBase.value)
def RetType     : gt.ObjectType = gt.anObjectType.fromDType(typeNodeRetType.value)
def Param       : gt.ObjectType = gt.anObjectType.fromDType(typeNodeParam.value)
def ParamType   : gt.ObjectType = gt.anObjectType.fromDType(typeNodeParamType.value)

//  *****************************
//  **   start of test suite   **
//  *****************************

testSuiteNamed "ampersand incompatibility" with {

    test "incompatible return type" by {
        assert({Base & RetType}) shouldRaise (TypeError)
    }

    test "incompatible parameter count" by {
        assert({Base & Param}) shouldntRaise (TypeError)
    }

    test "incompatible parameter type" by {
        assert({Param & ParamType}) shouldRaise (TypeError)
    }
}
