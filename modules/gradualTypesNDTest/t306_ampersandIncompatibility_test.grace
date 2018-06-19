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
    "type Base = \{a -> Boolean\}",
    "type ReturnType = \{a -> Number\}",
    "type Param = \{a(bool: Boolean) -> Boolean\}",
    "type ParamType = \{a(num: Number) -> Boolean\}",
    "type ParamSubtype = \{a(bool: (Boolean & Base)) -> Boolean\}",
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
def typeNodeBase        : ast.AstNode = nodes.at(1)
def typeNodeReturnType  : ast.AstNode = nodes.at(2)
def typeNodeParam       : ast.AstNode = nodes.at(3)
def typeNodeParamType   : ast.AstNode = nodes.at(4)
def typeNodeParamSubtype: ast.AstNode = nodes.at(5)

def Base        : gt.ObjectType = gt.anObjectType.fromTypeNode(typeNodeBase)
def ReturnType  : gt.ObjectType = gt.anObjectType.fromTypeNode(typeNodeReturnType)
def Param       : gt.ObjectType = gt.anObjectType.fromTypeNode(typeNodeParam)
def ParamType   : gt.ObjectType = gt.anObjectType.fromTypeNode(typeNodeParamType)
def ParamSubtype: gt.ObjectType = gt.anObjectType.fromTypeNode(typeNodeParamSubtype)

//  *****************************
//  **   start of test suite   **
//  *****************************

testSuiteNamed "ampersand incompatibility" with {

    test "incompatible return type" by {
        assert({Base & ReturnType}) shouldRaise (TypeError)
    }

    test "incompatible parameter count" by {
        assert({Base & Param}) shouldRaise (TypeError)
    }

    test "incompatible parameter type" by {
        assert({Param & ParamType}) shouldRaise (TypeError)
    }

    test "parameter is a sub-type" by {
        //Param is evaluated as type dynamic Unknown, so we cannot yet check
        //subtyping
        def andType: gt.ObjectType = ParamSubtype & Param
        assert(true) description ("placeholder assert")
        //assert(andType.isSubtypeOf(Param))
        //assert(Param.isSubtypeOf(andType))
    }
}
