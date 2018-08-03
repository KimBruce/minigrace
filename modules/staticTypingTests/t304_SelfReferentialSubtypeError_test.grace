dialect "minitest"
import "lexer" as lexer
import "parser" as parser
import "ast" as ast
import "util" as util
import "io" as io
import "StaticTyping" as st
import "identifierresolution" as ir

//Declare types such that types C and D are subtypes of type A
def input : String =
    "type Foo = \{\n" ++
    "    a -> Foo\n" ++
    "    b -> Number\n" ++
    "\}\n" ++
    "type Bar = \{\n" ++
    "    a -> Bar\n" ++
    "\}\n" ++
    "class bar -> Bar \{\n" ++
    "    method a -> Bar \{ self \}\n" ++
    "\}\n" ++
    "def test : Foo = bar\n" ++
    ""

//Turns input into an abstract syntax tree (ast)
def tokens = lexer.lexString(input)
def module = parser.parse(tokens)
def inputTree = ir.resolve(module)

testSuiteNamed "self-referential subtype test" with {
    test "self-referential not a subtype" by {
        assert({inputTree.accept(st.astVisitor)}) shouldRaise (st.DefError)
    }
}
