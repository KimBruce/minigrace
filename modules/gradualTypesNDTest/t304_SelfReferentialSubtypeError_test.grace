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
    "type Foo = \{",
    "    a -> Foo",
    "    b -> Number",
    "\}",
    "type Bar = \{",
    "    a -> Bar",
    "\}",
    "class bar -> Bar \{",
    "    method a -> Bar \{ self \}",
    "\}",
    "def test : Foo = bar",
    ""
]

util.lines.addAll(input)

//Turns input into an abstract syntax tree (ast)
def tokens = lexer.new.lexinput(input)
def module = parser.parse(tokens)
def inputTree = ir.resolve(module)

testSuiteNamed "self-referential subtype test" with {
    test "self-referential not a subtype" by {
        assert({inputTree.accept(gt.astVisitor)}) shouldRaise (TypeError)
    }
}
