dialect "minitest"
import "lexer" as lexer
import "parser" as parser
import "ast" as ast
import "util" as util
import "io" as io
import "gradualTypesND" as gt
import "identifierresolution" as ir

def input = sequence [
    "def testBlock1: Object = object \{",
    "   def value: String = \"Hello World\"",
    "",
    "   var specific: String := match(value)",
    "       case\{\"Hello World\" -> \"Hello World\" \}",
    "       case\{s:String -> \"\" \}",
    "",
    "   var general: String := match(value)",
    "       case\{s:String -> \"\" \}",
    "",
    "   var wildcard: String := match(value)",
    "       case\{_:Object -> \"\" \}",
    "\}",
    "",
    "def testBlock2: Object = object \{",
    "   def value: Number = 5",
    "",
    "   var answer: String := match(value)",
    "       case\{\"Hello World\" -> \"Hello World\" \}",
    "       case\{s:String -> \"\" \}",
    "\}",
    "",
    "def testBlock3: Object = object \{",
    "   def value: String = \"Hello World\"",
    "",
    "   var answer: String := match(value)",
    "       case\{\"Hello\" , \"World\" -> \"Hello World\" \}",
    "       case\{s:String -> \"\" \}",
    "\}",
    ""
]

util.lines.addAll(input)

def tokens = lexer.new.lexinput(input)
def module = parser.parse(tokens)
def inputTree = ir.resolve(module)
def nodes  = inputTree.value

print "{nodes}"

//TODO rename everything

testSuiteNamed "visitMatchCase tests" with {
  test "match specific case" by {
    //print ("{nodes.first.value}")
    //print ("{nodes.first.name.name}")

    def firstDef = nodes.filter{n -> n.name.name == "testBlock1"}.first
    assert ({firstDef.accept(gt.astVisitor)})
        shouldntRaise (TypeError)
  }

  test "returnType" by {
    def firstDef = nodes.filter{n -> n.name.name == "testBlock1"}.first
    def block1Answer = firstDef.value.value.at(2)
    //checks that the type of the var answer is String
    assert(gt.anObjectType.fromDType(block1Answer.dtype))
        shouldBe (gt.anObjectType.string)
  }

  test "paramType mismatch error" by {
    def firstDef = nodes.filter{n -> n.name.name == "testBlock2"}.first
    assert ({firstDef.accept(gt.astVisitor)}) shouldRaise (TypeError)
  }

  test "multiple params error" by {
    def firstDef = nodes.filter{n -> n.name.name == "testBlock3"}.first
    assert ({firstDef.accept(gt.astVisitor)}) shouldRaise (RequestError)
  }




}
