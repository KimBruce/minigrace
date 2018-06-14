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
    "",
    "def testBlock4: Object = object \{",
    "   def value: String | Number = \"Hello World\"",
    "",
    "   var answer: String := match(value)",
    "       case\{s:String -> \"\" \}",
    "       case\{n:Number -> \"\" \}",
    "\}",
    "",
    "def testBlock5: Object = object \{",
    "   def value: String | Number = \"Hello World\"",
    "",
    "   var answer: String := match(value)",
    "       case\{s:String -> \"\" \}",
    "       case\{b:Boolean -> \"\" \}",
    "\}",
    "",
    "def testBlock6: Object = object \{",
    "   def value: String = \"Hello World\"",
    "",
    "   var answer: String | Boolean := match(value)",
    "       case\{\"Hello World\" -> \"Hello World\" \}",
    "       case\{s:String -> true \}",
    "\}",
    "",
    "def testBlock7: Object = object \{",
    "   def value: String = \"Hello World\"",
    "",
    "   var answer: String | Boolean := match(value)",
    "       case\{\"Hello World\" -> \"Hello World\" \}",
    "       case\{s:String -> 5 \}",
    "\}",
    "",
    "def testBlock8: Object = object \{",
    "   var value: Number := 0",
    "",
    "   var answer: Done := match(value)",
    "       case\{0 -> def foobar: Number = value + 1 \}",
    "       case\{n:Number -> def foobar: Number = value + 2 \}",
    "\}",
    "",
    "def testBlock9: Object = object \{",
    "   var value: Number := 0",
    "",
    "   var answer: Done := match(value)",
    "       case\{0 -> def foobar: Number = value + 1 \}",
    "       case\{n:Number -> 2 \}",
    "\}",
    ""
]

util.lines.addAll(input)

def tokens = lexer.new.lexinput(input)
def module = parser.parse(tokens)
def inputTree = ir.resolve(module)
def nodes  = inputTree.value

testSuiteNamed "visitMatchCase tests" with {

  test "match specific, general, and wildcard cases" by {
    def blk1 = nodes.filter{n -> n.name.name == "testBlock1"}.first
    assert ({blk1.accept(gt.astVisitor)}) shouldntRaise (Exception)
  }

  //Use testBlock1 to test type-checking of non-variant return type
  test "returnType" by {
    def blk1 = nodes.filter{n -> n.name.name == "testBlock1"}.first
    def blk1Answer = blk1.value.value.at(2)
    //checks that the type of the var answer is String
    assert(gt.anObjectType.fromDType(blk1Answer.dtype))
        shouldBe (gt.anObjectType.string)
  }

  test "matchee and param type-mismatch error" by {
    def blk2 = nodes.filter{n -> n.name.name == "testBlock2"}.first
    assert ({blk2.accept(gt.astVisitor)}) shouldRaise (TypeError)
  }

  test "multiple params error" by {
    def blk3 = nodes.filter{n -> n.name.name == "testBlock3"}.first
    assert ({blk3.accept(gt.astVisitor)}) shouldRaise (RequestError)
  }

  test "variant type matchee and params" by {
    def blk4 = nodes.filter{n -> n.name.name == "testBlock4"}.first
    assert({blk4.accept(gt.astVisitor)}) shouldntRaise (Exception)

    def blk5 = nodes.filter{n -> n.name.name == "testBlock5"}.first
    assert ({blk5.accept(gt.astVisitor)}) shouldRaise (TypeError)
  }

  test "variant return-type" by {
    def blk6 = nodes.filter{n -> n.name.name == "testBlock6"}.first
    assert ({blk6.accept(gt.astVisitor)}) shouldntRaise (Exception)

    def blk7 = nodes.filter{n -> n.name.name == "testBlock7"}.first
    assert ({blk7.accept(gt.astVisitor)}) shouldRaise (TypeError)
  }

  test "return-type Done" by {
    def blk8 = nodes.filter{n -> n.name.name == "testBlock8"}.first
    assert ({blk8.accept(gt.astVisitor)}) shouldntRaise (Exception)

    def blk9 = nodes.filter{n -> n.name.name == "testBlock9"}.first
    assert ({blk9.accept(gt.astVisitor)}) shouldntRaise (Exception)
  }
}
