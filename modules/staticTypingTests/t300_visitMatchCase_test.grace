dialect "minitest"
import "lexer" as lexer
import "parser" as parser
import "ast" as ast
import "util" as util
import "io" as io
import "StaticTyping" as st
import "ObjectTypeModule" as ot
import "identifierresolution" as ir

//Divided the input into testBlock objects so that each testBlock can be
//type checked independently by the astVisitor inside gradualTypesND
def input : String =
    "def testBlock1: Object = object \{\n" ++
    "   def value: String = \"Hello World\"\n" ++
    "\n" ++
    "   var specific: String := match(value)\n" ++
    "       case\{\"Hello World\" -> \"Hello World\" \}\n" ++
    "       case\{s:String -> \"\" \}\n" ++
    "\n" ++
    "   var general: String := match(value)\n" ++
    "       case\{s:String -> \"\" \}\n" ++
    "\n" ++
    "   var wildcard: String := match(value)\n" ++
    "       case\{_:Object -> \"\" \}\n" ++
    "\}\n" ++
    "\n" ++
    "def testBlock2: Object = object \{\n" ++
    "   def value: Number = 5\n" ++
    "\n" ++
    "   var result: String := match(value)\n" ++
    "       case\{\"Hello World\" -> \"Hello World\" \}\n" ++
    "       case\{s:String -> \"\" \}\n" ++
    "\}\n" ++
    "\n" ++
    "def testBlock3: Object = object \{\n" ++
    "   def value: String = \"Hello World\"\n" ++
    "\n" ++
    "   var result: String := match(value)\n" ++
    "       case\{\"Hello\" , \"World\" -> \"Hello World\" \}\n" ++
    "       case\{s:String -> \"\" \}\n" ++
    "\}\n" ++
    "\n" ++
    "def testBlock4: Object = object \{\n" ++
    "   def value: String | Number = \"Hello World\"\n" ++
    "\n" ++
    "   var result: String := match(value)\n" ++
    "       case\{s:String -> \"\" \}\n" ++
    "       case\{n:Number -> \"\" \}\n" ++
    "\}\n" ++
    "\n" ++
    "def testBlock5: Object = object \{\n" ++
    "   def value: String | Number = \"Hello World\"\n" ++
    "\n" ++
    "   var result: String := match(value)\n" ++
    "       case\{s:String -> \"\" \}\n" ++
    "       case\{b:Boolean -> \"\" \}\n" ++
    "\}\n" ++
    "\n" ++
    "def testBlock6: Object = object \{\n" ++
    "   def value: String = \"Hello World\"\n" ++
    "\n" ++
    "   var result: String | Boolean := match(value)\n" ++
    "       case\{\"Hello World\" -> \"Hello World\" \}\n" ++
    "       case\{s:String -> true \}\n" ++
    "\}\n" ++
    "\n" ++
    "def testBlock7: Object = object \{\n" ++
    "   def value: String = \"Hello World\"\n" ++
    "\n" ++
    "   var result: String | Boolean := match(value)\n" ++
    "       case\{\"Hello World\" -> \"Hello World\" \}\n" ++
    "       case\{s:String -> 5 \}\n" ++
    "\}\n" ++
    "\n" ++
    "def testBlock8: Object = object \{\n" ++
    "   var value: Number := 0\n" ++
    "\n" ++
    "   var result: Done := match(value)\n" ++
    "       case\{0 -> def foobar: Number = value + 1 \}\n" ++
    "       case\{n:Number -> def foobar: Number = value + 2 \}\n" ++
    "\}\n" ++
    "\n" ++
    "def testBlock9: Object = object \{\n" ++
    "   var value: Number := 0\n" ++
    "\n" ++
    "   var result: Done := match(value)\n" ++
    "       case\{0 -> def foobar: Number = value + 1 \}\n" ++
    "       case\{n:Number -> 2 \}\n" ++
    "\}\n" ++
    ""

//Turns input into an abstract syntax tree (ast)
def tokens = lexer.lexString(input)
def module = parser.parse(tokens)
def inputTree = ir.resolve(module)

//Returns a list of AstNodes corresponding to each testBlock
def nodes  = inputTree.value

testSuiteNamed "visitMatchCase tests" with {

  //type checker raises no Exception given legal match cases
  test "match specific, general, and wildcard cases" by {
    def blk1 = nodes.filter{n -> n.name.name == "testBlock1"}.first
    assert ({blk1.accept(st.astVisitor)}) shouldntRaise (Exception)
  }

  //test type-checking of non-variant return type
  test "returnType" by {
    def blk1 = nodes.filter{n -> n.name.name == "testBlock1"}.first
    def blk1Specific = blk1.value.value.at(2)

    //checks that the type of the var result is String
    assert(ot.anObjectType.fromDType(blk1Specific.dtype))
        shouldBe (ot.anObjectType.string)
  }

  test "matchee and param type-mismatch error" by {
    def blk2 = nodes.filter{n -> n.name.name == "testBlock2"}.first
    assert ({blk2.accept(st.astVisitor)}) shouldRaise (TypeError)
  }

  test "multiple params error" by {
    def blk3 = nodes.filter{n -> n.name.name == "testBlock3"}.first
    assert ({blk3.accept(st.astVisitor)}) shouldRaise (RequestError)
  }

  test "variant type matchee and params" by {
    def blk4 = nodes.filter{n -> n.name.name == "testBlock4"}.first
    assert({blk4.accept(st.astVisitor)}) shouldntRaise (Exception)

    def blk5 = nodes.filter{n -> n.name.name == "testBlock5"}.first
    assert ({blk5.accept(st.astVisitor)}) shouldRaise (TypeError)
  }

  test "variant return-type" by {
    def blk6 = nodes.filter{n -> n.name.name == "testBlock6"}.first
    assert ({blk6.accept(st.astVisitor)}) shouldntRaise (Exception)

    def blk7 = nodes.filter{n -> n.name.name == "testBlock7"}.first
    assert ({blk7.accept(st.astVisitor)}) shouldRaise (TypeError)
  }

  test "return-type Done" by {
    def blk8 = nodes.filter{n -> n.name.name == "testBlock8"}.first
    assert ({blk8.accept(st.astVisitor)}) shouldntRaise (Exception)

    def blk9 = nodes.filter{n -> n.name.name == "testBlock9"}.first
    assert ({blk9.accept(st.astVisitor)}) shouldntRaise (Exception)
  }
}
