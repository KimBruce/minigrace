dialect "minitest"
import "lexer" as lexer
import "parser" as parser
import "ast" as ast
import "util" as util
import "io" as io
import "StaticTyping" as st
import "identifierresolution" as ir
import "ObjectTypeModule" as ot 
import "SharedTypes" as sh
import "ScopeModule" as sm


def input : String = 
    "method m[[T]] (p:T) -> T \{\n" ++
    "   p\n" ++
    "\}\n" //++
    //"(m⟦String⟧(\"test succeeded\"))\n" 
    //print (m⟦String⟧(\"test succeeded\"))\n"
    

//Turns input into an abstract syntax tree (ast)
def tokens = lexer.lexString(input)
def module = parser.parse(tokens)
def inputTree = ir.resolve(module)

def nodes  = inputTree.value

def methodNode = nodes.first
//def callNode = nodes.at(2)


print ("method node = {nodes.first}")
//print ("generics : {callNode.generics}")
// print (callNode.parts)
// print (callNode.receiver)
// print (callNode.numArgs)
// print ("generics : {callNode.generics}")
// print (callNode.numTypeArgs)




print (nodes.first.dtype) //dType is the return type of the method
print (nodes.first.typeParams)



// print (nodes)
// print ("this one {nodes.first.typeParams}")
// print ("this one {nodes.first.pretty(0)}")
// print ("this one {nodes.first.signature.first}")
// print (nodes.first.kind)
// print (nodes.at(2).kind)
//print (nodes.at(1).generics)
//print(nodes.at(2).generics)
//print(sm.scope.generics)













// def typeFoo  : ast.AstNode = nodes.at(1)
// def typeBar : ast.AstNode = nodes.at(2)
// def typebar  : ast.AstNode = nodes.at(3)



// testSuiteNamed "self-referential subtype test" with {
//     test "self-referential not a subtype" by {
//         assert({inputTree.accept(st.astVisitor)}) shouldRaise (TypeError)
//     }
// }