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
    "type F⟦T⟧ = interface \{\n" ++
    "   m → T \n" ++
    "\}\n" ++

    "def o: F⟦Number⟧ = object \{\n" ++
    "   method m → Number \{7\}\n" ++
    "\}\n" ++

    "print(o.m)\n" //++

    // "type G⟦T⟧ = interface \{\n" ++ 
 //    "    m(n:T) → T\n" ++ 
    // "\}\n" ++

    // "def p: G⟦String⟧ = object \{\n" ++
 //    "    method m(k: String) → String \{"hello"\}\n" ++
    // "\}\n" ++

    // "def r: String = p.m(\"test\")\n" ++

    // "print(r)\n"  
    

//Turns input into an abstract syntax tree (ast)


def tokens = lexer.lexString(input)
def module = parser.parse(tokens)
def inputTree = ir.resolve(module)

def nodes  = inputTree.value

def typeDecNode = nodes.first
def defDecNode = nodes.at(2)
def callNode = nodes.at (3)


print (nodes)

print (defDecNode.value)
print (defDecNode.value.body.at(1).kind)
//print (defDecNode.value.methList)


print (defDecNode.dtype)










// print ("generics : {callNode.generics}")
// print (callNode.parts)
// print (callNode.receiver)
// print (callNode.numArgs)
// print ("generics : {callNode.generics}")
// print (callNode.numTypeArgs)




// print (nodes.first.dtype) //dType is the return type of the method
// print (nodes.first.typeParams)



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