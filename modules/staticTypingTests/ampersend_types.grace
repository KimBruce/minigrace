dialect "minitest"
import "lexer" as lexer
import "parser" as parser
import "ast" as ast
import "util" as util
import "io" as io
import "StaticTyping" as st
import "ObjectTypeModule" as ot
import "identifierresolution" as ir
import "ScopeModule" as sc
import "SharedTypes" as share

def input : String =
    "type A = \{\n" ++
    "   m -> String\n" ++
    "   n -> Number\n" ++
    "}\n" ++
    "type B = \{\n" ++
    "   m -> String\n" ++
    "}\n" 

// Turns input into an abstract syntax tree (ast)
def tokens = lexer.lexString(input)
def module = parser.parse(tokens)
def inputTree = ir.resolve(module)

// Returns a list of AstNodes
def nodes  = inputTree.value

print(nodes)

for(nodes) do { node ->
    print("\nnode.asString: {node.asString}")
}


def Anode = nodes.filter{n -> n.asString == "typedec A"}.first
def Bnode = nodes.filter{n -> n.asString == "typedec B"}.first

def AOT: share.ObjectType = ot.anObjectType.fromDType(Anode.value) with (emptyList)
def BOT: share.ObjectType = ot.anObjectType.fromDType(Bnode.value) with (emptyList)


print(AOT.methList)
print ("1: " ++ AOT.isSubtypeOf(BOT))
print ("2: " ++ BOT.isSubtypeOf(AOT))
print ("3: " ++ (BOT == AOT))
print ("4: " ++ AOT.isSubtypeOf(AOT))





// print("\n53: scope.types.stack: {sc.scope.types.stack}")
// print ("\n54:{comboOT.methList}")
// print ("\n55: {ot.anObjectType.fromDType(dobj.dtype) with (emptyList)}")