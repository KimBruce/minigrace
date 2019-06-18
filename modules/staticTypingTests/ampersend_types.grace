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
    "type Point = \{\n" ++
    "    x -> Number\n" ++
    "}\n" ++
    "type ColorPoint = Point & interface \{\n" ++
    "    color -> String\n" ++
    "    m -> ColorPoint\n" ++
    "}\n" ++
    "type FatPoint = Point & interface \{\n" ++
    "    radius -> Number\n" ++
    "    m -> FatPoint\n" ++
    "}\n" ++
    "type Combo = ColorPoint & FatPoint\n" ++
    "class combo -> Combo \{\n" ++
    "    method color -> String \{ \"Color\" \}\n" ++
    "    method x -> Number \{ 47 \}\n" ++
    "    method radius -> Number \{ 0 \}\n" ++
    "    method m -> FatPoint \{ fat \}\n" ++
    "}\n" ++
    "class fat -> FatPoint \{\n" ++
    "    method radius -> Number \{ 1 \}\n" ++
    "    method m -> FatPoint \{ fat \}\n" ++
    "    method x -> Number \{ 2 \}\n" ++
    "}\n" ++
    "def d: Combo = combo\n"

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

def dobj = nodes.filter{n -> n.asString == "defdec d"}.first
def combotype = nodes.filter{n -> n.asString == "typedec Combo"}.first

def comboOT: share.ObjectType = ot.anObjectType.fromDType(combotype.value) with (emptyList)

print("\n53: scope.types.stack: {sc.scope.types.stack}")
print ("\n54:{comboOT.methList}")
print ("\n55: {ot.anObjectType.fromDType(dobj.dtype) with (emptyList)}")