#pragma ExtendedLineups
#pragma noTypeChecks
dialect "none"
import "standardGrace" as sg

import "lexer" as lexer
import "parser" as parser
import "ObjectTypeModule" as ot
import "ast" as ast
inherit sg.methods

def collectionDec = [
   "type Collection⟦T⟧ = \{",
   "   isEmpty -> Boolean",
   "   size -> Number",
   "   first -> T",
   "   sizeIfUnknown(action: Function0⟦Number⟧) -> Number",
   "   do(action: Function1⟦T,Unknown⟧) -> Done",
   "   do(action:Function1⟦T, Unknown⟧) separatedBy(sep:Function0⟦Unknown⟧) -> Done",
   "   map⟦R⟧(unaryFunction:Function1⟦T, R⟧) -> Collection⟦T⟧",
   "   fold⟦R⟧(binaryFunction:Function2⟦R, T, R⟧) startingWith(initial:R) -> R",
   "   filter(condition:Function1⟦T, Boolean⟧) -> Collection⟦T⟧",
   "   ++(other: Collection⟦T⟧) -> Collection⟦T⟧",
   "\}",
   "var x:Collection⟦Number⟧",
   "x.first"]
def tokens = lexer.lexLines(collectionDec)
def parseTree: ast.moduleNode = parser.parse(tokens)
print(parseTree.kind)
print("\n29 parseTree.value(1) is "++
     "{parseTree.value.at(1).value.methods.at(2).rtype}")
def gType = ot.aGenericType.fromTypeDec(parseTree.value.at(1))
print("\n 31 gType has body {gType.oType}")
print("\n 32 gType is {gType}")
//def gType = ot.aGenericType.fromName("Collection")
//                parameters (list["T"]) objectType(oType)
//print ("gType is {gType}")
def nList = gType.apply(list[ast.identifierNode.new("Number",false)])
print ("\n37 nList is {nList}")
//print (parseTree.value.at(1).value)
//for (parseTree.value.at(1).value.methods) do {m ->
//   print(m.rtype)
//}
//print (parseTree.value.at(1).typeParams)
//print (parseTree.value.at(1).toGrace)

