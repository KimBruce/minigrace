#pragma noTypeChecks
#pragma ExtendedLineups
import "util" as util
import "identifierKinds" as k

// This module contains classes and pseudo-classes for all the AST nodes used
// in the parser. Because of the limitations of the class syntax, classes that
// need more than one method are written as object literals containing methods.
// Each node has a different signature according to its function, but the
// common interface is given by type ASTNode
//
// Most nodes also contain a "value" field, with varied type, holding the `main value`
// in the node.  This field is confusing and should be appropriately re-named in
// each case. Some nodes contain other fields for their specific use: while has
// both a value (the condition) and a body, for example.

type Position = type {
    line -> Number
    column -> Number
    > -> Boolean
    ≥ -> Boolean
    == -> Boolean
    < -> Boolean
    ≤ -> Boolean
}
type Range = type {
    start -> Position
    end -> Position
}

class line (l:Number) column (c:Number) -> Position
class start (s:Position) end (e:Position) -> Range

def noPosition: Position is public
def emptyRange: Range is public

positionOfNext(needle:String) after (pos:Position) -> Position

lineLength -> Number
def uninitialized = Singleton.named "uninitialized"
listMap(l: Unknown, b: Unknown) ancestors(a: Unknowns) -> Unknown is confidential 
maybeMap(n: Unknown, b: Unknown) ancestors(as: Unknown) -> Unknown is confidential 
maybeListMap(n: Unknown, b: Unknown) ancestors(as: Unknown) -> Unknown is confidential 

// look up AST tree
type AncestorChain = {
    // do you have an ancestor
    isEmpty -> Boolean
    asString -> String
    // add yourself to ancestor chain
    extend -> AncestorChain
}

type ExtendedAncestorChain = AncestorChain & type {
    forbears -> AncestorChain
    parent -> Unknown
    grandparent -> Unknown
    // search up chain looking for match to cond
    suchThat(cond: Boolean) ifAbsent (action: Block0[[Unknown]]) -> Unknown
    // extend with node n (then n is the parent)
    extend(n: AstNode) -> AncestorChain
}

type AncestorChainFactory = {
    empty -> AncestorChain
    with(n: Unknown) -> AncestorChain
    cons(p: Unknown) onto (as: AncestorChain) -> AncestorChain
}

def ancestorChain: AncestorChainFactory is public
def emptySeq: Sequence
// Types of nodes:
// AstNode, BaseNode, IfNode, BlockNode, TryCatchNode, MatchCaseNode

type AstNode = type {
    kind -> String
        // Used for pseudo-instanceof tests.
    register -> String
        // Used in the code generator on to name the resulting object
    line -> Number
        // The source line the node came from
    line:=(ln:Number)
    linePos -> Number
    linePos:=(lp:Number)
    scope -> SymbolTable
        // The symbolTable for names defined in this node and its sub-nodes
    pretty(n:Number) -> String
        // Pretty-print-string of node at depth n
    comments -> AstNode
        // Comments associated with this node
    range -> Range
    start -> Position
    end -> Position
}

// rename as AstNode
type BaseNode = AstNode & {
    setLine (l: Number) col (c: Number) -> Self
    setPositionFrom (tokenOrNode) -> Self
    setStart(p: Position) -> Self
    start -> Position
    end -> Position
    range -> Position
    == (other: AstNode) -> Boolean       // for usesAsType
    // a use of an identifier (not declaration)
    isAppliedOccurenceOfIdentifier -> Boolean
    isMatchingBlock  -> Boolean
    isFieldDec  -> Boolean
    isInherits  -> Boolean
    isLegalInTrait -> Boolean
    isMember -> Boolean
    isMethod  -> Boolean
    isExecutable -> Boolean
    isCall -> Boolean
    isComment -> Boolean
    isClass -> Boolean    // is an expression that returns a fresh object
    inClass -> Boolean    // object in a syntactic class definiton
    isTrait -> Boolean    // is an expression that returns a trait object
    inTrait -> Boolean    // object in a syntactic trait definition
    isBind -> Boolean
    isReturn -> Boolean
    isSelf -> Boolean
    isSuper -> Boolean
    isPrelude -> Boolean
    isOuter -> Boolean
    isSelfOrOuter -> Boolean
    isBlock -> Boolean
    isObject -> Boolean
    isIdentifier -> Boolean
    isDialect -> Boolean
    isImport -> Boolean
    isTypeDec -> Boolean
    isExternal -> Boolean
    isFresh -> Boolean
    isConstant -> Boolean
    canInherit -> Boolean
    returnsObject -> Boolean
    // request w/implicit receiver
    isImplicit -> Boolean
    usesAsType(aNode) -> Boolean
    hash { line.hash * linePos.hash }
    asString -> String
    nameString -> String
    isWritable -> Boolean
    isReadable -> Boolean
    isPublic -> Boolean
    isConfidential -> Boolean
    // type may not be right.  Only works on things with dtype.
    decType -> IdentifierNode
    isSimple -> Boolean  // needs no parens when used as receiver
    isDelimited -> Boolean  // needs no parens when used as argument
    description -> String
    accept(visitor: ASTVisitor) -> Done
    accept(visitor: ASTVisitor) from (as:AncestorChain) -> Done 
    scope:=(st: SymbolTable) -> Done
    
    setScope(st: SymbolTable) -> Self
    shallowCopyFieldsFrom(other: SymbolTable) -> Self
    postCopy(other:SymbolTable) -> Self
    prettyPrefix(depth: Number) -> String 
    basePretty(depth: Number) -> String
    pretty(depth: Number) -> String
    deepCopy -> AncestorChain
    // object enclosing this node:  ModuleNode | ObjectType (FIX)
    enclosingObject -> SymbolTable
    addComment(cmtNode: CommentNode) -> Done
    addComments(cmtNodeList: List[CommentNode]]) -> Done
    statementName -> String
    shallowCopy -> Self      // confidential?? Not in baseNode, should be

}

class baseNode ->BaseNode

// Implicit receiver of request w/out explicit receiver
type Implicit = BaseNode & type {
    toGrace(depth: Number) -> String
    map(blk) ancestors(as) -> Self
}

def implicit: Implicit is public

def nullNode: Implicit is public

// Symbol table associated with a node
type SymbolTable = {
    node -> AstNode   
    node:=(newNode:AstNode) -> Done
    addNode(n: AstNode) as (kind:String) -> Done
    thatDefines(name:String) ifNone (Block0[[Done]]) -> SymbolTable
    enclosingObjectScope -> SymbolTable
    variety -> String
    ==(other: SymbolTable) -> Boolean
}

// place holder symbol table.  Will be replaced
def fakeSymbolTable: SymbolTable is public

// type of factory that creates ifNode
type IfNodeFactory = {
    new (cond:AstNode, thenblock':BlockNode,elseBlock':BlockNode) -> IfNode
}

type IfNode = Implicit & type {
   // condition of if-statement
   value -> Cond
   value:= (cond: Cond) -> Done
   thenblock -> BlockNode
   thenblock:=(block: BlockNode) -> Done
   elseblock -> BlockNode
   elseblock:=(block: BlockNode) -> Done
   // not clear what handledIdentifiers are
   handledIdentifiers -> Boolean
   handledIdentifiers:=(handled: Boolean) -> Done
}
   
def ifNode: IfNodeFactory is public

type BlockNodeFactory = {
   new(params':List[[IdentifierNode] ,body': List[[AstNode]]) -> BlockNode
}

type BlockNode = Implicit & type {
   value -> String  // "block"
   params ->  List[[AstNode]]?
   params:=(params': List[[AstNode]]) -> Done
   body ->  List[[AstNode]]
   body:=(body': List[[AstNode]]) -> Done
   selfclosure -> Boolean
   matchingPattern -> Boolean
   extraRunTimeData -> Boolean
   isEmpty -> Boolean
   isntEmpty -> Boolean
   declarationKindWithAncestors(as:AncestorChain) -> k.T
   returnsObject -> Boolean
   returnedObjectScope -> SymbolTable
   parametersDo(b:Block1[[IdentifierNode,Done]]) -> Done
   shallowCopy -> Self
}

def blockNode: BlockNodeFactory is public

type TryCatchNodeFactory = {
   new(block: BlockNode, cases:List[[AstNode]], finally:AstNode) -> TryCatchNode
}

type TryCatchNode = Implicit & type {
   value -> BlockNode   // block to be executed
   value:= (block':BlockNode) -> Done
   cases -> :List[[AstNode]]  // catch clauses
   cases:= (cases'::List[[AstNode]]) -> Done
   finally -> AstNode   // finally clause
   finally:= (finally':AstNode) -> Done



def tryCatchNode: TryCatchNodeFactory is public

type MatchCaseNodeFactory = {
    new (matchee':AstNode, cases':List[[AstNode]], elsecase':AstNode) -> MatchCaseNode
}

type MatchCaseNode = BaseNode & type {
   value -> AstNode // expression to be matched
   value:= (block':AstNode) -> Done
   cases -> List[[BlockNode]]
   cases:= (cases': List[[BlockNode]]) -> Done

   // elsecase no longer used so always false
   elsecase -> AstNode
   elsecase:= (elsecase':AstNode) -> Done
   matchee -> AstNode  // same as value
}

def matchCaseNode: MatchCaseNodeFactory is public

type MethodTypeNodeFactory = {
   new (signature':List[[SignaturePartNode]], rtype': TypeNode) -> MethodTypeNode
}

type MethodTypeNode = BaseNode &
   signature -> List[[SignaturePartNode]]
   signature:= (signature':List[[SignaturePartNode]]) -> Done
   rtype -> TypeNode
   rtype:= (rtype':TypeNode) -> Done
   typeParams -> Boolean
   typeParams:= (isTypeParams':Boolean) -> Done
   cachedIdentifier -> ??       // uninitialized
   cachedIdentifier:= (id':??) -> Done
   value -> ???   // same as cachedIdentifier
   parametersDo(b:???) -> ???
   scope:= (st: SymbolTable)
}

def methodTypeNode: MethodTypeNodeFactory is public

type TypeLiteralNodeFactory = {
    new(methods':???, types':???) -> TypeLiteralNode
}

type TypeLiteralNode = BaseNode & type{
   methods -> ??
   methods:= (methods':??) -> Done
   types -> ??
   types:= (types':??) -> Done
   nominal -> Boolean
   nominal:= (nominal':Boolean) -> Done
   anonymous -> Boolean
   anonymous:= (anonymous':Boolean) -> Done
   value -> String
   value:= (value':String) -> Done
   name -> String
   name:=(name':String) -> Done
}

def typeLiteralNode: TypeLiteralNodeFactory is public

type TypeDecNodeFactory = {
    new(name':String, typeValue':String) -> TypeDecNode
}

type TypeDecNode = BaseNode & type{
   methods -> ??
   methods:= (methods':??) -> Done
   types -> ??
   types:= (types':??) -> Done
   nominal -> Boolean
   nominal:= (nominal':Boolean) -> Done
   anonymous -> Boolean
   anonymous:= (anonymous':Boolean) -> Done
   value -> String
   value:= (value':String) -> Done
   name -> ???
   name:=(name':??) -> Done
   annotations -> List[[???]]
   annotations:=(annotations':??) -> Done
   typeParams -> Boolean
   typeParams:= (isTypeParams':Boolean) -> Done

}

def typeDecNode: TypeDecNodeFactory is public

type MethodNodeTypeFactory = {
    new (signature':??, body: ???, dtype: ???) -> MethodNodeType
    new (signature':??, body: ???, dtype: ???) scope(s:???) -> MethodNodeType
}

type MethodNode = BaseNode & type {
   body ->  ???
   body:=(body': ???) -> Done
   dtype ->  ???
   dtype:=(dtype': ???) -> Done
   typeParams -> Boolean
   typeParams:= (isTypeParams':Boolean) -> Done
   selfclosure -> Boolean
   selfclosure:= (selfclosure':Boolean) -> Done
   typeParams -> Boolean
   typeParams:= (isTypeParams':Boolean) -> Done
   annotations -> List[[???]]
   annotations:=(annotations':??) -> Done
   usesClassSyntax -> List[[???]]
   usesClassSyntax:=(usesClassSyntax':??) -> Done
   cachedIdentifier -> ??       // uninitialized
   cachedIdentifier:= (id':??) -> Done
   isBindingOccurrence -> Boolean
   ilkName -> ???
   appliedOccurrence -> Self
   numParams -> Number
   endPos -> Number
   asIdentifier -> ???
   value -> ???
   canonicalName -> String
   hasParams -> Boolean
   numParamLists -> Number
   hasTypeParams -> Boolean
   needsArgChecks -> Boolean
   declarationKindWithAncestors(as:???) -> k.T
   returnedObjectScope -> ???
   parametersDo(b:???) -> ???
   shallowCopy -> ???
   resultExpression -> ???

def methodNode: MethodNode is public

type CallNodeFactory = {
   new(receiver:??, parts:??) -> CallNode
   new(receiver:??, parts:??) scope(s: SymbolTable) -> CallNode
}

type CallNode = BaseNode && {
   with -> ???   // same type as parts
   with:=(parts':???)
   generics -> Boolean
   generics:= (generics':Boolean) -> Done
   isPattern -> Boolean
   isPattern:= (isPattern':Boolean) -> Done
   receiver -> ??
   receiver:= (receiver':??) -> Done
   isSelfRequest -> Boolean
   isSelfRequest:= (isSelfRequest':Boolean) -> Done
   isTailCall -> Boolean
   isTailCall:= (isTailCall':Boolean) -> Done
   isFresh -> Boolean
   generics:= (isFresh':Boolean) -> Done
   onSelf -> Self
   canonicalName -> String
   isCopy -> Boolean
   isClone -> Boolean
   returnedObjectScope -> ???
   arguments -> List[[??]]
   argumentsDo(b:???) -> ???
   shallowCopy -> ???
   asIdentifier -> ???
   shallowCopy -> ???
}
   
def callNode:CallNodeFactory is public

def moduleNode is public = object {
    method body(b) named(n) scope(s) {
        def result = body(b)
        result.name := n
        result.scope := s
        result
    }
    method body(b) named(n) {
        def result = body(b)
        result.name := n
        result
    }
    class body(b) {
        inherit objectNode.new(b, false)
        def kind is public = "module"
        def sourceLines = util.lines
        var theDialect is public := dialectNode.new "standardGrace"
        line := 0       // because the module is always implicit
        linePos := 0
        var imports is public := [ ]

        method end -> Position {
            line (util.lines.size) column (util.lines.last.size)
        }
        method isModule -> Boolean
        method isTrait -> Boolean
        method returnsObject -> Boolean
        method importsDo(action) {
            value.do { o ->
                if (o.isExternal) then { action.apply(o) }
            }
        }
        method externalsDo(action) {
            if (theDialect.value ≠ "none") then {
                action.apply(theDialect)
            }
            value.do { o ->
                if (o.isExternal) then { action.apply(o) }
            }
        }
        method accept(visitor : ASTVisitor) from(as) {
            if (visitor.visitModule(self) up(as)) then {
                def newChain = as.extend(self)
                theDialect.accept(visitor) from (newChain)
                if (false != self.superclass) then {
                    self.superclass.accept(visitor) from(newChain)
                }
                for (self.value) do { x ->
                    x.accept(visitor) from(newChain)
                }
            }
        }
        method map(blk) ancestors(as) {
            var n := shallowCopy
            def newChain = as.extend(n)
            n.theDialect := theDialect.map(blk) ancestors(newChain)
            n.value := listMap(value, blk) ancestors(newChain)
            n.superclass := maybeMap(superclass, blk) ancestors(newChain)
            n.usedTraits := listMap(usedTraits, blk) ancestors(newChain)
            blk.apply(n, as)
        }
        method basePretty(depth) {
            def spc = "  " * (depth+1)
            prettyPrefix(depth) ++ "\n" ++
                "{spc}{theDialect.toGrace 0}"
        }
        method shallowCopy {
            moduleNode.body(emptySeq).shallowCopyFieldsFrom(self)
        }
        method postCopy(other) {
            imports := other.imports
            theDialect := other.theDialect
            // copy the field of moduleNode

            name := other.name
            value := other.value
            superclass := other.superclass
            usedTraits := other.usedTraits
            inClass := other.inClass
            inTrait := other.inTrait
            annotations := other.annotations
            // copy the fields of objectNode — should be an alias to objectNode.postCopy

            self
        }
    }
}
def objectNode is public = object {
    method body(b) named(n) scope(s) {
        def result = new(b, false)
        result.name := n
        result.scope := s
        result
    }
    method body(b) named(n) {
        body(b) named(n) scope(fakeSymbolTable)
    }
    class new(b, superclass') {
        inherit baseNode
        def kind is public = "object"
        var value is public := b
        var superclass is public := superclass'
        var usedTraits is public := [ ]
        var name is public := "object"
        var inClass is public := false
        var inTrait is public := false
        var myLocalNames := false
        var annotations is public := [ ]

        method end -> Position {
            if (value.isEmpty.not) then {
                return positionOfNext "}" after (value.last.end)
            }
            def iEnd = if (false == superclass) then { noPosition } else { superclass.end }
            def tEnd = if (usedTraits.isEmpty) then { noPosition } else { usedTraits.end }
            if (iEnd ≠ tEnd) then {
                positionOfNext "}" after (max(iEnd, tEnd))
            } else {
                positionOfNext "}" after (start)
            }
        }
        method description -> String {
            if (isTrait) then {
                "{kind} (trait)"
            } elseif { inClass } then {
                "{kind} (class)"
            } else {
                kind
            }
        }
        method isFresh -> Boolean     // the epitome of freshness!
        method isTrait {
            // answers true if this object qualifies to be a trait, whether
            // or not it was declared with the trait syntax

            if (inTrait) then { return true }
            if (false != superclass) then { return false }
            value.do { each ->
                if (each.isLegalInTrait.not) then { return false }
            }
            return true
        }

        method localNames -> Set⟦String⟧ {
            // answers the names of all of the methods defined directly in
            // this object.  Inherited names are _not_ included.
            if (false == myLocalNames) then {
                myLocalNames := emptySet
                value.do { node ->
                    if (node.isFieldDec || node.isMethod) then {
                        myLocalNames.add(node.nameString)
                    }
                }
            }
            myLocalNames
        }

        method parentsDo(action) {
            // iterate over my superclass and my used traits

            if (false != superclass) then { action.apply(superclass) }
            usedTraits.do { t -> action.apply(t) }
        }

        method methodsDo(action) {
            // iterate over my method declarations

            value.do { o ->
                if (o.isMethod) then { action.apply(o) }
            }
        }

        method executableComponentsDo(action) {
            // iterate over my executable code, including
            // field declarations (since they may have initializers)
            value.do { o ->
                if (o.isExecutable) then { action.apply(o) }
            }
        }

        method scope:=(st) {
            // sets up the 2-way conection between this node
            // and the symbol table that defines the scope that I open.
            symbolTable := st
            st.node := self
        }
        method body { value }
        method returnsObject -> Boolean
        method returnedObjectScope { scope }
        method canInherit { inTrait.not }   // an object can inherit if not in a trait
        method canUse -> Boolean
        method isObject -> Boolean
        method accept(visitor : ASTVisitor) from(as) {
            if (visitor.visitObject(self) up(as)) then {
                def newChain = as.extend(self)
                if (false != superclass) then {
                    superclass.accept(visitor) from(newChain)
                }
                usedTraits.do { t -> t.accept(visitor) from(newChain) }
                value.do { x -> x.accept(visitor) from(newChain) }
            }
        }
        method nameString {
            if (name == "object") then {
                "object_on_line_{line}"
            } else {
                name
            }
        }
        method map(blk) ancestors(as) {
            var n := shallowCopy
            def newChain = as.extend(n)
            n.value := listMap(value, blk) ancestors(newChain)
            n.superclass := maybeMap(superclass, blk) ancestors(newChain)
            n.usedTraits := listMap(usedTraits, blk) ancestors(newChain)
            blk.apply(n, as)
        }
        method pretty(depth') {
            var depth := depth'
            def spc = "  " * (depth+1)
            var s := basePretty(depth)
            s := "{s}\n{spc}Name: {self.name}"
            if (false != self.superclass) then {
                s := s ++ "\n" ++ spc ++ "Superclass: " ++
                        self.superclass.pretty(depth + 1)
            }
            if (usedTraits.isEmpty.not) then {
                s := s ++ "\n" ++ spc ++ "Traits:"
                usedTraits.do { t ->
                    s := "{s}\n{spc}  {t.pretty(depth + 1)}"
                }
            }
            value.do { x ->
                s := s ++ "\n"++ spc ++ x.pretty(depth + 1)
            }
            s
        }
        method toGrace(depth : Number) -> String {
            def spc = "    " * depth
            var s := "object \{"
            if (inTrait) then { s := s ++ "   // trait" }
            if (inClass) then { s := s ++ "   // class" }
            if (false != superclass) then {
                s := s ++ "\n" ++ superclass.toGrace(depth + 1)
            }
            usedTraits.do { t -> s := s ++ "\n" ++ t.toGrace(depth + 1) }
            value.do { x ->
                s := s ++ "\n" ++ spc ++ "    " ++ x.toGrace(depth + 1)
            }
            s := s ++ "\n" ++ spc ++ "\}"
            s
        }
        method shallowCopy {
            objectNode.new(emptySeq, false).shallowCopyFieldsFrom(self)
        }
        method postCopy(other) {
            name := other.name
            value := other.value
            superclass := other.superclass
            usedTraits := other.usedTraits
            inClass := other.inClass
            inTrait := other.inTrait
            annotations := other.annotations
            self
        }
        method asString {
            "object {nameString}"
        }
    }
}
def arrayNode is public = object {
  class new(values) {
    inherit baseNode
    def kind is public = "array"
    var value is public := values
    method end -> Position {
        if (value.isEmpty) then {
            positionOfNext "]" after (start)
        } else {
            positionOfNext "]" after (value.last.end)
        }
    }
    method accept(visitor : ASTVisitor) from(as) {
        if (visitor.visitArray(self) up(as)) then {
            def newChain = as.extend(self)
            for (self.value) do { ax ->
                ax.accept(visitor) from(newChain)
            }
        }
    }
    method map(blk) ancestors(as) {
        var n := shallowCopy
        def newChain = as.extend(n)
        n.value := listMap(value, blk) ancestors(newChain)
        blk.apply(n, as)
    }
    method pretty(depth) {
        def spc = "  " * (depth+1)
        var s := basePretty(depth)
        for (self.value) do { ax ->
            s := s ++ "\n"++ spc ++ ax.pretty(depth+1)
        }
        s
    }
    method toGrace(depth : Number) -> String {
        var s := "["
        for (self.value.indices) do { i ->
            s := s ++ self.value.at(i).toGrace(0)
            if (i < self.value.size) then {
                s := s ++ ", "
            }
        }
        s := s ++ "]"
        s
    }
    method shallowCopy {
        arrayNode.new(emptySeq).shallowCopyFieldsFrom(self)
    }
  }
}
class outerNode(nodes) {
    // references an object outside the current object.
    // nodes, a sequence of objectNodes, tells us which one.
    // The object that we refer to is the one OUTSIDE nodes.last
    inherit baseNode
    def kind is public = "outer"
    def theObjects is public = nodes
    method numberOfLevels { theObjects.size }
    method asString { "‹object outside that at line {theObjects.last.line}›" }
    method pretty(depth) { basePretty(depth) ++ asString }
    method accept(visitor) from (as) {
        visitor.visitOuter(self) up (as)
        // don't visit theObject, since this would introduce a cycle
    }
    method toGrace(depth) {
        "outer" ++ (".outer" * (theObjects.size - 1))
    }
    method isOuter -> Boolean
    method isSelfOrOuter -> Boolean
    method shallowCopy {
        outerNode(theObjects).shallowCopyFieldsFrom(self)
    }
    method map (blk) ancestors (as) {
        var nd := shallowCopy
        blk.apply(nd, as)
    }
    def end is public = if (line == 0) then { noPosition } else {
        line (line) column (linePos + 4)
    }
}
def memberNode is public = object {
    method new(request, receiver) scope(s) {
        // Represents a dotted request ‹receiver›.‹request› with no arguments.
        def result = new(request, receiver)
        result.scope := s
        result
    }
    class new(request, receiver') {
        // Represents a dotted request ‹receiver›.‹request› with no arguments.
        inherit baseNode
        def kind is public = "member"
        var value:String is public := request
        var receiver is public := receiver'
        var generics is public := false
        var isSelfRequest is public := false
        var isTailCall is public := false
        method end -> Position {
            def reqPos = if (receiver.isImplicit) then {
                start
            } else {
                positionOfNext (request) after (receiver.end)
            }
            line (reqPos.line) column (reqPos.column + request.size - 1)
        }
        method onSelf {
            isSelfRequest := true
            self
        }
        method nameString { value }
        method canonicalName { value }
        method isMember -> Boolean
        method isCall -> Boolean

        method with { emptySeq }
        method arguments { emptySeq }
        method argumentsDo { }
        method accept(visitor : ASTVisitor) from(as) {
            if (visitor.visitMember(self) up(as)) then {
                def newChain = as.extend(self)
                if (false != generics) then {
                    generics.do { each -> each.accept(visitor) from(newChain) }
                }
                receiver.accept(visitor) from(newChain)
            }
        }
        method isSelfOrOuter {
            if (value ≠ "outer") then { return false }
            receiver.isSelfOrOuter
        }
        method map(blk) ancestors(as) {
            var n := shallowCopy
            def newChain = as.extend(n)
            n.receiver := receiver.map(blk) ancestors(newChain)
            n.generics := maybeListMap(generics, blk) ancestors(newChain)
            blk.apply(n, as)
        }
        method pretty(depth) {
            def spc = "  " * (depth+1)
            var s := basePretty(depth)
            s := s ++ if (isSelfRequest) then { " on self " } else { " " }
            s := s ++ "‹" ++ self.value ++ "›\n"
            s := s ++ spc ++ receiver.pretty(depth)
            if (false != generics) then {
                s := s ++ "\n" ++ spc ++ "  Generics:"
                for (generics) do {g->
                    s := s ++ "\n" ++ spc ++ "    " ++ g.pretty(0)
                }
            }
            s
        }
        method toGrace(depth : Number) -> String {
            var s := ""
            if (self.value.substringFrom(1)to(6) == "prefix") then {
                s := self.value.substringFrom(7)to(value.size)
                s := s ++ " " ++ self.receiver.toGrace(0)
            } else {
                s := self.receiver.toGrace(depth) ++ "." ++ self.value
            }
            if (false != generics) then {
                s := s ++ "⟦"
                for (1..(generics.size - 1)) do {ix ->
                    s := s ++ generics.at(ix).toGrace(depth + 1) ++ ", "
                }
                s := s ++ generics.last.toGrace(depth + 1) ++ "⟧"
            }
            s
        }
        method asString { toGrace 0 }
        method asIdentifier {
            // make and return an identifiderNode for my request
            if (fakeSymbolTable == scope) then {
                ProgrammingError.raise "asIdentifier requested on {pretty 0} when scope was fake"
            }
            def resultNode = identifierNode.new (nameString, false) scope (scope)
            resultNode.inRequest := true
            resultNode.line := line
            resultNode.linePos := linePos
            return resultNode
        }
        method shallowCopy {
            memberNode.new(nameString, receiver).shallowCopyFieldsFrom(self)
        }
        method statementName { "expression" }
        method postCopy(other) {
            generics := other.generics
            isSelfRequest := other.isSelfRequest
            isTailCall := other.isTailCall
            self
        }
    }
}
def genericNode is public = object {
  class new(base, arguments) {
    // represents an application of a parameterized type to some arguments.
    inherit baseNode
    def kind is public = "generic"
    var value is public := base
        // in a generic application, `value` is the applied type
        // e.g. in List⟦Number⟧, value is Identifier‹List›
    var args is public := arguments
    method end -> Position { positionOfNext "⟧" after (args.last.end) }
    method nameString { value.nameString }
    method asString { toGrace 0 }
    method accept(visitor : ASTVisitor) from(as) {
        if (visitor.visitGeneric(self) up(as)) then {
            def newChain = as.extend(self)
            self.value.accept(visitor) from(newChain)
            for (self.args) do { p ->
                p.accept(visitor) from(newChain)
            }
        }
    }
    method map(blk) ancestors(as) {
        var n := shallowCopy
        def newChain = as.extend(n)
        n.value := value.map(blk) ancestors(newChain)
        n.args := listMap(args, blk) ancestors(newChain)
        blk.apply(n, as)
    }
    method pretty(depth) {
        var s := "{basePretty(depth)}({value.pretty(depth)})⟦"
        args.do { each -> s := s ++ each.pretty(depth+2) }
            separatedBy { s := s ++ ", " }
        s ++ "⟧"
    }
    method toGrace(depth : Number) -> String {
        var s := nameString ++ "⟦"
        args.do { each -> s := s ++ each.toGrace(0) }
            separatedBy { s := s ++ ", " }
        s ++ "⟧"
    }
    method shallowCopy {
        genericNode.new(value, args).shallowCopyFieldsFrom(self)
    }
  }
}

def typeParametersNode is public = object {
  class new(params') {
    inherit baseNode
    def kind is public = "typeparams"
    var params is public := params'
    method asString { toGrace 0 }
    method declarationKindWithAncestors(as) { k.typeparam }

    method accept(visitor : ASTVisitor) from(as) {
        if (visitor.visitTypeParameters(self) up(as)) then {
            def newChain = as.extend(self)
            params.do { p ->
                p.accept(visitor) from(newChain)
            }
        }
    }
    method do(blk) {
        params.do(blk)
    }
    method size { params.size }
    method map(blk) ancestors(as) {
        var n := shallowCopy
        def newChain = as.extend(n)
        n.params := listMap(params, blk) ancestors(newChain)
        blk.apply(n, as)
    }
    method pretty(depth) {
        def spc = "  " * (depth+1)
        var s := spc ++ basePretty(depth) ++ "⟦"
        params.do { each -> s := s ++ each.pretty(depth+2) }
            separatedBy { s := s ++ ", " }
        s ++ "⟧"
    }
    method toGrace(depth:Number) -> String {
        var s := "⟦"
        params.do { each -> s := "{s}{each.toGrace(depth)}" }
            separatedBy { s := s ++ ", " }
        s ++ "⟧"
    }
    method shallowCopy {
        typeParametersNode.new(emptySeq).shallowCopyFieldsFrom(self)
    }
  }
}

type IdentifierNode = BaseNode & {
    wildcard -> Boolean
    wildcard(wc:Boolean) -> Done
    dtype -> TypeNode
    dtype(typ: TypeNode) -> Done
    value -> String  // name of identifier
}

type IdentifierNodeFactory = {
    new(name: String, dtype:TypeNode) scope(s: Scope) -> IdentifierNode
    new(name: String, dtype:TypeNode)  -> IdentifierNode
}
  
def identifierNode is public = object {

    method new(name, dtype) scope(s) {
        def result = new(name, dtype)
        result.scope := s
        result
    }

    class new(name', dtype') {
        inherit baseNode
        def kind is public = "identifier"
        var name is public := name'
        var wildcard is public := false
        var dtype is public := dtype'
        var isBindingOccurrence is public := false
        var isAssigned is public := false
        var inRequest is public := false
        var generics is public := false
        var isDeclaredByParent is public := false
        var end:Position is public := line (line) column (linePos + value.size - 1)

        method value { name }
        method value:=(nu) {
            name := nu
            end := line (line) column (linePos + nu.size - 1)
        }
        method nameString { value }
        var canonicalName is public := value
        method quoted { value.quoted }
        method isIdentifier -> Boolean

        method isSelf { "self" == value }
        method isSuper { "super" == value }
        method isPrelude { "prelude" == value }
        method isOuter { "outer" == value }
        method isSelfOrOuter {
            if (isSelf) then { return true }
            if (isOuter) then { return true }
            return false
        }
        method isAppliedOccurenceOfIdentifier {
            if (wildcard) then {
                false
            } else {
                isBindingOccurrence.not
            }
        }
        method declarationKindWithAncestors(as) {
            as.parent.declarationKindWithAncestors(as)
        }
        method inTypePositionWithAncestors(as) {
            // am I used by my parent node as a type?
            // This is a hack, used as a subsitute for having information in the .gct
            // telling us which identifiers represent types
            if (as.isEmpty) then { return false }
            as.parent.usesAsType(self)
        }
        method usesAsType(aNode) {
            aNode == dtype
        }
        method accept(visitor : ASTVisitor) from(as) {
            if (visitor.visitIdentifier(self) up(as)) then {
                def newChain = as.extend(self)
                if (false != self.dtype) then {
                    self.dtype.accept(visitor) from(newChain)
                }
            }
        }
        method map(blk) ancestors(as) {
            var n := shallowCopy
            def newChain = as.extend(n)
            n.dtype := maybeMap(dtype, blk) ancestors(newChain)
            blk.apply(n, as)
        }
        method pretty(depth) {
            def spc = "  " * (depth+1)
            var s := basePretty(depth)
            if ( wildcard ) then {
                s := s ++ " Wildcard"
            } elseif { isBindingOccurrence } then {
                s := s ++ "Binding‹{value}›"
            } else {
                s := s ++ "‹{value}›"
            }
            if (false != self.dtype) then {
                s := s ++ "\n" ++ spc ++ "  Type: "
                s := s ++ self.dtype.pretty(depth + 2)
            }
            if (false != generics) then {
                s := s ++ "\n" ++ spc ++ "Generics:"
                for (generics) do {g->
                    s := s ++ "\n" ++ spc ++ "  " ++ g.pretty(depth + 2)
                }
            }
            s
        }
        method toGrace(depth : Number) -> String {
            var s
            if(self.wildcard) then {
                s := "_"
            } else {
                s := self.value
            }
            if (false != self.dtype) then {
                s := s ++ " : " ++ self.dtype.toGrace(depth + 1)
            }
            if (false != generics) then {
                s := s ++ "⟦"
                for (1..(generics.size - 1)) do {ix ->
                    s := s ++ generics.at(ix).toGrace(depth + 1)
                }
                s := s ++ generics.last.toGrace(depth + 1) ++ "⟧"
            }
            s
        }

        method asString {
            if (isBindingOccurrence) then {
                "identifierBinding‹{value}›"
            } else {
                "identifier‹{value}›"
            }
        }
        method shallowCopy {
            identifierNode.new(value, false).shallowCopyFieldsFrom(self)
        }
        method postCopy(other) {
            wildcard := other.wildcard
            isBindingOccurrence := other.isBindingOccurrence
            isDeclaredByParent := other.isDeclaredByParent
            isAssigned := other.isAssigned
            inRequest := other.inRequest
            end := other.end
            self
        }
        method statementName { "expression" }
    }
}

def typeType is public = identifierNode.new("Type", false)
def unknownType is public = identifierNode.new("Unknown", typeType)

def stringNode is public = object {
    method new(v) scope(s) {
        def result = new(v)
        result.scope := s
        result
    }

    class new(v) {
        inherit baseNode
        def kind is public = "string"
        var value is public := v
        method end -> Position { line (line) column (linePos + (toGrace 0).size - 1) }
        method accept(visitor : ASTVisitor) from(as) {
            visitor.visitString(self) up(as)
        }
        method map(blk) ancestors(as) {
            var n := shallowCopy
            def newChain = as.extend(n)
            blk.apply(n, as)
        }
        method pretty(depth) {
            "{basePretty(depth)}({self.value})"
        }
        method toGrace(depth : Number) -> String {
            def q = "\""
            q ++ value.quoted ++ q
        }
        method asString { "string {toGrace 0}" }
        method shallowCopy {
            stringNode.new(value).shallowCopyFieldsFrom(self)
        }
        method statementName { "expression" }
        method isDelimited -> Boolean
        method isConstant -> Boolean
    }
}
def numNode is public = object {
    class new(val) {
        inherit baseNode
        def kind is public = "num"
        var value is public := val
        method accept(visitor : ASTVisitor) from(as) {
            visitor.visitNum(self) up(as)
        }
        method map(blk) ancestors(as) {
            var n := shallowCopy
            def newChain = as.extend(n)
            blk.apply(n, as)
        }
        method pretty(depth) {
            "{basePretty(depth)}({self.value})"
        }
        method toGrace(depth : Number) -> String {
            self.value.asString
        }
        method asString { "num {value}" }
        method shallowCopy {
            numNode.new(value).shallowCopyFieldsFrom(self)
        }
        method statementName { "expression" }
        method isDelimited -> Boolean
        method isConstant -> Boolean
    }
}
def opNode is public = object {
  class new(op, l, r) {
    inherit baseNode
    def kind is public = "op"
    def value is public = op     // a String
    var left is public := l
    var right is public := r
    var isTailCall is public := false      // is possibly the result of a method
    var isSelfRequest is public := false

    method start -> Position { left.start }
    method end -> Position { right.end }
    method onSelf {
        isSelfRequest := true
        self
    }
    method isSimple -> Boolean    // needs parens when used as reciever
    method nameString { value ++ "(1)" }
    method canonicalName { value ++ "(_)" }
    method receiver { left }
    method isCall -> Boolean
    method accept(visitor : ASTVisitor) from(as) {
        if (visitor.visitOp(self) up(as)) then {
            def newChain = as.extend(self)
            self.left.accept(visitor) from(newChain)
            self.right.accept(visitor) from(newChain)
        }
    }
    method map(blk) ancestors(as) {
        var n := shallowCopy
        def newChain = as.extend(n)
        n.left := left.map(blk) ancestors(newChain)
        n.right := right.map(blk) ancestors(newChain)
        blk.apply(n, as)
    }
    method pretty(depth) {
        def spc = "  " * (depth+1)
        var s := "{basePretty(depth)}‹{self.nameString}›"
        s := s ++ "\n"
        s := s ++ spc ++ self.left.pretty(depth + 1)
        s := s ++ "\n"
        s := s ++ spc ++ self.right.pretty(depth + 1)
        s
    }
    method toGrace(depth : Number) -> String {
        var s := ""
        if ((self.left.kind == "op") && {self.left.value != self.value}) then {
            s := "(" ++ self.left.toGrace(0) ++ ")"
        } else {
            s := self.left.toGrace(0)
        }
        if (self.value == "..") then {
            s := s ++ self.value
        } else {
            s := s ++ " " ++ self.value ++ " "
        }
        if ((self.right.kind == "op") && {self.right.value != self.value}) then {
            s := s ++ "(" ++ self.right.toGrace(0) ++ ")"
        } else {
            s := s ++ self.right.toGrace(0)
        }
        s
    }
    method asIdentifier {
        // make an identifiderNode with the same properties as me
        def resultNode = identifierNode.new (nameString, false) scope (scope)
        resultNode.inRequest := true
        resultNode.line := line
        resultNode.linePos := linePos
        return resultNode
    }
    method shallowCopy {
        opNode.new(value, nullNode, nullNode).shallowCopyFieldsFrom(self)
    }
    method postCopy(other) {
        isTailCall := other.isTailCall
        isSelfRequest := other.isSelfRequest
        self
    }
  }
}
def bindNode is public = object {
  class new(dest', val') {
    // an assignment, or a request of a setter-method
    inherit baseNode
    def kind is public = "bind"
    var dest is public := dest'
    var value is public := val'

    method end -> Position { value.end }
    method nameString { value ++ ":=(1)" }
    method canonicalName { value ++ ":=(_)" }
    method isBind -> Boolean
    method asString { "bind {value}" }
    method accept(visitor : ASTVisitor) from(as) {
        if (visitor.visitBind(self) up(as)) then {
            def newChain = as.extend(self)
            self.dest.accept(visitor) from(newChain)
            self.value.accept(visitor) from(newChain)
        }
    }
    method map(blk) ancestors(as) {
        var n := shallowCopy
        def newChain = as.extend(n)
        n.dest := dest.map(blk) ancestors(newChain)
        n.value := value.map(blk) ancestors(newChain)
        blk.apply(n, as)
    }
    method pretty(depth) {
        def spc = "  " * (depth+1)
        var s := basePretty(depth) ++ "\n"
        s := s ++ spc ++ self.dest.pretty(depth + 1)
        s := s ++ "\n"
        s := s ++ spc ++ self.value.pretty(depth + 1)
        s
    }
    method toGrace(depth : Number) -> String {
        def spc = "    " * depth
        var s := self.dest.toGrace(depth + 1)
        s := s ++ " := " ++ self.value.toGrace(depth + 1)
        s
    }
    method shallowCopy {
        bindNode.new(dest, value).shallowCopyFieldsFrom(self)
    }
    method statementName { "assignment or assigment request" }
  }
}
def defDecNode is public = object {
    method new(name', val, dtype') scope(s) {
        def result = new(name', val, dtype')
        result.scope := s
        result
    }

    class new(name', val, dtype') {
        inherit baseNode
        def kind is public = "defdec"
        var name is public := name'
        var value is public := val
        var dtype is public := dtype'
        def nameString is public = name.nameString
        var annotations is public := [ ]
        var startToken is public := false

        method end -> Position { value.end }
        method isPublic {
            // defs are confidential by default
            if (annotations.size == 0) then { return false }
            if (findAnnotation(self, "public")) then { return true }
            findAnnotation(self, "readable")
        }
        method isFieldDec -> Boolean
        method isWritable -> Boolean
        method isReadable { isPublic }

        method returnsObject {
            value.returnsObject
        }
        method returnedObjectScope {
            // precondition: returnsObject
            value.returnedObjectScope
        }
        method usesAsType(aNode) {
            aNode == dtype
        }
        method declarationKindWithAncestors(as) { k.defdec }

        method accept(visitor : ASTVisitor) from(as) {
            if (visitor.visitDefDec(self) up(as)) then {
                def newChain = as.extend(self)
                self.name.accept(visitor) from(newChain)
                if (false != self.dtype) then {
                    self.dtype.accept(visitor) from(newChain)
                }
                for (self.annotations) do { ann ->
                    ann.accept(visitor) from(newChain)
                }
                value.accept(visitor) from(newChain)
            }
        }
        method map(blk) ancestors(as) {
            var n := shallowCopy
            def newChain = as.extend(n)
            n.name := name.map(blk) ancestors(newChain)
            n.value := value.map(blk) ancestors(newChain)
            n.dtype := maybeMap(dtype, blk) ancestors(newChain)
            n.annotations := listMap(annotations, blk) ancestors(newChain)
            blk.apply(n, as)
        }
        method pretty(depth) {
            def spc = "  " * (depth+1)
            var s := basePretty(depth) ++ "\n"
            s := s ++ spc ++ self.name.pretty(depth)
            if (false != dtype) then {
                s := s ++ "\n" ++ spc ++ "Type: " ++ self.dtype.pretty(depth + 2)
            }
            if (false != value) then {
                s := s ++ "\n" ++ spc ++ "Value: " ++ value.pretty(depth + 2)
            }
            if (annotations.isEmpty.not) then {
                s := s ++ "\n{spc}Annotations:"
                annotations.do { ann ->
                    s := "{s} {ann.pretty(depth + 2)}"
                }
            }
            if (false != comments) then {
                s := s ++ comments.pretty(depth+2)
            }
            s
        }
        method toGrace(depth : Number) -> String {
            def spc = "    " * depth
            var s := "def {self.name.toGrace(0)}"
            if ( (false != self.dtype) && {
                    self.dtype.value != "Unknown" }) then {
                s := s ++ " : " ++ self.dtype.toGrace(0)
            }
            if (self.annotations.size > 0) then {
                s := s ++ " is "
                s := s ++ self.annotations.fold{ a,b ->
                    if (a != "") then { a ++ ", " } else { "" } ++ b.toGrace(0) }
                        startingWith ""
            }
            if (false != self.value) then {
                s := s ++ " = " ++ self.value.toGrace(depth)
            }
            s
        }
        method shallowCopy {
            defDecNode.new(name, value, dtype).shallowCopyFieldsFrom(self)
        }
        method postCopy(other) {
            startToken := other.startToken
            self
        }
        method statementName { "definition" }
    }
}
def varDecNode is public = object {
  class new(name', val', dtype') {
    inherit baseNode
    def kind is public = "vardec"
    var name is public := name'
    var value is public := val'
    var dtype is public := dtype'
    def nameString is public = name.value
    var annotations is public := [ ]

    method end -> Position {
        if (false ≠ value) then { return value.end }
        if (annotations.isEmpty.not) then { return annotations.last.end }
        if (false ≠ dtype) then { return dtype.end }
        return name.end
    }
    method isPublic {
        // vars are confidential by default
        if (annotations.size == 0) then { return false }
        if (findAnnotation(self, "public")) then { return true }
        findAnnotation(self, "readable")
    }
    method isWritable {
        if (annotations.size == 0) then { return false }
        if (findAnnotation(self, "public")) then { return true }
        if (findAnnotation(self, "writable")) then { return true }
        false
    }
    method isReadable {
        if (annotations.size == 0) then { return false }
        if (findAnnotation(self, "public")) then { return true }
        if (findAnnotation(self, "readable")) then { return true }
        false
    }
    method isFieldDec -> Boolean

    method usesAsType(aNode) {
        aNode == dtype
    }

    method declarationKindWithAncestors(as) { k.vardec }

    method accept(visitor : ASTVisitor) from(as) {
        if (visitor.visitVarDec(self) up(as)) then {
            def newChain = as.extend(self)
            self.name.accept(visitor) from(newChain)
            if (false != self.dtype) then {
                self.dtype.accept(visitor) from(newChain)
            }
            for (self.annotations) do { ann ->
                ann.accept(visitor) from(newChain)
            }
            if (false != self.value) then {
                self.value.accept(visitor) from(newChain)
            }
        }
    }
    method map(blk) ancestors(as) {
        var n := shallowCopy
        def newChain = as.extend(n)
        n.name := name.map(blk) ancestors(newChain)
        n.value := maybeMap(value, blk) ancestors(newChain)
        n.dtype := maybeMap(dtype, blk) ancestors(newChain)
        n.annotations := listMap(annotations, blk) ancestors(newChain)
        blk.apply(n, as)
    }
    method pretty(depth) {
        def spc = "  " * (depth+1)
        var s := basePretty(depth) ++ "\n"
        s := s ++ spc ++ self.name.pretty(depth + 1)
        if (false != self.dtype) then {
            s := s ++ "\n" ++ spc ++ "Type: "
            s := s ++ self.dtype.pretty(depth + 2)
        }
        if (false != self.value) then {
            s := s ++ "\n" ++ spc ++ "Value: "
            s := s ++ self.value.pretty(depth + 2)
        }
        if (false != comments) then {
            s := s ++ comments.pretty(depth+2)
        }
        s
    }
    method toGrace(depth : Number) -> String {
        def spc = "    " * depth
        var s := "var {self.name.toGrace(0)}"
        if ( (false != self.dtype) && {
                self.dtype.value != "Unknown" }) then {
            s := s ++ " : " ++ self.dtype.toGrace(0)
        }
        if (self.annotations.size > 0) then {
            s := s ++ " is "
            s := s ++ self.annotations.fold{ a,b ->
                if (a != "") then { a ++ ", " } else { "" } ++ b.toGrace(0) }
                    startingWith ""
        }
        if (false != self.value) then {
            s := s ++ " := " ++ self.value.toGrace(depth)
        }
        s
    }
    method shallowCopy {
        varDecNode.new(name, value, dtype).shallowCopyFieldsFrom(self)
    }
    method statementName { "variable declaration" }

  }
}
def importNode is public = object {
  class new(path', name', dtype') {
    inherit baseNode
    def kind is public = "import"
    var value is public := name'
    var path is public := path'
    var annotations is public := [ ]
    var dtype is public := dtype'
    method end -> Position { value.end }
    method isImport -> Boolean
    method isExternal -> Boolean
    method isExecutable -> Boolean
    method name { value }
    method nameString { value.nameString }
    method isPublic {
        // imports, like defs, are confidential by default
        if (annotations.size == 0) then { return false }
        if (findAnnotation(self, "public")) then { return true }
        findAnnotation(self, "readable")
    }
    method moduleName {
        var bnm := ""
        for (path) do {c->
            if (c == "/") then {
                bnm := ""
            } else {
                bnm := bnm ++ c
            }
        }
        bnm
    }
    method isWritable -> Boolean
    method isReadable { isPublic }
    method declarationKindWithAncestors(as) { k.defdec }
    method usesAsType(aNode) {
        aNode == dtype
    }
    method accept(visitor : ASTVisitor) from(as) {
        if (visitor.visitImport(self) up(as)) then {
            def newChain = as.extend(self)
            for (self.annotations) do { ann ->
                ann.accept(visitor) from(newChain)
            }
            self.value.accept(visitor) from(newChain)
            if (false != self.dtype) then {
                self.dtype.accept(visitor) from(newChain)
            }
        }
    }
    method map(blk) ancestors(as) {
        var n := shallowCopy
        def newChain = as.extend(n)
        n.value := value.map(blk) ancestors(newChain)
        n.dtype := maybeMap(dtype, blk) ancestors(newChain)
        n.annotations := listMap(annotations, blk) ancestors(newChain)
        blk.apply(n, as)
    }
    method pretty(depth) {
        def spc = "  " * (depth+1)
        var s := basePretty(depth) ++ "\n"
        s := s ++ "{spc}Path: {path}\n"
        s := s ++ "{spc}Identifier: {value}\n"
        if (annotations.size > 0) then {
            s := s ++ "{spc}Anotations: {annotations}\n"
        }
        s
    }
    method toGrace(depth : Number) -> String {
        "import \"{self.path}\" as {nameString}"
    }
    method shallowCopy {
        importNode.new(path, nullNode, false).shallowCopyFieldsFrom(self)
    }
  }
}
def dialectNode is public = object {
  class new(path') {
    inherit baseNode
    def kind is public = "dialect"
    var value is public := path'

    method end -> Position { line (line) column (util.lines.at(line).size) }
    method isDialect -> Boolean
    method isExternal -> Boolean
    method isExecutable -> Boolean
    method moduleName {
        var bnm := ""
        for (value) do {c->
            if (c == "/") then {
                bnm := ""
            } else {
                bnm := bnm ++ c
            }
        }
        bnm
    }
    method path {
        value
    }
    method accept(visitor : ASTVisitor) from(as) {
        visitor.visitDialect(self) up(as)
    }
    method map(blk) ancestors(as) {
        var n := shallowCopy
        def newChain = as.extend(n)
        blk.apply(n, as)
    }
    method pretty(depth) {
        def spc = "  " * (depth+1)
        var s := basePretty(depth) ++ "\n"
        s := s ++ "{spc}Path: {self.value}\n"
        s
    }
    method toGrace(depth : Number) -> String {
        "dialect \"{self.value}\""
    }
    method shallowCopy {
        dialectNode.new(value).shallowCopyFieldsFrom(self)
    }
  }
}
def returnNode is public = object {
  class new(expr) {
    inherit baseNode
    def kind is public = "return"
    var value is public := expr
    var dtype is public := false  // the enclosing method's declared return type

    method end -> Position {
        if (noPosition ≠ value.end) then {
            value.end
        } else {
            line (line) column (linePos + 5)
        }
    }
    method isReturn -> Boolean
    method accept(visitor : ASTVisitor) from(as) {
        if (visitor.visitReturn(self) up(as)) then {
            def newChain = as.extend(self)
            self.value.accept(visitor) from(newChain)
        }
    }
    method map(blk) ancestors(as) {
        var n := shallowCopy
        def newChain = as.extend(n)
        n.value := value.map(blk) ancestors(newChain)
        n.dtype := maybeMap(dtype, blk) ancestors(newChain)
        blk.apply(n, as)
    }
    method pretty(depth) {
        def spc = "  " * (depth+1)
        var s := basePretty(depth) ++ "\n"
        s := s ++ spc ++ self.value.pretty(depth + 1)
        if (false ≠ dtype) then { s := "{s} (type {dtype.toGrace 0})" }
        s
    }
    method toGrace(depth : Number) -> String {
        "return " ++ self.value.toGrace(depth)
    }
    method shallowCopy {
        returnNode.new(value).shallowCopyFieldsFrom(self)
    }
    method postCopy(other) {
        dtype := other.dtype
        self
    }
    method returnsObject { value.returnsObject }
    method returnedObjectScope {
        // precondition: returns object
        value.returnedObjectScope
    }
    method resultExpression { value }
  }
}
def inheritNode is public = object {
    method new(expr) scope(s) {
        def result = new(expr)
        result.scope := s
        result
    }
    class new(expr) {
        inherit baseNode
        def kind is public = "inherit"
        var value is public := expr
        var providedNames is public := emptySet
        var aliases is public := [ ]
        var exclusions is public := [ ]
        var isUse is public := false  // this is a `use trait` clause, not an inherit

        method end -> Position { value.end }
        method isLegalInTrait { isUse }
        method isInherits -> Boolean
        method inheritFromMember { value.isMember }
        method inheritFromCall { value.isCall }
        method isExecutable -> Boolean
        method accept(visitor : ASTVisitor) from(as) {
            if (visitor.visitInherits(self) up(as)) then {
                def newChain = as.extend(self)
                value.accept(visitor) from(newChain)
                aliases.do { a ->
                    a.newName.accept(visitor) from(newChain)
                    a.oldName.accept(visitor) from(newChain)
                }
                exclusions.do { e -> e.accept(visitor) from(newChain) }
            }
        }
        method declarationKindWithAncestors(as) {
            // identifiers declared in an inherit statement are aliases for
            // methods.  We treat them as methods, because (unlike inherited names)
            // they can't be overridden by local methods.
            k.methdec
        }
        method map(blk) ancestors(as) {
            var n := shallowCopy
            def newChain = as.extend(n)
            n.value := value.map(blk) ancestors(newChain)
            blk.apply(n, as)
        }
        method pretty(depth) {
            def spc = "  " * (depth+1)
            var s := basePretty(depth)
            if (isUse) then { s := "{s} (use)" }
            s := s ++ "\n" ++ spc ++ self.value.pretty(depth + 1)
            aliases.do { a ->
                s := "{s}\n{a.pretty(depth)}"
            }
            if (exclusions.isEmpty.not) then { s := "{s}\n{spc}" }
            exclusions.do { e ->
                s := "{s} exclude {e} "
            }
            if (providedNames.isEmpty.not) then {
                s := s ++ "\n{spc}Provided names: {list(providedNames).sort}"
            }
            s
        }
        method toGrace(depth : Number) -> String {
            var s := ""
            repeat (depth) times {
                s := s ++ "    "
            }
            s := s ++ if (isUse) then { "use " } else { "inherit " }
            s := s ++ self.value.toGrace(0)
            aliases.do { a ->
                s := "{s} {a} "
            }
            exclusions.do { e ->
                s := "{s} exclude {e.nameString} "
            }
            s
        }
        method asString {
            if (isUse) then { "use " } else { "inherit " } ++ value.toGrace 0
        }
        method nameString { value.toGrace(0) }
        method addAlias (newName) for (oldName) {
            aliases.push(aliasNew(newName) old(oldName))
        }
        method addExclusion(ident) {
            exclusions.push(ident)
        }
        method shallowCopy {
            inheritNode.new(nullNode).shallowCopyFieldsFrom(self)
        }
        method postCopy(other) {
            providedNames := other.providedNames
            aliases := other.aliases
            exclusions := other.exclusions
            isUse := other.isUse
            self
        }
        method statementName {
            if (isUse) then { "use" } else { "inherit" }
        }
    }
}
type AliasPair = {
    newName
    oldName
}

class aliasNew(n) old(o) {
    method newName {n}
    method oldName {o}
    method asString { "alias {n.nameString} = {o.nameString}" }
    method pretty(depth) {
        def spc = "  " * (depth+1)
        "{spc}  alias {n.pretty(depth)} = {o.pretty(depth)}"
    }
    method hash { (n.hash * 1171) + o.hash }
    method isExecutable -> Boolean
    method == (other) {
        match (other)
            case { that:AliasPair -> (n == that.newName) && (o == that.oldName) }
            case { _ -> false }
    }
}

type SignaturePartNode = BaseNode & {
    name -> String
    name:=(s:String) -> Done
    params -> List[[IdentifierNode]]
    lineLength -> Number
    lineLength(n: Number) -> Done
    numParams -> Number  
}

def signaturePart is public = object {
    method new {
        partName "" params []
    }
    method partName(n) scope(s) {
        def result = partName(n) params []
        result.scope := s
        result
    }
    method partName(n) params(ps) scope(s) {
        def result = partName(n) params(ps)
        result.scope := s
        result
    }
    method partName(n) {
        partName(n) params []
    }
    class partName(n) params(ps) {
        inherit baseNode
        def kind is public = "signaturepart"
        var name is public := n
        var params is public := ps
        var typeParams is public := false
        var lineLength is public := 0

        method end -> Position {
            if (params.isEmpty.not) then {
                return positionOfNext ")" after (params.last.end)
            }
            if (false ≠ typeParams) then {
                return positionOfNext "⟧" after (typeParams.last.end)
            }
            return line (line) column (linePos + name.size - 1)
        }
        method nameString {
            if (params.isEmpty) then {return name}
            name ++ "(" ++ params.size ++ ")"
        }
        method numParams { params.size }
        method canonicalName {
            if (params.size == 0) then {return name}
            var underScores := ""
            params.do { _ -> underScores := underScores ++ "_" }
                separatedBy { underScores := underScores ++ "," }
            name ++ "(" ++ underScores ++ ")"
        }

        method accept(visitor : ASTVisitor) from(as) {
            if (visitor.visitSignaturePart(self) up(as)) then {
                def newChain = as.extend(self)
                params.do { p -> p.accept(visitor) from(newChain) }
                if (false != typeParams) then {
                    typeParams.accept(visitor) from(newChain)
                }
            }
        }
        method declarationKindWithAncestors(as) { k.parameter }
        method map(blk) ancestors(as) {
            var nd := shallowCopy
            def newChain = as.extend(nd)
            nd.params := listMap(params, blk) ancestors(newChain)
            nd.typeParams := maybeMap(typeParams, blk) ancestors(newChain)
            blk.apply(nd, as)
        }
        method pretty(depth) {
            def spc = "  " * (depth+1)
            var s := "{basePretty(depth)}: {name}"
            if (params.isEmpty.not) then { s := "{s}\n{spc}Parameters:" }
            for (params) do { p ->
                s := "{s}\n  {spc}{p.pretty(depth + 2)}"
            }
            s
        }
        method toGrace(depth) {
            var s := name
            if (params.isEmpty.not) then {
                s := s ++ "("
                params.do { each -> s := each.toGrace(depth + 1) }
                    separatedBy { s := s ++ ", " }
                s := s ++ ")"
            }
            s
        }
        method shallowCopy {
            signaturePart.partName(name) params(params)
                .shallowCopyFieldsFrom(self)
        }
        method postCopy(other) {
            typeParams := other.typeParams
            lineLength := other.lineLength
            self
        }
        method asString {
            "part: {nameString}"
        }
    }
}

def requestPart is public = object {
    method new {
        request "" withArgs( [] )
    }
    method request(name) withArgs(argList) scope (s) {
        def result = request(name) withArgs(argList)
        result.scope := s
        result
    }
    class request(rPart) withArgs(xs) {
        inherit baseNode
        def kind is public = "callwithpart"
        var name is public := rPart
        var args is public := xs
        var typeArgs := emptySeq
        var lineLength is public := 0

        method end -> Position {
            if (args.isEmpty.not) then {
                return args.last.end  // there may or may not be a following `)`
            }
            if (typeArgs.isEmpty.not) then {
                return positionOfNext "⟧" after (typeArgs.last.end)
            }
            return line (line) column (linePos + name.size - 1)
        }
        method nameString {
            if (args.size == 0) then {return name}
            name ++ "(" ++ args.size ++ ")"
        }

        method canonicalName {
            if (args.size == 0) then {return name}
            var underScores := ""
            args.do { _ -> underScores := underScores ++ "_" }
                separatedBy { underScores := underScores ++ "," }
            name ++ "(" ++ underScores ++ ")"
        }

        method map(blk) ancestors(as) {
            var n := shallowCopy
            def newChain = as.extend(n)
            n.args := listMap(args, blk) ancestors(newChain)
            blk.apply(n, as)
        }
        method pretty(depth) {
            def spc = "  " * (depth+1)
            var s := "{basePretty(depth)}: {name}"
            s := "{s}\n{spc}Args:"
            for (args) do { a ->
                s := "{s}\n  {spc}{a.pretty(depth + 3)}"
            }
            s
        }
        method toGrace(depth) {
            var s := name
            if (typeArgs.size > 0) then {
                s := s ++ "⟦"
                typeArgs.do { tArg ->
                    s := s ++ tArg.toGrace(depth + 1)
                } separatedBy { s := s ++ ", " }
                s := s ++ "⟧"
            }
            if (args.size > 0) then {
                def needsParens = (args.size > 1) || (args.first.isDelimited.not)
                s := s ++ if (needsParens) then { "(" } else { " " }
                args.do { arg ->
                    s := s ++ arg.toGrace(depth)
                } separatedBy {
                    s := s ++ ", "
                }
                if (needsParens) then { s := s ++ ")" }
            }
            s
        }

        method shallowCopy {
            requestPart.request(name) withArgs(args).shallowCopyFieldsFrom(self)
        }
        method postCopy(other) {
            lineLength := other.lineLength
            self
        }
        method statementName { "request" }
    }
}

def commentNode is public = object {
    class new(val') {
        inherit baseNode
        def kind is public = "comment"
        var value is public := val'
        var isPartialLine:Boolean is public := false
        var isPreceededByBlankLine is public := false
        var endLine is public := util.linenum

        method end -> Position { line (endLine) column (util.lines.at(endLine).size) }
        method isComment -> Boolean
        method isLegalInTrait -> Boolean
        method isExecutable -> Boolean
        method asString { "comment ({line}–{endLine}): {value}" }
        method extendCommentUsing(cmtNode) {
            value := value ++ " " ++ cmtNode.value
            endLine := cmtNode.endLine
        }
        method map(blk) ancestors(as) {
            var n := shallowCopy
            def newChain = as.extend(n)
            blk.apply(n, as)
        }
        method accept(visitor : ASTVisitor) from(as) {
            visitor.visitComment(self) up(as)
        }
        method pretty(depth) {
            var s := "\n"
            repeat (depth-1) times {
                s := s ++ "  "
            }
            def pb = if (isPreceededByBlankLine) then { " > blank" } else { "" }
            "{s}Comment{pb}({line}–{endLine}): ‹{value}›"
        }
        method toGrace(depth) {
            // Partial line comments don't start with a newline, whereas
            // full-line comments do.  No newline at end in either case.
            if (isPartialLine) then {
                "// (partial) {value}"
            } else {
                def spc = "    " * depth
                wrap(value) to (lineLength) prefix (spc ++ "// ")
            }
        }
        method shallowCopy {
            commentNode.new(nullNode).shallowCopyFieldsFrom(self)
        }
        method postCopy(other) {
            value := other.value
            isPartialLine := other.isPartialLine
            isPreceededByBlankLine := other.isPreceededByBlankLine
            endLine := other.endLine
            self
        }
    }
}

method wrap(str) to (l:Number) prefix (margin) {
    def ind = margin.size
    def len = max(ind + 4, l)
    if ((ind + str.size) <= len) then {
        return "\n" ++ margin ++ str
    }
    var currBreak
    var trimmedLine

    try {
        currBreak := str.lastIndexOf " " startingAt (len - ind)
            ifAbsent {len - ind}
        trimmedLine := str.substringFrom (1) to (currBreak).trim
    } catch { ex:NoSuchMethod ->  // C string libraries lack methods
        currBreak := min(len - ind, str.size)
        (1..currBreak).do { ix ->
            if (str.at(ix) == " ") then { currBreak := ix }
        }
        var end := currBreak
        while {(end >= 1) && {str.at(end) == " "}} do {
            end := end - 1
        }
        var start := 1
        while {(start <= str.size) && {str.at(start) == " "}} do {
            start := start + 1
        }
        trimmedLine := str.substringFrom (start) to (end)
    }
    "\n" ++ margin ++ trimmedLine ++
        wrap(str.substringFrom (currBreak+1) to (str.size))
            to (l) prefix (margin)
}


type ASTVisitor = {
    visitIf(o) up(as) -> Boolean
    visitBlock(o) up(as) -> Boolean
    visitMatchCase(o) up(as) -> Boolean
    visitTryCatch(o) up(as) -> Boolean
    visitMethodType(o) up(as) -> Boolean
    visitSignaturePart(o) up(as) -> Boolean
    visitTypeLiteral(o) up(as) -> Boolean
    visitTypeParameters(o) up(as) -> Boolean
    visitTypeDec(o) up(as) -> Boolean
    visitMethod(o) up(as) -> Boolean
    visitCall(o) up(as) -> Boolean
    visitObject(o) up(as) -> Boolean
    visitModule(o) up(as) -> Boolean
    visitArray(o) up(as) -> Boolean
    visitMember(o) up(as) -> Boolean
    visitGeneric(o) up(as) -> Boolean
    visitIdentifier(o) up(as) -> Boolean
    visitString(o) up(as) -> Boolean
    visitNum(o) up(as) -> Boolean
    visitOp(o) up(as) -> Boolean
    visitBind(o) up(as) -> Boolean
    visitDefDec(o) up(as) -> Boolean
    visitVarDec(o) up(as) -> Boolean
    visitImport(o) up(as) -> Boolean
    visitReturn(o) up(as) -> Boolean
    visitInherits(o) up(as) -> Boolean
    visitDialect(o) up(as) -> Boolean
    visitComment(o) up(as) -> Boolean
    visitImplicit(o) up(as) -> Boolean
    visitOuter(o) up(as) -> Boolean
}

class baseVisitor -> ASTVisitor {
    method visitIf(o) up(as) { visitIf(o) }
    method visitBlock(o) up(as) { visitBlock(o) }
    method visitMatchCase(o) up(as) { visitMatchCase(o) }
    method visitTryCatch(o) up(as) { visitTryCatch(o) }
    method visitMethodType(o) up(as) { visitMethodType(o) }
    method visitSignaturePart(o) up(as) { visitSignaturePart(o) }
    method visitTypeDec(o) up(as) { visitTypeDec(o) }
    method visitTypeLiteral(o) up(as) { visitTypeLiteral(o) }
    method visitTypeParameters(o) up(as) { visitTypeParameters(o) }
    method visitMethod(o) up(as) { visitMethod(o) }
    method visitCall(o) up(as) { visitCall(o) }
    method visitObject(o) up(as) { visitObject(o) }
    method visitModule(o) up(as) { visitObject(o) }
    method visitArray(o) up(as) { visitArray(o) }
    method visitMember(o) up(as) { visitMember(o) }
    method visitGeneric(o) up(as) { visitGeneric(o) }
    method visitIdentifier(o) up(as) { visitIdentifier(o) }
    method visitString(o) up(as) { visitString(o) }
    method visitNum(o) up(as) { visitNum(o) }
    method visitOp(o) up(as) { visitOp(o) }
    method visitBind(o) up(as) { visitBind(o) }
    method visitDefDec(o) up(as) { visitDefDec(o) }
    method visitVarDec(o) up(as) { visitVarDec(o) }
    method visitImport(o) up(as) { visitImport(o) }
    method visitReturn(o) up(as) { visitReturn(o) }
    method visitInherits(o) up(as) { visitInherits(o) }
    method visitDialect(o) up(as) { visitDialect(o) }
    method visitComment(o) up(as) { visitComment(o) }
    method visitImplicit(o) up(as) { visitImplicit(o) }
    method visitOuter(o) up(as) -> Boolean { visitOuter(o) }

    // copied wrong -- FIX
    method visitIf(o) -> Boolean -> Boolean
    method visitBlock(o) -> Boolean -> Boolean
    method visitMatchCase(o) -> Boolean -> Boolean
    method visitTryCatch(o) -> Boolean -> Boolean
    method visitMethodType(o) -> Boolean -> Boolean
    method visitSignaturePart(o) -> Boolean -> Boolean
    method visitTypeDec(o) -> Boolean -> Boolean
    method visitTypeLiteral(o) -> Boolean -> Boolean
    method visitTypeParameters(o) -> Boolean -> Boolean
    method visitMethod(o) -> Boolean -> Boolean
    method visitCall(o) -> Boolean -> Boolean
    method visitObject(o) -> Boolean -> Boolean
    method visitModule(o) -> Boolean -> Boolean
    method visitArray(o) -> Boolean -> Boolean
    method visitMember(o) -> Boolean -> Boolean
    method visitGeneric(o) -> Boolean -> Boolean
    method visitIdentifier(o) -> Boolean -> Boolean
    method visitString(o) -> Boolean -> Boolean
    method visitNum(o) -> Boolean -> Boolean
    method visitOp(o) -> Boolean -> Boolean
    method visitBind(o) -> Boolean -> Boolean
    method visitDefDec(o) -> Boolean -> Boolean
    method visitVarDec(o) -> Boolean -> Boolean
    method visitImport(o) -> Boolean -> Boolean
    method visitReturn(o) -> Boolean -> Boolean
    method visitInherits(o) -> Boolean -> Boolean
    method visitDialect(o) -> Boolean -> Boolean
    method visitComment(o) -> Boolean -> Boolean
    method visitImplicit(o) -> Boolean -> Boolean
    method visitOuter(o) -> Boolean -> Boolean

    method asString { "an AST visitor" }
}

class pluggableVisitor(visitation:Block2) -> ASTVisitor {
    // Manufactures a default visitor, given a 2-parameter block.
    // Typically, some of the methods will be overridden.
    // The block will be applied with the AST node as the first argument
    // and the ancestor chain as the second, and should answer true if
    // the visitation is to continue and false if it is to go no deeper.

    method visitIf(o) up(as) { visitation.apply (o, as) }
    method visitBlock(o) up(as) { visitation.apply (o, as) }
    method visitMatchCase(o) up(as) { visitation.apply (o, as) }
    method visitTryCatch(o) up(as) { visitation.apply (o, as) }
    method visitMethodType(o) up(as) { visitation.apply (o, as) }
    method visitSignaturePart(o) up(as) { visitation.apply (o, as) }
    method visitTypeDec(o) up(as) { visitation.apply (o, as) }
    method visitTypeLiteral(o) up(as) { visitation.apply (o, as) }
    method visitMethod(o) up(as) { visitation.apply (o, as) }
    method visitCall(o) up(as) { visitation.apply (o, as) }
    method visitObject(o) up(as) { visitation.apply (o, as) }
    method visitModule(o) up(as) { visitation.apply (o, as) }
    method visitArray(o) up(as) { visitation.apply (o, as) }
    method visitMember(o) up(as) { visitation.apply (o, as) }
    method visitGeneric(o) up(as) { visitation.apply (o, as) }
    method visitIdentifier(o) up(as) { visitation.apply (o, as) }
    method visitString(o) up(as) { visitation.apply (o, as) }
    method visitNum(o) up(as) { visitation.apply (o, as) }
    method visitOp(o) up(as) { visitation.apply (o, as) }
    method visitBind(o) up(as) { visitation.apply (o, as) }
    method visitDefDec(o) up(as) { visitation.apply (o, as) }
    method visitVarDec(o) up(as) { visitation.apply (o, as) }
    method visitImport(o) up(as) { visitation.apply (o, as) }
    method visitReturn(o) up(as) { visitation.apply (o, as) }
    method visitInherits(o) up(as) { visitation.apply (o, as) }
    method visitDialect(o) up(as) { visitation.apply (o, as) }
    method visitComment(o) up(as) { visitation.apply (o, as) }
    method visitImplicit(o) up(as) { visitation.apply (o, as) }
    method visitOuter(o) up(as) { visitation.apply (o, as) }

    method asString { "a pluggable AST visitor" }
}


def patternMarkVisitor = object {
    inherit baseVisitor
    method visitCall(c) up(as) {
        c.isPattern := true
        true
    }
}

method findAnnotation(node, annName) {
    for (node.annotations) do {ann->
        if ((ann.kind == "identifier") && {
            ann.value == annName }) then {
            return object {
                inherit true
                def value is public = ann
            }
        }
    }
    false
}
