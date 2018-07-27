#pragma ExtendedLineups
#pragma noTypeChecks
dialect "none"
import "standardGrace" as sg

import "ast" as ast
import "xmodule" as xmodule
import "io" as io

inherit sg.methods

// Returned when searching for a method that is not there.
def noSuchMethod: outer.Pattern = object {
    inherit BasicPattern.new

    method match(obj : Object) {
        if(self.isMe(obj)) then {
            SuccessfulMatch.new(self, list[])
        } else {
            FailedMatch.new(obj)
        }
    }
}

// Replace newline characters with spaces. This is a
// workaround for issue #116 on the gracelang/minigrace
// git repo. The result of certain astNodes.toGrace(0)
// is a string containing newlines, and error messages
// containing these strings get cut off at the first
// newline character, resulting in an unhelpful error
// message.
method stripNewLines(str) → String is confidential {
    str.replace("\n")with(" ")
}

type ObjectType = {
    methods → Set⟦MethodType⟧
    getMethod (name : String) → MethodType | noSuchMethod
    node → AstNode
    resolve → ObjectType
    isResolved → Boolean
    isDynamic → Boolean
    == (other:ObjectType) → Boolean
    isSubtypeOf (other : ObjectType) → Boolean
    isSubtypeHelper (trials : List⟦TypePair⟧, other : ObjectType) → Answer
    restriction (other : ObjectType) → ObjectType
    isConsistentSubtypeOf (other : ObjectType) → Boolean
    getVariantTypes → List⟦ObjectType⟧
    setVariantTypes(newVariantTypes:List⟦ObjectType⟧) → Done
    | (other : ObjectType) → ObjectType
    & (other : ObjectType) → ObjectType
}

// methods to create an object type from various inputs
type ObjectTypeFactory = {
    definedByNode (node : AstNode) → ObjectType
    fromMethods (methods' : Set⟦MethodType⟧) withNode(node:AstNode)→ ObjectType
    fromMethods (methods' : Set⟦MethodType⟧) withNode(node:AstNode)
                                          withName (name : String) → ObjectType
    fromDType (dtype) → ObjectType
    fromIdentifier(ident : Identifier) → ObjectType
    dynamic → ObjectType
    bottom → ObjectType
    blockTaking (params : List⟦Parameter⟧) returning (rType : ObjectType) → ObjectType
    blockReturning (rType : ObjectType) → ObjectType
    preludeTypes → Set⟦String⟧
    base → ObjectType
    doneType → ObjectType
    pattern → ObjectType
    iterator → ObjectType
    boolean → ObjectType
    number → ObjectType
    string → ObjectType
    listTp → ObjectType
    set → ObjectType
    sequence → ObjectType
    dictionary → ObjectType
    point → ObjectType
    binding → ObjectType
    collection → ObjectType
    enumerable → ObjectType
}

// Method signature information.
// isSpecialisation and restriction are used for type-checking
type MethodType = {
    // name of the method
    name → String

    // name of the method with number of parameters for each part
    nameString → String

    // parameters and their types for each part
    signature → List⟦MixPart⟧

    // return type
    returnType → ObjectType

    // check equivalence of part names, param types, and return type;
    // does not check if param names are the same
    == (other: MethodType) → Boolean

    // create restriction of method type using other
    restriction (other : MethodType) → MethodType

    // Does it extend other
    isSpecialisationOf (trials : List⟦TypePair⟧, other : MethodType) → Answer
}

type MethodTypeFactory = {
    signature (signature' : List⟦MixPart⟧)
            returnType (rType : ObjectType)→ MethodType
    member (name : String) ofType (rType : ObjectType) → MethodType
    fromGctLine (line : String, importName: String) → MethodPair
    fromNode (node: AstNode) → MethodType
}

//This type is used as the return type of the method fromGCTLine
type MethodPair = {
    mType → MethodType
    mNode → AstNode
}

//This type is used for checking subtyping
type TypePair = {
    first → ObjectType
    second → ObjectType
    == (other:Object)→ Boolean
    asString → String
}

// MixPart is a "segment" of a method:
// Ex. for (param1) do (param2), for(param1) and do(param2) are separate "MixParts."
type MixPart = {
    name → String
    parameters → List⟦Param⟧
}

//This type is used for checking subtyping
type Answer = {
    ans → Boolean
    trials → List⟦TypePair⟧
    asString → String
}

// type of a parameter
type Param = {
    name → String
    typeAnnotation → ObjectType
}

type AstNode = { kind → String }

// Create a pattern for matching kind
class aPatternMatchingNode (kind : String) → Pattern {
    inherit outer.BasicPattern.new

    method match (obj : Object) → MatchResult | false {
        match (obj)
          case { node : AstNode →
            if (kind == node.kind) then {
                SuccessfulMatch.new (node, outer.emptySequence)
            } else {
                false
            }
        } case { _ → false }
    }
}

// Same as Pattern??
//type Matcher = {
//    match(obj: AstNode) → MatchResult | false
//}

// A pattern that matches if parameter satisfies predicate
class booleanPattern (predicate: Function1⟦AstNode⟧) → Pattern {
    inherit BasicPattern.new
    method match (obj: AstNode) → MatchResult | false{
        if (predicate.apply (obj)) then {
            SuccessfulMatch.new (obj, outer.emptySequence)
        } else {
            false
        }
    }
}

// patterns for built-in AST Nodes
def If: Pattern is public = aPatternMatchingNode "if"
def BlockLiteral: Pattern is public = aPatternMatchingNode "block"
def MatchCase: Pattern is public = aPatternMatchingNode "matchcase"
def TryCatch: Pattern is public = aPatternMatchingNode "trycatch"
def Outer: Pattern is public = aPatternMatchingNode "outer"
def MethodSignature: Pattern is public = aPatternMatchingNode "methodtype"
def TypeLiteral: Pattern is public = aPatternMatchingNode "typeliteral"
def TypeDeclaration: Pattern is public = aPatternMatchingNode "typedec"
def TypeAnnotation: Pattern is public = aPatternMatchingNode "dtype"
def Member: Pattern is public = aPatternMatchingNode "member"
def Method: Pattern is public = aPatternMatchingNode "method"
def Parameter: Pattern is public = aPatternMatchingNode "parameter"
// matches anything that is a call
def Request: Pattern is public = booleanPattern { x → x.isCall }
def Class: Pattern is public = aPatternMatchingNode "class"
def ObjectLiteral: Pattern is public = aPatternMatchingNode "object"
def ArrayLiteral: Pattern is public = aPatternMatchingNode "array"
def Generic: Pattern is public = aPatternMatchingNode "generic"
def Identifier: Pattern is public = aPatternMatchingNode "identifier"
def OctetsLiteral: Pattern is public = aPatternMatchingNode "octets"
def StringLiteral: Pattern is public = aPatternMatchingNode "string"
def NumberLiteral: Pattern is public = aPatternMatchingNode "num"
def Operator: Pattern is public = aPatternMatchingNode "op"
def Bind: Pattern is public = aPatternMatchingNode "bind"
def Def: Pattern is public = aPatternMatchingNode "defdec"
def Var: Pattern is public = aPatternMatchingNode "vardec"
def Import: Pattern is public = aPatternMatchingNode "import"
def Dialect: Pattern is public = aPatternMatchingNode "dialect"
def Return: Pattern is public = aPatternMatchingNode "return"
def Inherit: Pattern is public = aPatternMatchingNode "inherit"
def Module: Pattern is public = aPatternMatchingNode "module"
