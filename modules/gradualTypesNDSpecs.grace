#pragma ExtendedLineups
//#pragma noTypeChecks
dialect "none"
import "standardGrace" as sg

import "ast" as ast 
import "xmodule" as xmodule
import "io" as io

inherit sg.methods


// Checker error

def CheckerFailure is public = Exception.refine "CheckerFailure"

var methodtypes := [ ]
// visitor to convert a type expression to a string
def typeVisitor: ast.AstVisitor

// convert type expression to string for debugging
method dtypeToString(dtype) 

// The cached type assignments.
def cache: Dictionary = emptyDictionary

// Copied from Dialect2
// data structure to hold cached type assignments

// Scope represents stack of scopes using stackOfKind

type StackOfKind⟦V⟧ = {
    stack → List⟦Dictionary⟧
    at (name : String) put (value:V) -> Done
    find (name : String) butIfMissing (bl: Function0⟦V⟧) → V
}

type Scope = {
    // keep track of each kind of expression separately
    variables → StackOfKind⟦ObjectType⟧
    methods → StackOfKind⟦MethodType⟧
    types → StackOfKind⟦ObjectType⟧
    // number of items on stack
    size → Number
    // Enter new scope to execute block bl and then delete afterwards
    // returns value of bl
    enter⟦V⟧(bl: Function0⟦V⟧) → V
}

// scope consists of stacks of scopes for each of variables, methods, & types
def scope: Scope is public

// check the type of node and insert into cache only
method checkTypes (node: AstNode) → Done 

// check type of node, insert into cache, and then return the type
// THIS IS THE MOST IMPORTANT METHOD!
method typeOf (node: AstNode) → ObjectType

type AstNode = { kind -> String }

// Create a pattern for matching kind
class aPatternMatchingNode (kind : String) -> Pattern 

type Matcher = {
    match(obj: AstNode) → MatchResult
}

// A pattern that matches if parameter satisfies predicate
class booleanPattern (predicate: Function1⟦AstNode⟧) → Pattern 

// patterns for built-in AST Nodes
// Making them patterns means can use in match statement
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


// -------------------------------------------------------------------
// Type declarations for type representations to use for type checking
// -------------------------------------------------------------------

// Method signature information.
// isSpecialization and restriction are used for type-checking
type MethodType = {
    name -> String
    signature -> List⟦MixPart⟧
    returnType -> ObjectType
    isSpecialisationOf (other : MethodType) -> Boolean
    restriction (other : MethodType) -> MethodType
}

// type of a parameter
type Param = {
    name -> String
    typeAnnotation -> ObjectType
}

type ParamFactory = {
    withName (name' : String) ofType (type' : ObjectType) -> Param
    ofType (type' : Object) -> Param
}
 
// Create parameter with given name' and type'
// if no name then use wildcard "_"
def aParam: ParamFactory 

// MixPart is a "segment" of a method:
// Ex. for (param1) do (param2), for and do are separate "MixParts."
type MixPart = {
    name -> String
    parameters -> List⟦Param⟧
}

// create a mixpart with given name' and parameters'
class aMixPartWithName(name' : String)
        parameters(parameters' : List⟦Param⟧) -> MixPart 

type MethodTypeFactory = {
    // create method type from list of mixparts and return type
    signature (signature' : List⟦MixPart⟧)
            returnType (rType : ObjectType)-> MethodType
    // Create method type with no parameters, but returning rType
    member (name : String) ofType (rType : ObjectType) -> MethodType
    // Parses a methodtype line from a gct file into a MethodType object.
    // Lines will always be formatted like this:
    //  1 methname(param : ParamType)part(param : ParamType) -> ReturnType
    // The number at the beginning is ignored here, since it is handled
    // down in the Imports rule.
    fromGctLine (line : String) -> MethodType
    // if node is a method, class, method signature,
    // def, or var, create appropriate method type
    fromNode (node: AstNode) -> MethodType
}
    
// factory for creating method types from various inputs
def aMethodType: MethodTypeFactory 

// default value when no match for method name
def noSuchMethod: outer.Pattern

// represents the type of an expression as a collection of method types
type ObjectType = {
    methods -> Set⟦MethodType⟧
    // getMethod (name : String) -> MethodType | noSuchMethod
    isDynamic -> Boolean
    isSubtypeOf (other : ObjectType) -> Boolean
    | (other : ObjectType) -> ObjectType
    & (other : ObjectType) -> ObjectType
    restriction (other : ObjectType) -> ObjectType
    isConsistentSubtypeOf (other : ObjectType) -> Boolean
}

// methods to create an object type from various inputs
type ObjectTypeFactory = {
    placeHolder -> ObjectType
    // create an objectType from the given methods
    fromMethods (methods' : Set⟦MethodType⟧) -> ObjectType
    // fixes asString to include name
    fromMethods (methods' : Set⟦MethodType⟧)
                                withName (name : String) -> ObjectType
    fromDType (dtype: AstNode) -> ObjectType
    fromIdentifier(ident : Identifier) -> ObjectType
    fromBlock (block: AstNode) -> ObjectType
    fromBlockBody (body: Sequence⟦AstNode⟧) -> ObjectType
    dynamic -> ObjectType
    bottom -> ObjectType
    blockTaking (params : List⟦Parameter⟧)
            returning (rType : ObjectType) -> ObjectType
    blockReturning (rType : ObjectType) -> ObjectType
    base -> ObjectType
    doneType -> ObjectType
    pattern -> ObjectType
    iterator -> ObjectType
    boolean -> ObjectType
    number -> ObjectType
    string -> ObjectType
    listTp -> ObjectType
    set -> ObjectType
    sequence -> ObjectType
    dictionary -> ObjectType
    point -> ObjectType
    binding -> ObjectType
    collection -> ObjectType
    enumerable -> ObjectType
}

def anObjectType: ObjectTypeFactory

// Adds name to variables and as parameterless method (really def, not var!) in current scope
method addVar (name : String) ofType (oType : ObjectType) → Done

// Exceptions while type-checking Object literals.

def ObjectError: outer.ExceptionKind = TypeError.refine("ObjectError")

// Class declaration.

def ClassError: outer.ExceptionKind = TypeError.refine("Class TypeError")

def MethodError = TypeError.refine("Method TypeError")

// Def and var declarations.

def DefError: outer.ExceptionKind = TypeError.refine("Def TypeError")

type RequestPart = {
   args -> List[[AstNode]]
   args:=(a: List[[AstNode]]) -> Done
}

type RequestPartFactory = {
   new -> RequestPart
   request(name: String) withArgs(argList) scope (s) -> RequestPart
   request(rPart) withArgs (sx) -> RequestPart
}

// Check if the signature and parameters of a request match
// the declaration, return the type of the result
method check(req : Request)
        against(meth : MethodType) -> ObjectType is confidential

// Throw error only if type of node is not consistent subtype of eType
method check (node: AstNode) matches(eType : ObjectType)
        inMethod (name : String) -> Done is confidential
	
// break of input string into list of strings as divided by separator
method split(input : String, separator : String) -> List⟦String⟧ 

// Static type checker visitor
// methods return false if goes no further recursively
def astVisitor: ast.AstVisitor 

// DEBUG: Outer not handled correctly yet

method outerAt(i : Number) -> ObjectType is confidential 

// Typing methods.

method processBody(body: List⟦AstNode⟧, superclass: AstNode | false) -> ObjectType is confidential

def TypeDeclarationError = TypeError.refine "TypeDeclarationError"

// The first pass over a body, collecting all type declarations so that they can
// reference one another declaratively.
method collectTypes(nodes : Collection⟦AstNode⟧) -> Done is confidential


// Determines if a node is publicly available.
method isPublic(node : Method | Def | Var) -> Boolean is confidential 


// Determines if a method will be accessed as a member.
method isMember(mType : MethodType) -> Boolean is confidential


// Helper methods.

// For loop with break.
method for(a) doWithBreak(bl) -> Done 

// For loop with continue.
method for(a) doWithContinue(bl) -> Done

method continue'(e, bl) -> Done is confidential 

// Replace newline characters with spaces. This is a
// workaround for issue #116 on the gracelang/minigrace
// git repo. The result of certain astNodes.toGrace(0)
// is a string containing newlines, and error messages
// containing these strings get cut off at the first
// newline character, resulting in an unhelpful error
// message.
method stripNewLines(str: String) -> String is confidential

class parameterFromNode (node) -> Parameter is confidential

def thisDialect is public


