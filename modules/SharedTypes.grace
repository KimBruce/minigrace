#pragma ExtendedLineups
#pragma noTypeChecks
dialect "none"
import "standardGrace" as sg

import "ast" as ast
import "xmodule" as xmodule
import "io" as io

inherit sg.methods

// Error resulting from type checking
def StaticTypingError is public = Exception.refine "StaticTypingError"

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
method stripNewLines(str) → String {
    str.replace("\n")with(" ")
}

//This type is used for checking subtyping
type TypePair = {
    first → ObjectType
    second → ObjectType
    == (other:Object)→ Boolean
    asString → String
}

//This type is used for checking subtyping
type Answer = {
    ans → Boolean
    trials → List⟦TypePair⟧
    asString → String
}

//Stores needed information used when type-checking a type defined by an opNode
type TypeOp = {
    op → String
    left → ObjectType
    right → ObjectType
}

// type of a parameter
type Param = {
    name → String
    typeAnnotation → ObjectType
}

// generators for parameters
type ParamFactory = {
    withName (name' : String) ofType (type' : ObjectType) → Param
    ofType (type' : ObjectType) → Param
}

// MixPart is a "segment" of a method:
// Ex. for (param1) do (param2): for (param1, and do (param2)
// are separate "MixParts."
type MixPart = {
    name → String
    parameters → List⟦Param⟧
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
    retType → ObjectType

    // optional type parameters
    typeParams → List⟦String⟧

    // checks if method has type parameters
    hasTypeParams → Boolean

    // check equivalence of part names, param types, and return type;
    // does not check if param names are the same
    == (other: MethodType) → Boolean

    // create restriction of method type using other ?? Needs better descr.
    restriction (other : MethodType) → MethodType

    // Does it extend other
    isSpecialisationOf (trials : List⟦TypePair⟧, other : MethodType) → Answer

    // pre-condition: This MethodType has type parameters
    apply(replacementTypes : List⟦ObjectType⟧) → MethodType

    // Takes a mapping of generic-to-ObjectType and returns a copy of self
    // with all of the generics replaced with their corresponding ObjectType
    updateTypeWith(replacements:Dictionary⟦String, ObjectType⟧) → MethodType

    hash -> Number
}

// Type of generator creating method types by various means
type MethodTypeFactory = {
    // Create method type from info on parameters and return type
    signature (signature' : List⟦MixPart⟧)
            returnType (rType : ObjectType)→ MethodType

    // Create method type for parameterless method
    member (name : String) ofType (rType : ObjectType) → MethodType

    // Create method type for imported method
    fromGctLine (line : String, importName: String) → MethodType

    // Create method type from AST node representing method
    fromNode (node: AstNode) → MethodType
}

// Representation of type with type parameters
type GenericType = {
    // name of the type
    name → String

    // List of type parameters
    typeParams → List⟦String⟧

    // base type (removing type variables)
    oType → ObjectType

    // instantiate type by replacing type variables
    apply (replacementTypes : List⟦ObjectType⟧) → ObjectType
}

// Type of generator creating generic types
type GenericTypeFactory = {
    // Create generic type from name, parameters, and base type
    fromName (name' : String) parameters (typeParams' : List⟦String⟧)
                                  objectType (oType' : ObjectType) → GenericType

    // Create generic type from AST node representing type definition
    fromTypeDec (node : AstNode) → GenericType
}

// Generic type used in type checking, supertype of all others
type ObjectType = {
    // Is this type built using & or |?
    isOp -> Boolean

    // Does this type have a name
    isId -> Boolean

    // Does this represent a type variable?
    isTypeVble -> Boolean

    // Is this object type built from a collection of methods?
    isMeths -> Boolean

    // Return the list of sets of methods of the type
    // Needed for building types with & or |
    methList -> List[[Set[[MethodType]]]]

    // Create new object type from self and other using op
    withOp(op: String, other: ObjectType) -> ObjectType

//    getMethod (name : String) → MethodType | noSuchMethod
//    resolve → ObjectType
//    isResolved → Boolean

    // Does this type represent the dynamic or unknown type
    isDynamic → Boolean

    // Is this just a placeholder for real type?
    isPlaceholder -> Boolean
    
    == (other:ObjectType) → Boolean

    // Determines if self is a subtype of other
    isSubtypeOf (other : ObjectType) → Boolean
//    isSubtypeHelper (trials : List⟦TypePair⟧, other : ObjectType) → Answer
//    restriction (other : ObjectType) → ObjectType
//    isConsistentSubtypeOf (other : ObjectType) → Boolean
//    getVariantTypes → List⟦ObjectType⟧
//    setVariantTypes(newVariantTypes:List⟦ObjectType⟧) → Done
//    getOpNode → TypeOp
//    setOpNode (op : TypeOp) → Done

    // Construct new type as self | other
    | (other : ObjectType) → ObjectType

    // Construct new type as self & other
    & (other : ObjectType) → ObjectType

    // replace type variables with object types using replacements
    updateTypeWith(replacements:Dictionary[[String,ObjectType]]) -> ObjectType
}

type ObjectTypeFromMeths = ObjectType & interface {
    // return the set of methods of the type
    methods → Set⟦MethodType⟧
}

// type of object built from & or |
type ObjectTypeFromOp = ObjectType & interface {
    // operator used to build object type: & or |
    op -> String
    
    left -> ObjectType
    right -> ObjectType
}

// methods to create an object type from various inputs
type ObjectTypeFactory = {
    
    // Create an ObjectType from a collection of method signatures
    fromMethods (methods' : Set⟦MethodType⟧) → ObjectTypeFromMeths
    
    // Create an ObjectType from a collection of method signatures and a name
    fromMethods(methods' : Set⟦MethodType⟧) withName(name : String) → ObjectTypeFromMeths
    
    // Create an ObjectType using | or &
    makeWithOp(op:String, left: ObjectType, right: ObjectType) -> ObjectType
    
    //takes an AstNode representing a type and returns its corresponding ObjectType
    fromDType (dtype) with (lst) → ObjectType
    
    //Create an ObjectType from a GenericNode; the type used in the GenericNode
    //must have already been declared and put into the generics scope. The types
    //scope will also contain the returned ObjectType after this method is done.
    fromGeneric (node : Generic) → ObjectType
    
    //Find ObjectType corresponding to the identifier in the scope. If not
    //already there, adds it to the scope.
    fromIdentifier(ident : Identifier) with (lst) → ObjectType
    
    // ObjectType corresponding to a type variable (e.g. from generic type)
    typeVble(name': String) -> ObjectType
    
    // Create a type corresponding to Dynamic or unknown
    dynamic → ObjectType
    
    // Type corresponding to bottom element, i.e., has all methods, so subtype of all
    bottom → ObjectType
    
    // type of a block with params and return type rType
    blockTaking(params:List⟦Parameter⟧) returning(rType:ObjectType) → ObjectType
    
    // type of block without prarameters returning value of type rType
    blockReturning (rType : ObjectType) → ObjectType
    
    // Used for type-checking imports; please update when additional types are
    // added
    preludeTypes → Set⟦String⟧
    
    // represents type "Object"
    base → ObjectType
    
    // type Done
    doneType → ObjectType

    // other built-in types from prelude
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
}

// AST node type, kind tells what kind of node it is,
// e.g., identifier, generic, literal, etc
type AstNode = { kind → String }

// Create a pattern for matching kind for match
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
        } else {false }
    }
}

// Same as Pattern??
//type Matcher = {
//    match(obj: AstNode) → MatchResult | false
//}

// A pattern that matches if parameter satisfies predicate
// Use in matches
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
