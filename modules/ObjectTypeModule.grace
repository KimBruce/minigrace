#pragma ExtendedLineups
#pragma noTypeChecks
dialect "none"
import "standardGrace" as sg

import "lexer" as lexer
import "parser2" as parser
import "ast" as ast
import "xmodule" as xmodule
import "io" as io
import "ScopeModule" as sc
import "SharedTypes" as share

inherit sg.methods

// Error resulting from type checking
def StaticTypingError is public = Exception.refine "StaticTypingError"

// rename imported types for convenience
type MethodType = share.MethodType
type MethodTypeFactory = share.MethodTypeFactory
type GenericType = share.GenericType
type GenericTypeFactory = share.GenericTypeFactory
type ObjectType = share.ObjectType
type ObjectTypeFactory = share.ObjectTypeFactory
type AstNode = share.AstNode
type Parameter = share.Parameter
type ObjectTypeFromOp = share.ObjectTypeFromOp
type ObjectTypeFromMeths = share.ObjectTypeFromMeths
def scope: share.Scope = sc.scope

// Scoping error declaration
def ScopingError: outer.ExceptionKind is public =
                                                TypeError.refine("ScopingError")

// If true prints extra info
def debug : Boolean = false

// Collection of method types in type
var methodtypes: List[[MethodType]] := [ ]

// visitor to convert a type expression to a string
// Makes printing them a bit clearer. Used in gct?
def typeVisitor: ast.AstVisitor = object {
    inherit ast.baseVisitor
    var literalCount := 1

    // Convert type with list of method types to string
    method visitTypeLiteral(lit) {
        for (lit.methods) do { meth →
            var mtstr := "{literalCount} "
            for (meth.signature) do { part →
                mtstr := mtstr ++ part.name
                if (part.params.size > 0) then {
                    mtstr := mtstr ++ "("
                    for (part.params.indices) do { pnr →
                        var p := part.params.at(pnr)
                        if (p.dtype != false) then {
                            mtstr := mtstr ++ p.toGrace(1)
                        } else {
                            // if parameter type not listed, give it type Unknown
                            if (p.wildcard) then {
                                mtstr := mtstr ++ "_"
                            } else {
                                mtstr := mtstr ++ p.value
                            }
                            mtstr := mtstr ++ ":" ++ ast.unknownType.value
                            if (false != p.generics) then {
                                mtstr := mtstr ++ "⟦"
                                for (1..(p.generics.size - 1)) do {ix →
                                    mtstr := mtstr ++ p.generics.at(ix).toGrace(1) ++ ", "
                                }
                                mtstr := mtstr ++ p.generics.last.toGrace(1) ++ "⟧"
                            }
                        }
                        if (pnr < part.params.size) then {
                            mtstr := mtstr ++ ", "
                        }
                    }
                    mtstr := mtstr ++ ")"
                }
            }
            if (meth.rtype != false) then {
                mtstr := mtstr ++ " → " ++ meth.rtype.toGrace(1)
            }
            methodtypes.add(mtstr)
        }
        return false
    }

    // Get string representation of type built using & or |
    method visitOp(op) {
        if ((op.value=="&") || (op.value=="|")) then {
            def leftkind = op.left.kind
            def rightkind = op.right.kind
            if ((leftkind=="identifier") || (leftkind=="member")) then {
                var typeIdent := op.left.toGrace(0)
                methodtypes.add("{op.value} {typeIdent}")
            } elseif { leftkind=="typeliteral" } then {
                literalCount := literalCount + 1
                methodtypes.add("{op.value} {literalCount}")
                visitTypeLiteral(op.left)
            } elseif { leftkind=="op" } then {
                visitOp(op.left)
            }
            if ((rightkind=="identifier") || (rightkind=="member")) then {
                var typeIdent := op.right.toGrace(0)
                methodtypes.add("{op.value} {typeIdent}")
            } elseif { rightkind=="typeliteral" } then {
                literalCount := literalCount + 1
                methodtypes.add("{op.value} {literalCount}")
                visitTypeLiteral(op.right)
            } elseif { rightkind=="op" } then {
                visitOp(op.right)
            }
        }
        return false
    }
}

// convert type expression to string for debugging
//
// suggestion: print typeLiteral by calling dtype.toGrace(0)
method dtypeToString(dtype) {
    if (false == dtype) then {
        "Unknown"
    } elseif {dtype.kind == "typeliteral"} then {
        methodtypes := []
        dtype.accept(typeVisitor)
        methodtypes.at (1)
    } else {
        dtype.value
    }
}


// -------------------------------------------------------------------
// Type declarations for type representations to use for type checking
// -------------------------------------------------------------------

// Used in match statements to catch indicate no method found
// Method returns these when no method found.
def noSuchMethod: outer.Pattern is readable = object {
    inherit BasicPattern.new

    method match(obj : Object) {
        if (self.isMe(obj)) then {
            SuccessfulMatch.new(self, list[])
        } else {
            FailedMatch.new(obj)
        }
    }
}

// Used in match statements to indicate no type found
def noSuchType: outer.Pattern = object {
    inherit BasicPattern.new

    method match(obj : Object) {
        if (self.isMe(obj)) then {
            SuccessfulMatch.new(self, list[])
        } else {
            FailedMatch.new(obj)
        }
    }
}

//This type is used for checking subtyping
type TypePair = share.TypePair

// Pair of types, used in coinductive check of subtyping
class typePair (first':ObjectType, second':ObjectType) →
                                   TypePair is confidential {
    method first → ObjectType {
        first'
    }

    method second → ObjectType {
        second'
    }

    method == (other:Object) → Boolean {
        (first == other.first) && (second == other.second)
    }

    method asString {
        "<{first'} , {second'}>"
    }
}

//This type is used for checking subtyping
type Answer = share.Answer

// Responds that keeps trail of types checked in subtyping
class answerConstructor(ans':Boolean, trials':List⟦TypePair⟧) → Answer {
    method ans → Boolean {
        ans'
    }

    method trials → List⟦TypePair⟧ {
        trials'
    }

    method asString → String{
        "Answer is {ans'}\n Trials is {trials}"
    }
}

type TypeOp = share.TypeOp

// Stores needed information used when type-checking a type defined by an opNode
// I.e., & or |
class typeOp(op' : String, left' : ObjectType, right' : ObjectType) → TypeOp {
    method op → String {
        op'
    }

    method left → ObjectType {
        left'
    }

    method right → ObjectType {
        right'
    }
}

// type of a parameter
type Param = share.Param

type ParamFactory = share.ParamFactory

// Create parameter with given name' and type'
// if no name then use wildcard "_"
def aParam: ParamFactory is readable = object {
    method withName(name': String) ofType(type' : ObjectType) → Param {
        object {
            def name : String is public = name'
            def typeAnnotation : ObjectType is public = type'

            method asString → String is override {
                "{name} : {typeAnnotation}"
            }
        }
    }

    method ofType (type': ObjectType) → Param {
        withName("_") ofType(type')
    }
}

// MixPart is a "segment" of a method: Ex. for (param1) do (param2), for(param1)
// and do(param2) are separate "MixParts."
type MixPart = share.MixPart

// create a mixpart with given name' and parameters'
class aMixPartWithName(name' : String)
            parameters(parameters' : List⟦Param⟧) → MixPart {
    def name : String is public = name'
    def parameters : List⟦Param⟧ is public = parameters'
}

// =================================
// METHOD TYPES
// =================================
// factory for creating method types from various inputs
def aMethodType: MethodTypeFactory is public = object {

    // Create method type from signature (including parameters
    // and their types) and return type
    method signature (signature' : List⟦MixPart⟧)
                      returnType (retType' : ObjectType) → MethodType {
        signature (signature') with (emptyList[[String]])
                         returnType (retType')
    }

    // Create method type from signature (including parameters
    // and their types) and return type, as well as type parameters
    method signature (signature' : List⟦MixPart⟧)
                     with (typeParams': List[[String]])
                       returnType (retType' : ObjectType) → MethodType {
        object {
            //Public defs of MethodType
            var name : String is readable := ""
            var nameString : String is readable := ""
            def signature : List⟦MixPart⟧ is public = signature'
            def retType : ObjectType is public = retType'
            def typeParams : List⟦String⟧ is public = typeParams'

            //Initialize name, nameString, and show (for the method asString)
            var show : String := ""
            def fst: MixPart = signature.first
            if (fst.parameters.isEmpty) then {
                name := fst.name
                nameString := fst.name
                show := name
            } else {
                var onceType : Boolean := true

                for (signature) do { part →
                    if (onceType && (typeParams != emptyList[[String]])) then {
                        name := "{name}{part.name}[{typeParams}]()"
                        nameString := ("{nameString}{part.name}[{typeParams}]" ++
                                                    "({part.parameters.size})")
                    } else {
                        onceType := false

                        name := "{name}{part.name}()"
                        nameString := ("{nameString}{part.name}" ++
                                    "({part.parameters.size})")
                    }
                    show := "{show}{part.name}("
                    var once: Boolean := false
                    for (part.parameters) do { param →
                        if (once) then {
                            show := "{show}, "
                        }
                        show := "{show}{param}"
                        once := true
                    }
                    show := "{show})"
                }

                // Throw away ", " at end of name
                name := name.substringFrom (1) to (name.size - 2)
            }
            show := "{show} → {retType}"

            method hash → Number is override {
                nameString.hash
            }

            // Does method take type parameters
            method hasTypeParams → Boolean {
                typeParams.size > 0
            }

            //Two method types are considered equal if they have the same
            //nameString, parameter types, and return type
            method == (other: MethodType) → Boolean {
                //Check part names and number of parameters
                if (nameString ≠ other.nameString) then {
                    return false
                }

                //Check parameter types and return type
                for (signature) and (other.signature)
                                            do { part: MixPart, part': MixPart →
                    for (part.parameters) and (part'.parameters)
                                                      do { p: Param, p': Param →
                        if (p.typeAnnotation ≠ p'.typeAnnotation) then {
                            return false
                        }
                    }
                }
                return (retType == other.retType)
            }

            // Mask unknown fields in corresponding methods
            // Assume that the methods share a signature.
            // Results in stripped down signature
            // Used for gradual typing  NOT CHECKED FOR CORRECTNESS!
            method restriction (other : MethodType) → MethodType {
                def restrictParts: List⟦MixPart⟧ = list[]

                if (other.signature.size != signature.size) then {
                    return self
                }

                for (signature) and (other.signature)
                                        do {part: MixPart, part': MixPart →
                    if (part.name == part'.name) then {
                        def restrictParams: List⟦Param⟧ = list[]
                        if (part.parameters.size != part'.parameters.size) then{
                            ProgrammingError.raise("part {part.name} has " ++
                                    "{part.parameters.size} while part " ++
                                    "{part'.name} has {part'.parameters.size}")
                        }

                        for (part.parameters) and (part'.parameters)
                                                do { p: Param, p': Param →
                            def pt': ObjectType = p'.typeAnnotation

                            // Contravariant in parameter types.
                            if (pt'.isDynamic) then {
                                restrictParams.add(aParam.withName (p.name)
                                                  ofType (anObjectType.dynamic))
                            } else {
                                restrictParams.add(p)
                            }
                        }

                        restrictParts.add (
                            aMixPartWithName (part.name)
                                   parameters (restrictParams))
                    } else {
                        restrictParts.add (part)
                    }
                }
                return signature (restrictParts) with (typeParams)
                                               returnType (retType)
            }

            // Determines if this method is a specialisation (<:) of
            // the given one.
            method isSpecialisationOf (trials: List⟦TypePair⟧,
                                         other: MethodType) → Answer {
                //Check part names and number of parameters
                if (nameString != other.nameString) then {
                    return answerConstructor(false, trials)
                }

                //Check that both methods have the same number of type params
                if (typeParams.size ≠ other.typeParams.size) then {
                    return answerConstructor(false, trials)
                }

                //Determine whether each method still have uninitialized
                //type params
                if (typeParams.size > 0) then {
                    return self.genericSpecialisationOf(trials, other)
                } else {
                    return self.nongenericSpecialisationOf(trials, other)
                }
            }

            // Initialize self's and other's type params with type Object, then
            // perform specialisation check
            //
            // pre: self and other has the same # of uninitialized type params
            // TODO: Hack which is not right!!!
            method genericSpecialisationOf(trials: List⟦TypePair⟧,
                                                  other: MethodType) → Answer {
                //We use '$Object$' instead of 'Object' because Object can be
                //overwritten by the programmer
                def baseList : List⟦AstNode⟧ = emptyList⟦AstNode⟧
                for (typeParams.indices) do { index : Number →
                    baseList.add(ast.identifierNode.new("$Object$", false))
                }

                //Initialize both methods
                def appliedSelf : MethodType = self.apply(baseList)
                def appliedOther : MethodType = other.apply(baseList)

                if (debug) then {
                    io.error.write("\n351 about to check appliedSelf " ++
                                "{appliedSelf} against {appliedOther}")
                }

                return appliedSelf.isSpecialisationOf(trials, appliedOther)
            }

            // Perform specialisation check on a pair of MethodTypes that have
            // no uninitialized type params
            method nongenericSpecialisationOf(trials: List⟦TypePair⟧,
                                                  other: MethodType) → Answer {
                //Shortcut for when self and other have the exact same param
                //types and return type
                if (self == other) then {
                    return answerConstructor(true, trials)
                }

                //Check subtyping of param and return types
                for (signature) and (other.signature)
                                            do { part: MixPart, part': MixPart →
                    for (part.parameters) and (part'.parameters)
                                                    do { p: Param, p': Param →
                        def pt: ObjectType = p.typeAnnotation
                        def pt': ObjectType = p'.typeAnnotation

                        // Contravariant in parameter types.
                        def paramSubtyping : Answer =
                                                pt'.isSubtypeHelper(trials, pt)

                        if (paramSubtyping.ans.not) then {
                            return paramSubtyping
                        }
                    }
                }

                return retType.isSubtypeHelper (trials, other.retType)
            }

            // Update this method's type params with the replacementTypes
            // Returns a different MethodType with the correct types
            // TODO: Modify or delete
            method apply(replacementTypes : List⟦AstNode⟧) → MethodType {
                if(replacementTypes.size ≠ typeParams.size) then {
                    TypeError.raise("Wrong number of type parameters " ++
                            "given when instantiating generic method " ++
                            "{nameString}. Attempted to replace " ++
                            "{typeParams} with {replacementTypes}.")
                }
                //Create a mapping of GenericTypes-to-ObjectTypes
                if (debug) then {
                     io.error.write ("\n444: updating {self} with replacement "++
                          "types: {replacementTypes}")
                }
                def replacements : Dictionary⟦String, AstNode⟧ =
                           makeDictionary(typeParams,replacementTypes)
                updateTypeWith(replacements)
            }

            // Takes a mapping of generic-to-ObjectType and returns a copy of
            // self with all of the generics replaced with their corresponding
            // ObjectType
            //
            // Note : Does not need 'replacements.size == typeParams.size'
            method updateTypeWith (replacements :
                              Dictionary⟦String, ObjectType⟧) → MethodType {
                if (debug) then {
                     io.error.write "\n457: updating method: {self}"
                }
                //Construct the list of mixParts of the new MethodType
                def newMixParts : List⟦MixPart⟧ = emptyList⟦MixPart⟧
                def debug3 = false
                for (signature) do { mPart : MixPart →
                    def newParams : List⟦Param⟧ = emptyList⟦Param⟧
                    for (mPart.parameters) do { param : Param →
                        def newTypeAnno : ObjectType =
                            param.typeAnnotation.updateTypeWith(replacements)

                        if (debug3) then {
                            io.error.write("\n389 newTypeAnno is {newTypeAnno}")
                        }
                        newParams.add(aParam.withName (param.name)
                                                      ofType (newTypeAnno))
                    }
                    newMixParts.add(aMixPartWithName(mPart.name)
                                                      parameters(newParams))
                }

                if(debug3) then {
                     io.error.write "\n469: new mix parts {newMixParts.at (1).name}"
                     io.error.write "\n471: replace {retType} with {replacements}"
                }

                //Update the return type of the method
                def newReturn : ObjectType = retType.updateTypeWith (replacements)
                if(debug3) then {
                     io.error.write "\n486: new return type: {newReturn}"
                }

                //Return with new MethodType
                // TODO: start with empty typeParams???
                def newMeth : MethodType = signature(newMixParts) with (typeParams)
                                                    returnType(newReturn)
                if {debug3} then {
                    io.error.write "\n481: newMeth is {newMeth}"
                }

                //Save any type params that weren't initialized
                // TODO: Shouldn't be necessary!!
                def newTypeParams : List⟦String⟧ = emptyList⟦String⟧
                for (typeParams) do { each : String →
                    if (replacements.containsKey(each).not) then {
                        newTypeParams.add(each)
                    }
                }
                newMeth.typeParams.addAll(newTypeParams)
                newMeth
            }

            // DELETE: UNUSED
            // Replaces the oType that is holding a node representing an
            // uninitialized type param with its replacement in the dictionary
            //
            // Note: -type params are stored as identifiernodes
            //       -it is possible that a type param will remain uninitialized
            //          if the dictionary does not contain its name
            method replaceType2(oType : ObjectType)
                            with(replacements : Dictionary⟦String, AstNode⟧)
                                                  → ObjectType is confidential {
                // If the oType is already initialized, it can't be a type param
                if (oType.isResolved) then {
                    return oType
                }

                // Call the helper function with the AstNode stored in oType
                def replacedNode : AstNode = replaceNode2(oType.getNode)
                                                              with(replacements)
                // used to be definedByNode
                return anObjectType.fromDType(replacedNode)with(emptyList)
            }

            // DELETE: UNUSED
            // Replaces node with replacements to instantiate generic
            method replaceNode2(node : AstNode)
                            with(replacements : Dictionary⟦String, AstNode⟧)
                                                    → AstNode is confidential {
                if (node.kind == "generic") then {
                    def newArgs : List⟦AstNode⟧ = emptyList⟦AstNode⟧
                    for (node.args) do { arg : AstNode →
                        newArgs.add(replaceNode2(arg) with(replacements))
                    }
                    return ast.genericNode.new(node.value, newArgs)
                } elseif {node.kind == "op"} then {
                    def left: AstNode= replaceNode2(node.left) with(replacements)
                    def right:AstNode= replaceNode2(node.right)with(replacements)
                    return ast.opNode.new(node.value, left, right)
                } elseif {node.kind == "identifier"} then {
                    if (replacements.containsKey(node.nameString)) then {
                        return replacements.at (node.nameString)
                    }
                }
                return node
            }

            method asString → String is override { show }
        }
    }

    // Create method type with no parameters, but returning rType
    method member (name : String) ofType (rType : ObjectType) → MethodType {
        signature(list[aMixPartWithName (name) parameters (list[])]) with (emptyList[[String]])
                                                              returnType (rType)
    }

    // if node is a method, class, method signature,
    // def, or var, create appropriate method type
    // Included just to catch errors
    method fromNode (node: AstNode) → MethodType {
        ProgrammingError.raise("Wrong version of fromNode in 566")
    }

    // if node is a method, class, method signature,
    // def, or var, create appropriate method type
    method fromNode (node: AstNode) with (typeParams: List[[String]]) → MethodType {
        def debug3 = true
        match(node) case{ meth :share.Method|share.Class|share.MethodSignature →
            //BREAK CASE OUT AS SEPARATE HELPER METHOD!!
            def signature: List⟦MixPart⟧ = list[]
            if (debug3) then {
               io.error.write "\n575: create method from {node} with type vars:{typeParams}"
            }
            for (meth.signature) do { part:AstNode →
                def params: List⟦Param⟧ = list[]
                if (debug) then {
                     io.error.write "\n568 creating type for {part}"
                }

                // Collect parameters for each part
                for (part.params) do { param: AstNode →
                    if (debug) then {
                        io.error.write "\n571 creating type for {param}:{param.dtype}"
                    }
                    params.add (aParam.withName (param.value)
                        // used to be definedByNode
                        ofType (anObjectType.fromDType (param.dtype) with (typeParams)))
                }

                // Add this mixpart to signature list
                signature.add (aMixPartWithName (part.name) parameters (params))
                if (debug3) then {
                   io.error.write "\n579 finished with {part}"
                }
            }

            // return type of the method or class
            def rType: AstNode = match (meth)
                case { m : share.Method | share.Class → m.dtype}
                case { m : share.MethodSignature → m.rtype}
            if (debug3) then {
               io.error.write "\n587 creating returntype for {rType}"
            }

            // Full method type
            // used to use definedByNode
            // Add type parameters to the method type
            def newTypeParams:  List[[String]] = emptyList[[String]]
            if (false ≠ meth.typeParams) then {
                for (meth.typeParams.params) do { ident : share.Identifier →
                    newTypeParams.add(ident.nameString)
                }
            }
    
            def mType : MethodType = signature (signature) with (newTypeParams)
                returnType (anObjectType.fromDType (rType) with (typeParams))
            if (debug3) then {
               io.error.write "\n588: created method type {mType}"
               io.error.write "\n594: created method {mType} from {node}"
               io.error.write "\n678: created return type {anObjectType.fromDType (rType) with (typeParams)}"
            }
            mType

        } case { defd : share.Def | share.Var →

            def signature: List⟦MixPart⟧ =
                    list[aMixPartWithName (defd.name.value) parameters (list[])]
            def dtype: ObjectType = if (defd.dtype == false) then {
                anObjectType.dynamic
            } else {
                // used to use definedByNode
                anObjectType.fromDType (defd.dtype)  with (typeParams)
            }
            def methType = signature (signature) with (typeParams) returnType (dtype)
            if (debug3) then {
               io.error.write "\n607 created method type {methType} from def or var {node}"
            }
            methType
        } case { _ →
            Exception.raise "unrecognised method node" with(node)
        }
    }
}

// =====================
// GENERIC TYPES
// =====================

// Factory to create generic types (and instantiate them)
def aGenericType : GenericTypeFactory is public = object{

    //Create a GenericType from name, type params, & base type
    class fromName (name': String) parameters (typeParams' : List⟦String⟧)
                                objectType(oType' : ObjectType) → GenericType {
        //May be unnecessary since we already store 'name' as the key in scope
        def name : String is public = name'

        //The generic type names used within this type
        def typeParams : List⟦String⟧ is public = typeParams'

        //The ObjectType belonging to this generic type
        var oType : ObjectType is public:= oType'

        //Takes a list of replacement ObjectTypes and replaces references to the
        //typeParams stored in oType with their counterpart in the list
        method apply(replacementTypes : List⟦ObjectType⟧) → ObjectType {
            if(replacementTypes.size ≠ typeParams.size) then {
                TypeError.raise("Wrong number of type parameters given when " ++
                            "instantiating generic type {name}. Attempted to" ++
                              " replace {typeParams} with {replacementTypes}.")
            }

            //First, resolve the oType if needed
            oType := oType.resolve

            //Create a mapping of GenericTypes-to-ObjectTypes
            def replacements : Dictionary⟦String, ObjectType⟧ =
                              makeDictionary(typeParams, replacementTypes)

            //Tells each method to replace references to any of the typeParams
            def appliedMethods : Set⟦MethodType⟧ = emptySet⟦MethodType⟧
            for (oType.methods) do { meth : MethodType →
                appliedMethods.add(meth.updateTypeWith(replacements))
            }

            //Returns an ObjectType with the generics replaced
            anObjectType.fromMethods(appliedMethods)
        }

        method asString → String {
            var s : String := name ++ "⟦"
            var once : Boolean := true
            for (typeParams) do {typeParam : String →
                if (once.not) then {
                    s := "{s}, {typeParam}"
                } else {
                    once := false
                    s := "{s}{typeParam}"
                }
            }
            "{s}⟧ : {oType.asString}"
        }

    }

    //Create a GenericType from a typeDecNode
    method fromTypeDec(typeDec : AstNode) → GenericType {
        def debug3: Boolean = false
        def name : String = typeDec.nameString
        if (debug3) then {
            io.error.write "\n661 typeDec is {name}"
        }
        def typeParams : List⟦String⟧ = getTypeParams(typeDec.typeParams.params)

        if (debug3) then {io.error.write "\n699: new typeParams: {typeParams}"}

        var oType : ObjectType := anObjectType.fromDType(typeDec.value)
                                      with (typeParams)

        def genType = fromName(name) parameters(typeParams) objectType(oType)
        if (debug) then {
           io.error.write "\n691: Created {genType} from type dec {typeDec}"
        }
        genType
    }
}

// Convert type parameters on type or method declaration to list of strings
method getTypeParams(params: List[[AstNode]]) -> List[[String]] {
    def typeParams : List⟦String⟧ = emptyList[[String]]
    for (params) do {param: AstNode ->
        typeParams.add(param.nameString)
    }
    typeParams
}

// Object type information.
def anObjectType: ObjectTypeFactory is public = object {
    // super class providing default implementations of methods
    class superObjectType -> ObjectType {
        // is type built with & or |
        method isOp -> Boolean {false}

        // is it a type name
        method isId -> Boolean {false}

        // Does this represent a type variable?
        method isTypeVble -> Boolean {false}

        // Is this object type built from a collection of methods?
        method isMeths -> Boolean {false}

        // Return the list of sets of methods of the type
        // Needed for building types with & or |
        method methList -> List[[Set[[MethodType]]]] {
            emptyList[[Set[[MethodType]]]]
        }

        // Does this type represent the dynamic or unknown type
        method isDynamic -> Boolean {false}

        // Does this type represent the dynamic or unknown type
        method isPlaceholder -> Boolean {false}

        // Create new object type from self and other using op
        method withOp(op: String, other: ObjectType) -> ObjectTypeFromOp {
            if (debug) then {
               io.error.write"\n697 calling makeWithOp for {self}, {other}"
            }
            anObjectType.makeWithOp(op, self, other)
        }

        // Return set of methods of the type
        method methods -> Set[[MethodType]] {emptySet}

        // Is it a consistent subtype of other (for gradual typing0
        method isConsistentSubtypeOf(other: ObjectType) {
            isSubtypeOf(other)
        }

        // Is it a subtype of other
        method isSubtypeOf(other: ObjectType) -> Boolean {
            required
        }

        // Construct a variant type from two object types.
        // Note: variant types can only be created by this constructor.
        method | (other' : ObjectType) → ObjectType {
            withOp("|", other')
        }

        // Construct an & type from self and other'
        method & (other': ObjectType) -> ObjectType {
            withOp("&", other')
        }

        //TODO: not sure we trust this. May have to refine == in the future
        //method == (other:ObjectType) → Boolean { self.isMe(other) }
        method == (other:ObjectType) → Boolean {
            //io.error.write "\n873 check equality of {asString} and {other.asString}"
            asString == other.asString
        }

        // Instantiate generic parameters with tparams
        // For now, just return self
        // TODO: Not sure if this is ever used!
        method replaceTypeParams(tparams: List[[String]]) -> ObjectType {
            self
        }

        // Not sure purpose.  For now return self
        method resolve -> ObjectType {
            self
        }

        // replace type variables with object types using replacements
        method updateTypeWith(replacements: Dictionary[[String,ObjectType]])
                    -> ObjectType {
            if (debug) then {
               io.error.write "\n740 default updateType with {self}"
            }
            self
        }    
    }

    def placeholder: ObjectType is public = object{
       inherit superObjectType
       method isPlaceholder -> Boolean {true}
    }

    // Create an ObjectType from a collection of method signatures and a name
    class fromMethods (methods' : Set⟦MethodType⟧) withName (name : String)
                                                → ObjectType {
        inherit fromMethods(methods')

        // don't update because it has a name
        method updateTypeWith(replacements:Dictionary[[String,ObjectType]])
                        -> ObjectType {
            if (debug) then {
               io.error.write "\n749 update type in fromMethods {name}"
            }
            self
        }

        // Is type given with a name (yes in this case)
        method isId -> Boolean is override {true}

        method asString → String is override {name}
    }

    // build object type from collection of methods
    method fromMethods (methods': Set⟦MethodType⟧) → ObjectTypeFromMeths {
        object {
            inherit superObjectType
                alias oldAnd(_) = &(_)

            method isMeths -> Boolean {true}

            // Holds base methods as well as those in methods'
            def methods : Set⟦MethodType⟧ is public = (if (base == dynamic) then {
                emptySet } else { emptySet.addAll(base.methods) }).addAll(methods')

            method methList -> List[[Set[[MethodType]]]] {
                list[[Set[[MethodType]]]] [methods]
            }

            // Return method type with matching nameString or return noSuchMethod.
            method getMethod(name : String) → MethodType | noSuchMethod {
                for(methList) do { methSet →
                    for(methSet) do { meth ->
                        if (meth.nameString == name) then {
                            return meth
                        }
                    }
                }
                return noSuchMethod
            }

            method resolve -> ObjectType { self }

            def isResolved : Boolean is public = true

            // Check if 'self', which is the ObjectType calling this method, is
            // a subtype of 'other'
            method isSubtypeOf(other : ObjectType) → Boolean {
                isSubtypeHelper(emptyList⟦TypePair⟧, other.resolve).ans
            }

            // TODO: Make confidential
            // helper method for subtyping a pair of non-variant types
            // Keeps track of pairs already considered (co-induction)
            method isSimpleSubtypeOf(trials: List⟦TypePair⟧,
                                                  other:ObjectType) → Answer {
                def debug3 = true
                //for each method in other, check that there is a corresponding
                //method in self
                for (other.methods) doWithContinue { otherMeth: MethodType, continue →
                    for (self.methods) do {selfMeth: MethodType →
                        if (debug3) then {print "\n945 selfMeth: {selfMeth}"}
                        def isSpec: Answer =
                              selfMeth.isSpecialisationOf(trials, otherMeth)
                        if (debug3) then {print "\n947 isSpec: {isSpec}"}
                        if (isSpec.ans) then { continue.apply }
                    }
                    //fails to find corresponding method
                    return answerConstructor(false, trials)
                }
                return answerConstructor(true, trials)
            }

      //    helper method for subtyping a pair of ObjectTypes
      //
      //    Param trials - Holds pairs of previously subtype-checked ObjectTypes
      //            Prevents infinite subtype checking of self-referential types
            //Param other' - The ObjectType that self is checked against
            method isSubtypeHelper(trials:List⟦TypePair⟧, other':ObjectType)
                                                     → Answer {
                def selfOtherPair : TypePair = typePair(self, other')

                if (debug) then {
                   io.error.write "\n841: checking suptyping for {self} and {other'}"
                }

                //if trials already contains selfOtherPair, we can assume
                //self <: other.  Check other trivial cases of subtyping
                if ((self == other') || {trials.contains(selfOtherPair)}
                        || {other'.isDynamic} || {other' == doneType}) then{
                    return answerConstructor(true, trials)
                } else {
                    trials.add(selfOtherPair)
                }

                // Handle simple case where other is collection of methods
                if (other'.isMeths) then {
                    if (debug) then {io.error.write "\n816: both are meths"}
                    def oans = isSimpleSubtypeOf(trials, other')
                    if (debug) then {io.error.write "\n818: {other'} ans is {oans}"}
                    return oans
                }

                // TODO: should this be an error? Probably not.
                if (other'.isTypeVble) then {
                   return answerConstructor(false,trials)
                }

                // case where other is a named type
                if (other'.isId) then {
                   if (debug) then {
                      io.error.write "\n825 in id case for {other'}"
                   }
                   def idType: ObjectType = scope.types.find(other'.id)
                        butIfMissing {ScopingError.raise("Failed to find {other'.id}")}
                   return isSubtypeHelper (trials, idType)
                }

                // case where other is built from & or |
                if (other'.isOp) then {
                    def left: Answer = self.isSubtypeHelper(trials,other'.left)
                    def right: Answer = self.isSubtypeHelper(trials,other'.right)
                    if (debug) then {
                        io.error.write
                            "\n832: ansLeft: {left}, ansRight:{right}"
                        io.error.write
                            "\n833: for {other'.left} and {other'.right}"
                    }
                    def helperResult: Answer = if (other'.op =="|") then {
                         answerConstructor(left.ans || right.ans,trials)
                    } else { // & node
                         answerConstructor(left.ans && right.ans,trials)
                    }
                    return helperResult
                }

                // Should have covered all cases by now!
                if (debug) then { 
                   io.error.write "Oops! Missed a case for {self} and {other'}"
                   io.error.write "other'.isMeths = {other'.isMeths}"
                }
            }


            // Restrict types for gradual typing
            // Don't worry about this for now
            method restriction(other : ObjectType) → ObjectType {
                if (other.isDynamic) then { return dynamic}
                def restrictTypes:Set⟦MethodType⟧ = emptySet
                // Restrict matching methods
                for(methods) doWithContinue { meth, continue →
                    // Forget restricting if it is a type
                    if (asString.substringFrom (1) to (7) != "Pattern") then {
                        for(other.methods) do { meth' →
                            if (meth.name == meth'.name) then {
                                restrictTypes.add(meth.restriction(meth'))
                                continue.apply
                            }
                        }
                    }
                    restrictTypes.add(meth)
                }
                return object {
                    inherit anObjectType.fromMethods(restrictTypes)

                    method asString → String is override {
                        "{outer}|{other}"
                    }
                }
            }

            // Consistent-subtyping:
            // If self restrict other is a subtype of other restrict self.
            method isConsistentSubtypeOf(other : ObjectType) → Boolean {
                return self.isSubtypeOf(other.resolve)

                //TODO: Fix restriction() so that it handles variant types
                //def selfRestType = self.restriction(other)
                //def otherRestType = other.restriction(self)
                //io.error.write "self's restricted type is {selfRestType}"
                //io.error.write "other's restricted type is {otherRestType}"

                //return selfRestType.isSubtypeOf(otherRestType)  //  FIX!!!
                //true
            }

            // TODO: BROKEN! fix to handle same method name in both halves?
            // Can be conservative and report error if types not identical
            // or use m( A | A') -> B & B'
            method & (other': ObjectType) -> ObjectType {
                if (debug) then {
                    io.error.write "\n907 checking {other'.isMeths}"
                }
                if (other'.isMeths) then {
                    def newMeths: Set[[MethodType]] =
                          noDupMethods(self.methods, other'.methods)
                    if (debug) then {
                        io.error.write "\n910 newMeths {newMeths}"
                    }
                    fromMethods(newMeths)
                } else {
                    anObjectType.makeWithOp("&",self,other')
                }
            }

            // return list of methods of & of two collections of
            // method types fst and snd
            // TODO: if duplicate, just take whichever is smallest.  
            // Need to fix to take glb of two!!!
            method noDupMethods(fst: Set[[MethodType]],
                                snd: Set[[MethodType]]) -> Set[[MethodType]] {
               if (debug) then {
                    io.error.write "\n923 NoDup fst:{fst}, and \nsnd:{snd}"
               }
               // optimize by throwing away methods in base
               def modFst: Set[[MethodType]] = fst.copy.removeAll(base.methods)
               def modSnd: Set[[MethodType]] = snd.copy.removeAll(base.methods)
               def newMeths: Set[[MethodType]] = fst.copy
               for (modSnd) do {smt: MethodType ->
                   for (modFst) do {fmt: MethodType ->
                       if (fmt.nameString == smt.nameString) then {
                           if (debug) then {
                              io.error.write "\n duplicate methods {fmt.nameString}"
                           }
                           if (smt.isSpecialisationOf(emptyList,fmt).ans) then {
                               newMeths.remove(fmt)
                               newMeths.add(smt)
                           } elseif {!fmt.isSpecialisationOf(emptyList,smt).ans} then {
                               TypeError.raise "need to find glb of types of {fmt.nameString}"
                                    with (smt)
                           }
                       } else {
                           newMeths.add(smt)
                       }
                   }
               }
               if (debug) then {
                    io.error.write "\n956: noDupMethods gives {newMeths}"
               }
               newMeths
            }

            // update type of all methods using replacement for generic types.
            method updateTypeWith(replacements:Dictionary[[String,ObjectType]])
                        -> ObjectType {
                if (debug) then {
                   io.error.write "\n966: update methods"
                }
                def newMeths: List[[MethodType]] = emptyList[[MethodType]]
                for (methods) do {m: MethodType ->
                    newMeths.add(m.updateTypeWith(replacements))
                    if (debug) then {
                        io.error.write "\n977: new version: {newMeths.at (newMeths.size)}"
                    }
                }
                fromMethods(newMeths)
            }

            // String representation of type
            method asString → String is override {
                if (methods.size == base.methods.size) then {
                    return "Object"
                }
                var out: String := "\{ "
                for(methods) do { mtype: MethodType →
                    if (base.methods.contains(mtype).not) then {
                        out := "{out}\n    {mtype}; "
                    }
                }
                return "{out}\n  \}"
            }
        }
    }

    // Create an ObjectType from left, right using | or &
    class makeWithOp(op': String, left': ObjectType, right': ObjectType) ->
                                                    ObjectType {
        inherit superObjectType
        if (debug) then {
           io.error.write("\n942: left':{left'} isMeths: {left'.isMeths}")
           io.error.write("right':{right'} isMeths: {right'.isMeths}")
        }
        method op -> String {op'}
        method left -> ObjectType {left'}
        method right -> ObjectType {right'}
        method isOp -> Boolean {true}
        method methods -> Set[[MethodType]] {
            match(op)
                case { "&" ->
                    if {left.isMeths} then {
                        def newMeths: Set[[MethodType]] = left.methods
                        if (debug) then {
                            io.error.write "\n945 newMeths: {newMeths}"
                        }
                        newMeths
                    } elseif {right.isMeths} then {
                        def newMeths: Set[[MethodType]] = right.methods
                        if (debug) then {
                            io.error.write "\n945 newMeths: {newMeths}"
                        }
                        newMeths
                    } else {
                        emptySet
                    }
            }
                case { "|" ->
                    // TODO Handle | operation
                    emptySet
            }
        }

        method methList -> List[[Set[[MethodType]]]] {
            match(op)
                case { "|" ->
                    combineMethList(left.methList, right.methList)
            }
                case { "&" ->
                    def newMethList = emptyList[[Set[[MethodType]]]]
                    def leftMethList = left.methList
                    def rightMethList = right.methList
                    for (leftMethList) do {leftMethSet ->
                        for(rightMethList) do {rightMethSet ->
                            newMethList.add(leftMethSet ++ rightMethSet)
                        }
                    }
                    newMethList
            }
        }

        // TODO: restrict to common types
        method restriction (other: ObjectType) -> ObjectType {
            self
        }

        // conservative version of subtype where A & B <: C iff A <: C or B <: C
        // TODO: Fix this to be correct!
        method isSubtypeOf(other: ObjectType) -> Boolean {
            if (op == "|") then {
                left.isSubtypeOf(other) && right.isSubtypeOf(other)
            } else {
                left.isSubtypeOf(other) || right.isSubtypeOf(other)
            }
        }

        // TODO: Fix if do gradual typing
        method isConsistentSubtypeOf(other: ObjectType) -> Boolean {
            isSubtypeOf(other)
        }

        // Replace type parameters in left and right pieces
        // TODO: Not sure if this is ever used
        method replaceTypeParams(tparams: List[[String]]) -> ObjectType {
            def newLeft: ObjectType = left.replaceTypeParams(tparams)
            def newRight: ObjectType = right.replaceTypeParams(tparams)
            makeWithOp(op,newLeft,newRight)
        }

        // print type nicelY
        method asString -> String {
            left.asString ++ "\n{op}\n" ++ right.asString
        }

        // Takes a mapping of generic-to-ObjectType and returns a copy of self
        // with all of the generics replaced with their corresponding ObjectType
        method updateTypeWith(replacements:Dictionary[[String,ObjectType]])
                        -> ObjectType {
            if (debug) then {
               io.error.write "\n1047: update methods"
            }
            makeWithOp(op', left.updateTypeWith(replacements),
                            right.updateTypeWith(replacements))
        }

        // Return method type with matching nameString or return noSuchMethod.
        method getMethod(name : String) → MethodType | noSuchMethod {
            for(methList) do { methSet →
                for(methSet) do { meth ->
                    if (meth.nameString == name) then {
                        return meth
                    }
                }
            }
            return noSuchMethod
        }
    }

    //NOT DONE YET!Version of ObjectType that allows for lazy-implementation of type
    // checking
    //Holds the AstNode that can be resolved to the real ObjectType
    // DO WE NEED THIS?
    class definedByNode (node': AstNode) with (typeParams: List[[String]]) -> ObjectTypeFromMeths{
        inherit superObjectType

        var node : AstNode := node'

        if (debug) then {
               io.error.write "\n1065: Using definedByNode to create {self}"
        }
        // ObjectTypes that are defined by an opNode can store a TypeOp with
        // relevant information for type checking

        var storedOp : TypeOp

        // Return methods of type
        method methods -> Set⟦MethodType⟧ {
            resolve.methods
        }

        // Is this object type built from a collection of methods?
        method isMeths -> Boolean {true}

        // Return method type with matching nameString or return noSuchMethod.
        method getMethod (name : String) → MethodType | noSuchMethod {
            resolve.getMethod(name)
        }

        //Process the AstNode to get its ObjectType
        method resolve -> ObjectType {
            fromDType (node') with (typeParams)
        }

        //an ObjectType is unresolved if it has yet to collect its methods and,
        //optionally, variant types
        def isResolved : Boolean is public = false

        // Is this type built using & or |?
        method isOp → Boolean{
            ((false != node) && {node'.kind == "op"})
        }

        // Does this type represent the dynamic or unknown type
        def isDynamic : Boolean is public = false

        //The asString of a definedByNode contains the information needed to
        //determine equivalency
        method == (other:ObjectType) → Boolean {
            asString == other.asString
        }

        // Determines if self is a subtype of other
        method isSubtypeOf (other : ObjectType) -> Boolean {
            isSubtypeHelper (emptyList⟦TypePair⟧, other).ans
        }

        method isSubtypeHelper (trials:List⟦TypePair⟧, other:ObjectType) → Answer {
            if (debug) then {
                io.error.write("\n607: entered isSubtypeHelper, " ++
                                  "subtyping {self} with {other}")
            }

            //if self is defined by an opNode, then we recursively call
            //isSubtypeHelper on its left and right
            if (isOp) then {
                def left: Answer = getOpNode.left.isSubtypeHelper(trials, other)

                if (getOpNode.op == "&") then {
                    //Check left's result to see if right needs to be evaluated
                    if (left.ans) then {
                        return left
                    }
                    def right : Answer = getOpNode.right.isSubtypeHelper(
                                                                  left.trials, other)
                    return right
                } elseif {getOpNode.op == "|"} then {
                    if (left.ans.not) then {
                        return left
                    }
                    def right : Answer = getOpNode.right.isSubtypeHelper(
                                                                  trials, other)
                    return right
                }
            }
            resolve.isSubtypeHelper(trials, other)
        }

        method restriction (other : ObjectType) → ObjectType {
            resolve.restriction(other.resolve)
        }

        method isConsistentSubtypeOf (other : ObjectType) → Boolean{
            //Call isSubTypeOf
            resolve.isConsistentSubtypeOf(other.resolve)
        }

        method getVariantTypes → List⟦ObjectType⟧ {
            resolve.getVariantTypes
        }

        method setVariantTypes(newVariantTypes:List⟦ObjectType⟧) → Done {
            resolve.setVariantTypes(newVariantTypes)
        }

        method getNode → AstNode {
            node
        }

        method setNode(nd: AstNode) → Done {
            node := nd
            //io.error.write "\n771 node is {node}"
            if ((false != nd) && {nd.kind == "op"}) then {
                // Both used to be definedByNode
                def left : ObjectType = fromDType (nd.left) with (typeParams)
                def right : ObjectType = fromDType(nd.right) with (typeParams)
                setOpNode(typeOp(nd.value, left, right))
            }
        }
        setNode(node')

        method getOpNode → TypeOp {
            storedOp
        }

        method setOpNode (op : TypeOp) → Done {
            storedOp := op
        }

        method | (other : ObjectType) → ObjectType {
            resolve | other
        }

        method & (other : ObjectType) → ObjectType {
            resolve & other
        }

        method asString → String is override {
            asStringHelper(node)
        }
        method asStringHelper (nd : AstNode) → String is confidential {
            match(nd) case { (false) →
                "Unknown"
            } case { typeLiteral : share.TypeLiteral →
                var printOut : String := "TypeLiteral::\{"
                for (typeLiteral.methods) do { meth : AstNode →
                    printOut := "{printOut}\n    {aMethodType.fromNode(meth)}"
                }
                "{printOut}\n  \}"
            } case { op : share.Operator →
                "{asStringHelper(op.left)} {op.value} {asStringHelper(op.right)}"
            } case { ident : share.Identifier →
                "{ident.value}{typeParamsToString(ident.generics)}"
            } case { generic : share.Generic →
                "{generic.nameString}{typeParamsToString(generic.args)}"
            } case { member : share.Member →
                "{asStringHelper(member.receiver)}.{member.value}" ++
                                        "{typeParamsToString(member.generics)}"
            } case { _ →
                ProgrammingError.raise("No case in method 'asString' of the" ++
                                          "class definedByNode for node of " ++
                                          "kind {nd.kind}") with(nd)
            }
        }
        method typeParamsToString (args : List⟦AstNode⟧ | false) → String {
            var printOut : String := ""
            if (false ≠ args) then {
                printOut := "{printOut}⟦"
                for(args) do { arg : AstNode →
                    printOut := "{printOut}{asStringHelper(arg)},"
                }
                printOut := printOut.substringFrom(1) to (printOut.size - 1)
                printOut := "{printOut}⟧"
            }
            printOut
        }

        method updateTypeWith(replacements:Dictionary[[String,ObjectType]])
                                -> ObjectType {
            if (debug) then {
               io.error.write "\n1240: update methods in definedByNode"
            }
            def newMeths: List[[MethodType]] = emptyList[[MethodType]]
            for (methods) do {m: MethodType ->
                newMeths.add(m.updateTypeWith(replacements))
                if (debug) then {
                   io.error.write "\n1244: new version: {newMeths.at (newMeths.size)}"
                }
            }
            fromMethods(newMeths)
        }
    }

    // Deprecated
//    method fromDType(dtype : AstNode) → ObjectType {
//            io.error.write "\n1374: fromDType with {dtype}"
//            ProgrammingError.raise("Wrong version of fromDType, line 1375 of ObjectTypeModule")
//    }

    //takes an AstNode and returns its corresponding ObjectType
    method fromDType(dtype : AstNode) with (typeParams: List[[String]])
                                → ObjectType {
        def debug2 = true
        if (debug2) then {
           io.error.write "\n1245: starting fromDType with {dtype} and type params: {typeParams}"
        }

        // the type value corresponding to dtype
        var returnValue: ObjectType
        match(dtype) case { (false) →  // no type provided, so make dynamic
            returnValue := dynamic

        } case { typeDec : share.TypeDeclaration →
            ProgrammingError.raise("Types cannot be declared inside other " ++
                                                            "types or objects")

        } case { typeLiteral : share.TypeLiteral →
            if (debug2) then {
               io.error.write "\n1255: case TypeLiteral: {typeLiteral}"
            }
            def meths : Set⟦MethodType⟧ = emptySet
            //collect MethodTypes
            for(typeLiteral.methods) do { mType : AstNode →
                if (debug2) then {
                   io.error.write "\n1259 about to add {mType.toGrace(0)}"
                }
                def convertedMeth:MethodType = aMethodType.fromNode(mType) with (typeParams)
                if (debug2) then {
                    io.error.write "\n1445 converted meth: {convertedMeth}"
                }
                meths.add(convertedMeth)
                if (debug2) then {
                   io.error.write "\n1261 finished adding"
                }
            }
            returnValue := anObjectType.fromMethods(meths)

        } case { op: share.Operator →
            if (debug2) then {
               io.error.write "\n1264: case operator {op}"
            }
            // Operator takes care of type expressions: Ex. A & B, A | C
            // What type of operator (& or |)?
            var opValue: String := op.value

            // Left side of operator
            var left: AstNode := op.left
            var leftType: ObjectType := fromDType(left) with (typeParams)
            if (debug2) then {io.error.write "\n1153: leftType is {leftType}"}
            // Right side of operator
            var right: AstNode := op.right
            var rightType: ObjectType := fromDType(right) with (typeParams)

            match(opValue) case { "&" →
              if (debug2) then {
                io.error.write ("\n1158: {left} has type {leftType}, right has:"
                    ++ " {rightType}")
              }
              returnValue := leftType & rightType
              if (debug2) then {
                 io.error.write"\n1206:returnValue for\n {left} &\n {right} is\n {returnValue}"
              }
            } case { "|" →
              returnValue := leftType | rightType
            } case { _ →
              ProgrammingError.raise("Expected '&' or '|', got {opValue}")
                                                                        with(op)
            }

        } case { ident : share.Identifier →
            if (debug2) then {
               io.error.write "\n1292: case {ident}"
            }
            if (typeParams.contains(ident.value)) then {
                if (debug2) then {
                   io.error.write "\n1269: Creating typeVble for {ident}"
                }
                returnValue := typeVble(ident.value)
            } else {
                //look for identifier in the scope.
                if (debug2) then {
                   io.error.write "\n1320: not found {ident.value} in parameters: {typeParams}"
                }
                returnValue := fromIdentifier(ident) with (typeParams)
            }
            if (debug2) then {
               io.error.write "\n1276: returnValue: {returnValue}"
            }

        } case { generic : share.Generic →
            if (debug2) then {
               io.error.write "\n1303: case generic {generic}"
            }
            //get the objecttype from instantiating the generic type and
            //add it to the types scope.
            returnValue := fromGeneric(generic)

        } case { member : share.Member →
            if (debug2) then {
               io.error.write "\n1309: case member {member}"
            }
            //name of the receiver
            var recName : String := member.receiver.toGrace(0)
            var memberCall : String := "{recName}.{member.value}"

            //check if the receiver consists of multiple calls
            // Here for accessing types from imports
            def period : Number = recName.indexOf(".")
            if (period > 0) then {
                //extract the first receiver in the member call
                def firstRec: String = recName.substringFrom(1) to (period - 1)

                //all members processed here are references to types, so we can
                //ignore these receivers since types are always at the least
                //recent level of the scope.
                if ((firstRec == "module()object") || (firstRec == "self")
                                            || (firstRec == "prelude")) then {
                    recName := recName.substringFrom(period + 1)
                    memberCall := "{recName}.{member.value}"
                }
            } else {
                if ((recName == "module()object") || (recName == "self")
                                            || (recName == "prelude")) then {
                    memberCall := "{member.value}"
                }
            }

            //checks if the member is generic
            if(member.generics ≠ false) then {
                //recursively process the member repackaged as a genericNode
                returnValue := fromDType(ast.genericNode.new(ast.identifierNode.new(
                                          memberCall, false), member.generics))
                                            with (typeParams)
            } else {
                returnValue := scope.types.find(memberCall) butIfMissing {
                              ScopingError.raise("Failed to find {memberCall}")}
            }

        } case { _ →
            ProgrammingError.raise "No case for node of kind {dtype.kind}"
                                                                    with(dtype)
        }
        if (debug2) then {
           io.error.write "\n1351: returning fromDType with {returnValue}"
        }

        returnValue
    }

    // Currently only accept ast.genericNodes and will return the name of the
    // type if it was stored in the types scope
    // Returns name of generic with args separated by "$"
    method typesScopeName (generic : share.Generic) → String is confidential {
        var genName : String := generic.nameString
        for (generic.args) do { arg : AstNode →
            def argName : String = if (arg.kind == "typeliteral") then {
                fromDType(arg)with(emptyList).asString
            } else {
                arg.nameString
            }
            genName := "{genName}${argName}"
        }
        genName
    }

    // Create an ObjectType from a GenericNode; the type used in the GenericNode
    // must have already been declared and put into the generics scope. The types
    // scope will also contain the returned ObjectType after this method is done.
    method fromGeneric(node : share.Generic) → ObjectType {
        //Search the ObjectType types scope to see if this type was already
        //processed. If not, find the corresponding GenericType in the generic
        //types scope and initialize its type parameters.
        def genName : String = typesScopeName(node)
        scope.types.find (genName) butIfMissing {

            //Raise error if the type used does not exist in the generic scope
            def genType : GenericType = scope.generics.find(node.nameString)
                                                                  butIfMissing {
                ProgrammingError.raise("Attempting to use an undefined " ++
                                  "generic type {node.nameString}.") with (node)
            }

            //Replace the typeParameters with replacementTypes
            if (debug) then {
               io.error.write "\n1414: node.args: {node.args}"
            }
            def argsAsTypes: List[[ObjectType]] = emptyList[[ObjectType]]
            for (node.args) do {arg ->
                argsAsTypes.add(fromDType(arg) with (emptyList[[String]]))
            }
            if (debug) then {
               io.error.write "\n1419: argsAsTypes: {argsAsTypes}"
            }
            def instantiated : ObjectType = genType.apply(argsAsTypes)

            //Update the types scope and return
            if (debug) then {
                io.error.write("\n1411 adding to {genName} with {instantiated}")
            }
            scope.types.addToGlobalAt (genName) put (instantiated)
            instantiated
        }
    }

    // ObjectType corresponding to the identifier in the scope. If not
    // already there, raise an exception.   
    class fromIdentifier(ident : share.Identifier) with (typeParams) → ObjectType { 
        inherit superObjectType

        method id  -> String { return ident.value }
        method isId -> Boolean { true }

        method ans -> ObjectType {
            if (debug) then {
                io.error.write("\n1249 fromIdentifier - looking for {ident.value}"++
                                            " inside {scope.types}")
            }

            // Check if identifier is generic, and if so, turn it into a
            // generic node and recurse so the generic case can handle it.
            if(ident.generics ≠ false) then {
                if (debug) then {
                   io.error.write "\n1429: generic node {ident}"
                }
                def genericAns: ObjectType = fromDType(ast.genericNode.new(ident, ident.generics))
                    with (typeParams)
                if (debug) then {
                   io.error.write "\n1433: generic node reduced to {ans}"
                }
                genericAns
            } else {
                scope.types.find(ident.value) butIfMissing {
                    StaticTypingError.raise("Type " + ident.value + "is not defined")
                }
            }            
        }
  
        method isSubtypeOf (other: ObjectType) -> Boolean { ans.isSubtypeOf(other) }  
        method == (other: ObjectType) -> Boolean { ans.asString == other.asString }
        method asString -> String { ident.value }
        method methods -> Set⟦MethodType⟧ is public  { ans.methods }

        method methList -> List[[Set[[MethodType]]]] {
            return ans.methList
        }

        // Return method type with matching nameString or return noSuchMethod.
        method getMethod(name : String) → MethodType | noSuchMethod {
            for(methList) do { methSet →
                for(methSet) do { meth ->
                    if (meth.nameString == name) then {
                        return meth
                    }
                }
            }
            return noSuchMethod
        }
    }

    // ObjectType corresponding to a type variable (e.g. from generic type)
    class typeVble(name': String) -> ObjectType {
        inherit superObjectType
        method isTypeVariable -> Boolean {true}
        method name -> String {name'}
        method isSubtypeOf(other': ObjectType) -> Boolean {
            return (self == other') || {other'.isDynamic} || {other' == doneType} || (other' == base)
        }
        method updateTypeWith(replacements: Dictionary[[String,ObjectType]]) -> ObjectType {
            if (debug) then {
               io.error.write "\n1411"
            }
            if(!replacements.containsKey(name)) then {
                ProgrammingError.raise("the type variable {name} " ++
                            "was not found with matching type") 
            }
            if (debug) then {
               io.error.write "\n1409: Substituting {name} -> {replacements.at (name)} in typeVble"
            }
            replacements.at (name)
        }

        method asString -> String {
            "typeVariable: {name'}"
        }
    }

    // ************ Methods to create specific kinds of object types *****************

    // Create a type corresponding to Dynamic
    method dynamic → ObjectType {
        object {
            inherit superObjectType
            method resolve -> ObjectType { self }

            def isResolved : Boolean is public = true

            def isOp : Boolean is public = false

            def isDynamic : Boolean is public = true

            method ==(other:ObjectType) → Boolean{self.isMe(other)}

            method isSubtypeOf(_ : ObjectType) → Boolean { true }

            method isSubtypeHelper (_ : List⟦TypePair⟧, _ : ObjectType) → Answer{
                answerConstructor(true , emptyList⟦TypePair⟧)
            }

            method restriction(_ : ObjectType) → dynamic { dynamic }

            method isConsistentSubtypeOf(_ : ObjectType) → Boolean { true }

            method getVariantTypes → List⟦ObjectType⟧ { emptyList⟦ObjectType⟧ }

            method setVariantTypes(newVariantTypes:List⟦ObjectType⟧) → Done { }

            method getOpNode → TypeOp { typeOp(" ", dynamic, dynamic) }

            method setOpNode (op : TypeOp) → Done { }

            method |(_ : ObjectType) → ObjectType { dynamic }

            method &(_ : ObjectType) → ObjectType { dynamic }

            def asString : String is public, override = "Unknown"
        }
    }

    // Type corresponding to bottom element, i.e., has all methods, so subtype of all
    method bottom → ObjectType {
        object {
            inherit dynamic
            def isDynamic : Boolean is public, override = false
            def asString : String is public, override = "Bottom"
            method |(other : ObjectType) → ObjectType { other }

            method &(other : ObjectType) → ObjectType { self }
        }
    }

    // type of a block with params and return type rType
    method blockTaking(params : List⟦Parameter⟧)
            returning(rType : ObjectType) → ObjectType {
        def signature = list[aMixPartWithName("apply") parameters(params)]
        def meths: Set⟦MethodType⟧ = emptySet
        meths.add(aMethodType.signature(signature) returnType(rType))
        fromMethods(meths) withName("Block")
    }

    // type of block without prarameters returning value of type rType
    method blockReturning(rType : ObjectType) → ObjectType {
        blockTaking(list[]) returning(rType)
    }

    // add method to oType.  Only use this variant if method is parameterless
    method addTo (oType : ObjectType) name (name' : String)
                          returns (rType : ObjectType) → Done is confidential {
        def signature = list[aMixPartWithName (name') parameters (list[])]
        oType.methods.add (aMethodType.signature (signature) returnType (rType))
    }

    // add method to oType.  Only use this variant if has exactly one part with one or more
    // parameters
    method addTo (oType : ObjectType) name (name' : String)
            params (ptypes : List⟦ObjectType⟧) returns (rType : ObjectType)
            → Done is confidential {
        def parameters = list[]
        for(ptypes) do { ptype →
            parameters.add(aParam.ofType(ptype))
        }

        def signature = list[aMixPartWithName(name') parameters (parameters)]

        oType.methods.add (aMethodType.signature(signature) returnType (rType))
    }

    // add method to oType.  Only use this variant for method with one part
    // and exactly one parameter
    method addTo (oType : ObjectType) name (name' : String)
            param (ptype : ObjectType) returns (rType : ObjectType)
            → Done is confidential {
        def parameters = list[aParam.ofType(ptype)]

        def signature = list[aMixPartWithName(name') parameters(parameters)]

        oType.methods.add (aMethodType.signature(signature) returnType(rType))
    }

    // add method to oType.  Only use this variant if more than one part.
    method addTo (oType: ObjectType) names (name: List⟦String⟧)
         parts(p: List⟦List⟦ObjectType⟧ ⟧) returns (rType: ObjectType) → Done
                                    is confidential{
         def parts: List⟦List⟦Param⟧⟧ = list[]
         var nameString: String := ""
         for (p) do { part: List⟦ObjectType⟧ →
             def parameters: List⟦Param⟧ = list[]
             for (part) do {ptype: ObjectType →
                 parameters.add(aParam.ofType(ptype))
             }
             parts.add(parameters)
         }

         def signature: List⟦MixPart⟧ = list[]
         for (1 .. name.size) do {i →
             signature.add(aMixPartWithName(name.at (i)) parameters(parts.at (i)))
         }
         oType.methods.add(aMethodType.signature (signature) returnType (rType))
    }

    // add methods of that to this
    method extend(this : ObjectType) with (that: ObjectType)
            → Done is confidential {
        this.methods.addAll(that.methods)
    }

    // TODO: Make sure get everything from standardGrace and
    // StandardPrelude
    var base : ObjectType is readable := dynamic

    // type of command
    def doneType : ObjectType is public = fromMethods(sg.emptySet)
                                                                withName("Done")
    base := fromMethods(sg.emptySet) withName("Object")

    // Used for type-checking imports; please update when additional types are
    // added
    def preludeTypes: Set⟦String⟧ is public = ["Done", "Pattern", "Iterator",
                                  "Boolean", "Number", "String", "List", "Set",
                                  "Sequence", "Dictionary", "Point", "Binding",
                                  "Collection", "Enumerable", "Range", "Object"]

    // Create object types for built-in types (includes all prelude types)
    // This part just creates them, no methods yet
    def pattern : ObjectType is public = fromMethods(sg.emptySet)
                                                  withName("Pattern")
    def iterator : ObjectType is public = fromMethods(sg.emptySet)
                                                  withName("Iterator")
    def boolean : ObjectType is public = fromMethods(sg.emptySet)
                                                  withName("Boolean")
    def number : ObjectType is public = fromMethods(sg.emptySet)
                                                  withName("Number")
    def string : ObjectType is public = fromMethods(sg.emptySet)
                                                  withName("String")
    def listTp : ObjectType is public = fromMethods(sg.emptySet)
                                                  withName("List")
    def set : ObjectType is public = fromMethods(sg.emptySet)
                                                  withName("Set")
    def sequence : ObjectType is public = fromMethods(sg.emptySet)
                                                  withName("Sequence")
    def dictionary : ObjectType is public = fromMethods(sg.emptySet)
                                                  withName("Dictionary")
    def point : ObjectType is public = fromMethods(sg.emptySet)
                                                  withName("Point")
    def binding : ObjectType is public = fromMethods(sg.emptySet)
                                                  withName("Binding")
    def collectionBase : ObjectType is public = fromMethods(sg.emptySet)
                                                  withName("CollectionBase")
    def enumerable : ObjectType is public = fromMethods(sg.emptySet)
                                                  withName("Enumerable")
    def rangeTp : ObjectType is public = fromMethods(sg.emptySet)
                                                  withName("Range")
    def preludeType: ObjectType is public = fromMethods(sg.emptySet)
                                                withName("PreludeType")
    def boolBlock: ObjectType is public = fromMethods(sg.emptySet)
                                                  withName("BoolBlock")
    def doneBlock: ObjectType is public = fromMethods(sg.emptySet)
                                                  withName("DoneBlock")
    def dynamicDoneBlock: ObjectType is public = fromMethods(sg.emptySet)
                                                  withName("DynamicDoneBlock")
    def exceptionKind: ObjectType is public = fromMethods(sg.emptySet)
                                                  withName("ExceptionKind")

    // Now add methods to each type

    // Done is very restricted!
    addTo (doneType) name ("asDebugString") returns (string)
    addTo (doneType) name ("asString") returns (string)

    // Add methods contained in all types
    // TODO: Not sure if "≠" belongs in or not!
    addTo (base) name ("≠") param (base) returns (boolean)
    
//    addTo (base) name ("hash") returns (number)
    addTo (base) name ("asString") returns (string)
    
//    addTo (base) name ("basicAsString") returns (string)
    addTo (base) name ("asDebugString") returns (string)
    
    addTo (base) name ("::") returns (binding)

//    IS THIS NEEDED?
//    addTo (base) name ("list") param (collection) returns (listTp)

    // Add methods available on patterns (which include types)
    extend (pattern) with (base)
    addTo (pattern) name ("match") param (base) returns (dynamic)
    addTo (pattern) name ("|") param (pattern) returns (pattern)
    addTo (pattern) name ("&") param (pattern) returns (pattern)

    extend (iterator) with (base)
    addTo (iterator) name ("hasNext") returns (boolean)
    addTo (iterator) name ("next") returns (dynamic)

    def shortCircuit: ObjectType =
        blockTaking(list[aParam.ofType(blockReturning(dynamic))])returning(base)

    extend (boolean) with(base)
    addTo (boolean) name ("&&") param (boolean) returns (boolean)
    addTo (boolean) name ("||") param (boolean) returns (boolean)
//  TODO: Fix && and || so they take boolean block as second arg.
//    Why doesn't below work??
//    addTo (boolean) name("&&") param (boolean | boolBlock) returns (boolean)
//    addTo (boolean) name("||") param (boolean | boolBlock) returns (boolean)
    addTo (boolean) name ("prefix!") returns (boolean)
    addTo (boolean) name ("not") returns (boolean)
    addTo (boolean) name ("andAlso") param (shortCircuit) returns (dynamic)
    addTo (boolean) name ("orElse") param (shortCircuit) returns (dynamic)
    addTo (boolean) name ("==") param (base) returns (boolean)
    addTo (boolean) name ("≠") param (base) returns (boolean)

    // Add methods for numbers
    extend (number) with(base)
    addTo (number) name ("+") param (number) returns (number)
    addTo (number) name ("*") param (number) returns (number)
    addTo (number) name ("-") param (number) returns (number)
    addTo (number) name ("/") param (number) returns (number)
    addTo (number) name ("^") param (number) returns (number)
    addTo (number) name ("%") param (number) returns (number)
    addTo (number) name ("@") param (number) returns (point)
    addTo (number) name ("hashcode") returns (string)
    addTo (number) name ("++") param (base) returns (string)
    addTo (number) name ("<") param (number) returns (boolean)
    addTo (number) name (">") param (number) returns (boolean)
    addTo (number) name ("==") param (base) returns (boolean)
    addTo (number) name ("≠") param (base) returns (boolean)
    addTo (number) name ("<=") param (number) returns (boolean)
    addTo (number) name ("≤") param (number) returns (boolean)
    addTo (number) name (">=") param (number) returns (boolean)
    addTo (number) name ("≥") param (number) returns (boolean)
    addTo (number) name ("..") param (number) returns (listTp)
    addTo (number) name ("asInteger32") returns (number)
    addTo (number) name ("prefix-") returns (number)
    addTo (number) name ("inBase") param (number) returns (number)
    addTo (number) name ("truncated") returns (number)
    addTo (number) name ("rounded") returns (number)
    addTo (number) name ("prefix<") returns (pattern)
    addTo (number) name ("prefix>") returns (pattern)
    addTo (number) name ("abs") returns (number)

    // helper object types for string methods
    def ifAbsentBlock: ObjectType = blockTaking (list[]) returning (dynamic)
    def stringDoBlock: ObjectType = blockTaking (list[aParam.ofType(string)])
            returning(doneType)
    def stringKeysValuesDoBlock: ObjectType =
        blockTaking (list[aParam.ofType(number), aParam.ofType(string)])
           returning (doneType)

    // Add methods for strings
    extend (string) with(base)
    addTo (string) name ("*") param (number) returns (string)
    addTo (string) name ("&") param (pattern) returns (pattern)
    addTo (string) name ("++") param (doneType) returns (string)
    addTo (string) name ("==") param (base) returns (boolean)
    addTo (string) name ("≠") param (base) returns (boolean)
    addTo (string) name (">") param (string) returns (boolean)
    addTo (string) name (">=") param (string) returns (boolean)
    addTo (string) name ("<") param (string) returns (boolean)
    addTo (string) name ("<=") param (string) returns (boolean)
    addTo (string) name ("≤") param (string) returns (boolean)
    addTo (string) name ("≥") param (string) returns (boolean)
    addTo (string) name ("at") param (number) returns (string)
    addTo (string) name ("asLower") returns (string)
    addTo (string) name ("asNumber") returns (number)
    addTo (string) name ("asUpper") returns (string)
    addTo (string) name ("capitalized") returns (string)
    addTo (string) name ("compare") param (string) returns (boolean)
    addTo (string) name ("contains") param (string) returns (boolean)
    addTo (string) name ("encode") returns (string)
    addTo (string) name ("endsWith") param (string) returns (boolean)
    addTo (string) name ("indexOf") param (string) returns (number)
    addTo(string) names (list["indexOf","startingAt"])
           parts(list [ list[string], list[number] ]) returns (number)
   
//    addTo(string) names(["indexOf","startingAt","ifAbsent"])
//       parts([ [string], [number],[ifAbsentBlock] ]) returns (number | dynamic)
//    addTo(string) names(["indexOf","startingAt","ifAbsent"])
//       parts([ [string], [number],[ifAbsentBlock] ]) returns (number | dynamic)

    addTo (string) name ("lastIndexOf") param (string) returns (number)
    addTo (string) names (list["lastIndexOf","startingAt"])
        parts(list[ list[string], list[number] ]) returns (number)

//    addTo(string) names(["lastIndexOf","ifAbsent"])
        //parts([ [string], [ifAbsentBlock] ]) returns (number | dynamic)
//    addTo(string) names(["lastIndexOf","startingAt","ifAbsent"])
        //parts([ [string], [number],[ifAbsentBlock] ])returns (number | dynamic)

    addTo(string) name ("indices") returns (listTp)
    addTo(string) name ("isEmpty") returns (boolean)
    addTo(string) name ("iterator") returns (base)
    addTo(string) name ("lastIndexOf") param (string) returns (number)
    
//    addTo(string) name ("lastIndexOf()ifAbsent") params(string, ifAbsentBlock)
                                                    //returns (number | dynamic)
//    addTo(string) name ("lastIndexOf()startingAt ()ifAbsent")
                      //params(string, ifAbsentBlock) returns (number | dynamic)
      
    addTo(string) name ("ord") returns (number)
    addTo(string) names(list["replace","with"])
                      parts(list[ list[string], list[string] ]) returns (string)
    addTo(string) name ("size") returns (number)
    addTo(string) name ("startsWith") param (string) returns (boolean)
    addTo(string) name ("startsWithDigit") returns (boolean)
    addTo(string) name ("startsWithLetter") returns (boolean)
    addTo(string) name ("startsWithPeriod") returns (boolean)
    addTo(string) name ("startsWithSpace") returns (boolean)
    addTo(string) names (list["substringFrom","size"])
        parts (list[ list[number], list[number] ]) returns (string)
    addTo(string) names (list["substringFrom","to"])
        parts (list[ list[number], list[number] ]) returns (string)
    addTo(string) name ("_escape") returns (string)
    addTo(string) name ("trim") returns (string)
    addTo(string) name ("do") param (stringDoBlock) returns (doneType)
    addTo(string) name ("size") returns (number)
    addTo(string) name ("iter") returns (iterator)

    // Add methods for point type
    extend(point) with(base)
    addTo (point) name ("==") param (base) returns (boolean)
    addTo (point) name ("≠") param (base) returns (boolean)
    addTo(point) name ("x") returns (number)
    addTo(point) name ("y") returns (number)
    addTo(point) name ("distanceTo") param (point) returns (number)
    addTo(point) name ("+") param (point) returns (point)
    addTo(point) name ("-") param (point) returns (point)
    addTo(point) name ("*") param (point) returns (point)
    addTo(point) name ("/") param (point) returns (point)
    addTo(point) name ("length") returns (number)

    // add methods for exceptionKind
    extend (exceptionKind) with (pattern)
    addTo (exceptionKind) name ("raise") param (string) returns (bottom)
    addTo (exceptionKind) names (list["raise", "with"]) parts (list[ list[string],list[base] ]) returns (doneType)
    addTo (exceptionKind) name ("refine") param (string) returns (exceptionKind)
    addTo (exceptionKind) name ("name") returns (string)
    addTo (exceptionKind) name ("==") param (base) returns (boolean)
    addTo (exceptionKind) name ("≠") param (base) returns (boolean)

    // helper type for lists
    def fold: ObjectType =
              blockTaking(list[aParam.ofType(dynamic), aParam.ofType(dynamic)])
                      returning (dynamic)

    // add methods for list type
    extend (listTp) with (base)
    addTo (listTp) name ("at") param (number) returns (dynamic)
    addTo(listTp) names(list["at","put"])
                    parts(list[ list[number], list[dynamic] ]) returns (doneType)
    addTo(listTp) name ("push") param (dynamic) returns (doneType)
    addTo(listTp) name ("add") param (dynamic) returns (listTp)
    // need to add varparams(comment belongs to "addFirst")
    addTo(listTp) name ("addFirst") param (dynamic) returns (listTp)
    addTo(listTp) name ("addLast") param (dynamic) returns (listTp)
    addTo(listTp) name ("addAll") param (listTp) returns (listTp)
    addTo(listTp) name ("pop") returns (dynamic)
    addTo(listTp) name ("size") returns (number)
    addTo(listTp) name ("iter") returns (iterator)
    addTo(listTp) name ("iterator") returns (iterator)
    addTo(listTp) name ("contains") param (dynamic) returns (boolean)
    addTo(listTp) name ("indexOf") param (dynamic) returns (number)
    addTo(listTp) name ("indices") returns (listTp)
    addTo(listTp) name ("first") returns (dynamic)
    addTo(listTp) name ("last") returns (dynamic)
    addTo(listTp) name ("prepended") param (dynamic) returns (listTp)
    addTo(listTp) name ("++") param (listTp) returns (listTp)
    addTo(listTp) name ("reduce") params(list[dynamic, fold]) returns (dynamic)
    addTo(listTp) name ("reversed") returns (dynamic)
    addTo(listTp) name ("removeFirst") returns (dynamic)
    addTo(listTp) name ("removeLast") returns (dynamic)
    addTo(listTp) name ("removeAt") param (number) returns (dynamic)
    addTo(listTp) name ("remove") param (dynamic) returns (listTp)
    addTo(listTp) name ("removeAll") param (listTp) returns (listTp)
    addTo(listTp) name ("copy") returns (listTp)
    addTo(listTp) name ("sort") returns (listTp)
    addTo(listTp) name ("reverse") returns (listTp)
    addTo (listTp) name ("==") param (listTp) returns (boolean)

    // Add methods to binding type (key,value) pairs
    extend(binding) with(base)
    addTo(binding) name ("key") returns (dynamic)
    addTo(binding) name ("value") returns (dynamic)

    // Add methods to two kinds of boolean blocks
    addTo(boolBlock) name ("apply") returns (boolean)
    addTo(doneBlock) name ("apply") returns (doneType)

    // Add methods to preludeType
    // TODO: Eventually should be picked up automatically from prelude
    addTo (preludeType) name ("print") param (doneType) returns (doneType)
    addTo (preludeType) name ("Exception") returns (exceptionKind)
    addTo (preludeType) name ("ProgrammingError") returns (exceptionKind)
    addTo (preludeType) name ("EnvironmentException") returns (exceptionKind)
    addTo (preludeType) name ("ResourceException") returns (exceptionKind)
    addTo (preludeType) names(list["while", "do"])
              parts(list[list[boolBlock], list[doneBlock] ]) returns (doneType)
    addTo (preludeType) names(list["for", "do"])
              parts(list[list[listTp], list[dynamicDoneBlock] ]) returns (doneType)

    // Add all predefined types to types scope
    scope.types.at ("Unknown") put (dynamic)
    scope.types.at ("Done") put (doneType)
    scope.types.at ("Object") put (base)
    scope.types.at ("Pattern") put (pattern)
    scope.types.at ("Boolean") put (boolean)
    scope.types.at ("Number") put (number)
    scope.types.at ("String") put (string)
    scope.types.at ("List") put (listTp)
    scope.types.at ("Set") put (set)
    scope.types.at ("Sequence") put (sequence)
    scope.types.at ("Dictionary") put (dictionary)
    scope.types.at ("Point") put (point)
    scope.types.at ("Binding") put (binding)
    scope.types.at ("Range") put (rangeTp)
    scope.types.at ("Iterator") put (iterator)
//    scope.types.at ("Collection") put (collection)
    scope.types.at ("Enumerable") put (enumerable)
    scope.types.at ("ExceptionKind") put (exceptionKind)
    scope.types.at ("Exception") put (exceptionKind)
    scope.types.at ("ProgrammingError") put (exceptionKind)
    scope.types.at ("EnvironmentException") put (exceptionKind)
    scope.types.at ("ResourceException") put (exceptionKind)
    scope.types.at ("PreludeType") put (preludeType)

    // right hand side of def of Collection[[T]]
    //extend (collectionBase) with (base)
    //addTo (collectionBase) name ("isEmpty") returns (boolean)
    //addTo (collectionBase) name ("size") returns (number)
    //addTo (collectionBase) name ("first") returns (ast.identifierNode.new("T",false))
    //  //addTo (collectionBase) name ("first") returns (typeVar)
    //def collection: GenericType is public = aGenericType.fromName ("Collection")
    //     parameters (list["T"]) objectType (collectionBase)
    //io.error.write "\n1815 collection (generic) is {collection}"
    //def nList = collection.apply(list[ast.identifierNode.new("Number",false)])
    //io.error.write ("nList is {nList}")
    //scope.generics.at ("Collection") put (collection)

    //Since the type 'Object' can be overwritten by the programmer, '$Object$'
    //is a dummy type used for saving the type definition of the prelude
    //'Object'. This is used when checking specialisation of generic methods
    scope.types.at ("$Object$") put (base)

    // Add these type names to scope as variables and parameterless methods
    addVar("Unknown") ofType(pattern)
    addVar("Dynamic") ofType(pattern)
    addVar("Done") ofType(pattern)
    addVar("Object") ofType(pattern)
    addVar("Pattern") ofType(pattern)
    addVar("Boolean") ofType(pattern)
    addVar("Number") ofType(pattern)
    addVar("String") ofType(pattern)
    addVar("List") ofType(pattern)
    addVar("Set") ofType(pattern)
    addVar("Sequence") ofType(pattern)
    addVar("Dictionary") ofType(pattern)
    addVar("Point") ofType(pattern)
    addVar("Binding") ofType(pattern)
    addVar("Range") ofType(pattern)

    addVar("Iterator") ofType(pattern)
//    addVar("Collection") ofType(pattern)
    addVar("Enumerable") ofType(pattern)

    // Add predefined constants to variable scope
    addVar("done") ofType(self.doneType)
    addVar("true") ofType(boolean)
    addVar("false") ofType(boolean)
    addVar("prelude") ofType (preludeType)
}

//def collectionDec: Collection[[String]] = [
//      "type Collection⟦T⟧ = \{",
//      "   isEmpty -> Boolean",
//      "   size -> Number",
//      "   first -> T",
//      "   sizeIfUnknown(action: Function0⟦Number⟧) -> Number",
//      "   do(action: Function1⟦T,Unknown⟧) -> Done",
//      "   do(action:Function1⟦T, Unknown⟧) separatedBy(sep:Function0⟦Unknown⟧) -> Done",
//      "   map⟦R⟧(unaryFunction:Function1⟦T, R⟧) -> Collection⟦T⟧",
//      "   fold⟦R⟧(binaryFunction:Function2⟦R, T, R⟧) startingWith(initial:R) -> R",
//      "   filter(condition:Function1⟦T, Boolean⟧) -> Collection⟦T⟧",
//      "   ++(other: Collection⟦T⟧) -> Collection⟦T⟧",
//      "\}"]
//def tokens = lexer.lexLines(collectionDec)
//io.error.write "\n1858 tokens are "
//for(tokens) do {tok -> io.error.write(tok.asString)}
//def moduledec: ast.moduleNode = parser.parse(tokens)
//io.error.write "\n1858 parseTree is {moduledec.value.at (1)}"
//def collection: GenericType is public = aGenericType.fromTypeDec(moduledec.value.at (1))
////def typeVar: ObjectType is public = anObjectType.fromMethods(sg.emptySet)
////                                                  withName ("T")
////io.error.write "\n1815 collection is {anObjectType.collection}"
//for (collection.oType.methods) do {meth: MethodType ->
//        io.error.write "\n {meth}"
//}
//def nList = collection.apply(list[ast.identifierNode.new("Number",false)])
//io.error.write ("nList is {nList}")
//scope.generics.at ("Collection") put (collection)



// Adds name to variables and as parameterless method (really def, not var!)
method addVar (name : String) ofType(oType : ObjectType) → Done is confidential{
    scope.variables.at (name) put (oType)
    scope.methods.at (name) put (aMethodType.member(name) ofType(oType))
}

//********************* HELPER METHODS *************************
// For loop with continue.
method for(a) doWithContinue(bl) → Done is confidential {
    for(a) do { e →
        continue'(e, bl)
    }
}

method continue'(e, bl) → Done is confidential {
    bl.apply(e, { return })
}

//Takes a non-empty list of objectTypes and combines them into a variant type
method fromObjectTypeList(oList : List⟦ObjectType⟧) → ObjectType{
      if (oList.size == 0) then {
          TypeError.raise("\nTried to construct a variant type from" ++
              "an empty list of variant types")
      }
      var varType: ObjectType := oList.at (1)
      var index:Number := 2
      while {index <= oList.size} do {

        //Combine types that are subsets of one-another
        if (varType.isSubtypeOf (oList.at (index))) then {
            varType := oList.at (index)
        } elseif {oList.at (index).isSubtypeOf(varType).not} then {
            varType := varType | oList.at (index)
        }

        index := index +1
      }
      varType
}

//Creates a Dictionary mapping elements of keys to elements of vals
method makeDictionary⟦K,V⟧(keys: List⟦K⟧, vals: List⟦V⟧) → Dictionary⟦K,V⟧ {
    //Make sure keys and vals are of the same size
    if(keys.size ≠ vals.size) then {
        ProgrammingError.raise("Attempted to create a dictionary from two " ++
                                                      "lists of unequal size.")
    }
    //Populate the dictionary
    def dict : Dictionary⟦K,V⟧ = emptyDictionary⟦K,V⟧
    for (keys.indices) do { index : Number →
        dict.at (keys.at (index)) put (vals.at (index))
    }
    dict
}

// TODO: Combines two method lists and discards the clauses that are subtypes 
method combineMethList(leftMethList: List[[Set[[MethodType]]]], rightMethList: List[[Set[[MethodType]]]]) -> List[[Set[[MethodType]]]] {
    leftMethList ++ rightMethList
}                            