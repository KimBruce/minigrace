#pragma ExtendedLineups
#pragma noTypeChecks
dialect "none"
import "standardGrace" as sg

import "ast" as ast
import "xmodule" as xmodule
import "io" as io
import "ScopeModule" as sc
import "SharedTypes" as share

inherit sg.methods

type MethodType = share.MethodType
type MethodTypeFactory = share.MethodTypeFactory
type GenericType = share.GenericType
type GenericTypeFactory = share.GenericTypeFactory
type ObjectType = share.ObjectType
type ObjectTypeFactory = share.ObjectTypeFactory
type AstNode = share.AstNode
type Parameter = share.Parameter

def scope: share.Scope = sc.scope

// Scoping error declaration
def ScopingError: outer.ExceptionKind is public =
                                                TypeError.refine("ScopingError")

def debug : Boolean = true

var methodtypes := [ ]
// visitor to convert a type expression to a string
def typeVisitor: ast.AstVisitor = object {
    inherit ast.baseVisitor
    var literalCount := 1
    method visitTypeLiteral(lit) {
//        io.error.write"\n 25 visiting Type Literal"
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
        methodtypes.at(1)
    } else {
        dtype.value
    }
}


// -------------------------------------------------------------------
// Type declarations for type representations to use for type checking
// -------------------------------------------------------------------

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

class typePair(first':ObjectType, second':ObjectType)→ TypePair is confidential{
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

//Stores needed information used when type-checking a type defined by an opNode
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

// factory for creating method types from various inputs
def aMethodType: MethodTypeFactory is public = object {
    // create method type from list of mixparts and return type
    method signature (signature' : List⟦MixPart⟧)
                              returnType (retType' : ObjectType) → MethodType {
        object {
            //Public defs of MethodType
            var name : String is readable := ""
            var nameString : String is readable := ""
            def signature : List⟦MixPart⟧ is public = signature'
            def retType : ObjectType is public = retType'
            def typeParams : List⟦String⟧ is public = emptyList⟦String⟧

            //Initialize name, nameString, and show(for the method asString)
            var show : String := ""
            def fst: MixPart = signature.first
            if (fst.parameters.isEmpty) then {
                name := fst.name
                nameString := fst.name
                show := name
            } else {
                for (signature) do { part →
                    name := "{name}{part.name}()"
                    nameString := ("{nameString}{part.name}" ++
                                                    "({part.parameters.size})")
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

                name := name.substringFrom (1) to (name.size - 2)
            }
            show := "{show} → {retType}"

            method hash → Number is override {
                nameString.hash
            }

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
                return signature (restrictParts) returnType (retType)
            }

            // Determines if this method is a specialisation of the given one.
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

            // Initialize this method's type params with the replacementTypes
            // Returns a different MethodType with the correct types
            method apply(replacementTypes : List⟦AstNode⟧) → MethodType {
                if(replacementTypes.size ≠ typeParams.size) then {
                    TypeError.raise("Wrong number of type parameters given " ++
                                      "when instantiating generic method " ++
                                      "{nameString}. Attempted to replace " ++
                                      "{typeParams} with {replacementTypes}.")
                }
                //Create a mapping of GenericTypes-to-ObjectTypes
                def replacements : Dictionary⟦String, AstNode⟧ =
                                    makeDictionary(typeParams,replacementTypes)
                replaceGenericsWith(replacements)
            }

            // Takes a mapping of generic-to-ObjectType and returns a copy of
            // self with all of the generics replaced with their corresponding
            // ObjectType
            //
            // Note : Does not need 'replacements.size == typeParams.size'
            method replaceGenericsWith (replacements :
                                      Dictionary⟦String, AstNode⟧) → MethodType {

                //Construct the list of mixParts of the new MethodType
                def newMixParts : List⟦MixPart⟧ = emptyList⟦MixPart⟧
                for (signature) do { mPart : MixPart →
                    def newParams : List⟦Param⟧ = emptyList⟦Param⟧
                    for (mPart.parameters) do { param : Param →
                        def newTypeAnno : ObjectType =
                            replaceType(param.typeAnnotation) with(replacements)

                        if (debug) then {
                            io.error.write("\n389 newTypeAnno is {newTypeAnno}")
                        }
                        newParams.add(aParam.withName (param.name)
                                                          ofType (newTypeAnno))
                    }
                    newMixParts.add(aMixPartWithName(mPart.name)
                                                          parameters(newParams))
                }

                //Check to see if the return type needs to be replaced
                def newReturn : ObjectType = replaceType(retType)
                                                              with(replacements)

                //Return with new MethodType
                def newMeth : MethodType = signature(newMixParts)
                                                          returnType(newReturn)

                //Save any type params weren't initialized
                def newTypeParams : List⟦String⟧ = emptyList⟦String⟧
                for (typeParams) do { each : String →
                    if (replacements.containsKey(each).not) then {
                        newTypeParams.add(each)
                    }
                }
                newMeth.typeParams.addAll(newTypeParams)
                newMeth
            }

            // Replaces the oType that is holding a node representing an
            // uninitialized type param with its replacement in the dictionary
            //
            // Note: -type params are stored as identifiernodes
            //       -it is possible that a type param will remain uninitialized
            //          if the dictionary does not contain its name
            method replaceType(oType : ObjectType)
                            with(replacements : Dictionary⟦String, AstNode⟧)
                                                  → ObjectType is confidential {
                // If the oType is already initialized, it can't be a type param
                if (oType.isResolved) then {
                    return oType
                }

                // Call the helper function with the AstNode stored in oType
                def replacedNode : AstNode = replaceNode(oType.getNode)
                                                              with(replacements)
                return anObjectType.definedByNode(replacedNode)
            }

            // Replaces node with
            method replaceNode(node : AstNode)
                            with(replacements : Dictionary⟦String, AstNode⟧)
                                                    → AstNode is confidential {
                if (node.kind == "generic") then {
                    def newArgs : List⟦AstNode⟧ = emptyList⟦AstNode⟧
                    for (node.args) do { arg : AstNode →
                        newArgs.add(replaceNode(arg) with(replacements))
                    }
                    return ast.genericNode.new(node.value, newArgs)
                } elseif {node.kind == "op"} then {
                    def left: AstNode= replaceNode(node.left) with(replacements)
                    def right:AstNode= replaceNode(node.right)with(replacements)
                    return ast.opNode.new(node.value, left, right)
                } elseif {node.kind == "identifier"} then {
                    if (replacements.containsKey(node.nameString)) then {
                        return replacements.at(node.nameString)
                    }
                }
                return node
            }

            method asString → String is override { show }
        }
    }

    // Create method type with no parameters, but returning rType
    method member (name : String) ofType (rType : ObjectType) → MethodType {
        signature(list[aMixPartWithName (name) parameters (list[])])
                                                              returnType (rType)
    }

    // if node is a method, class, method signature,
    // def, or var, create appropriate method type
    method fromNode (node: AstNode) → MethodType {
        match(node) case{ meth :share.Method|share.Class|share.MethodSignature →
            def signature: List⟦MixPart⟧ = list[]

            for (meth.signature) do { part:AstNode →
                def params: List⟦Param⟧ = list[]

                for (part.params) do { param: AstNode → // not of type Param??
                    params.add (aParam.withName (param.value)
                        ofType (anObjectType.definedByNode (param.dtype)))
                }

                signature.add (aMixPartWithName (part.name) parameters (params))
            }

            def rType: AstNode = match (meth)
                case { m : share.Method | share.Class → m.dtype}
                case { m : share.MethodSignature → m.rtype}

            def mType : MethodType = signature (signature)
                                returnType (anObjectType.definedByNode (rType))
            if (false ≠ meth.typeParams) then {
                for (meth.typeParams.params) do { ident : share.Identifier →
                    mType.typeParams.add(ident.nameString)
                }
            }
            mType

        } case { defd : share.Def | share.Var →

            def signature: List⟦MixPart⟧ =
                    list[aMixPartWithName (defd.name.value) parameters (list[])]
            def dtype: ObjectType = if (defd.dtype == false) then {
                anObjectType.dynamic
            } else {
                anObjectType.definedByNode (defd.dtype)
            }
            return signature (signature) returnType (dtype)
        } case { _ →
            Exception.raise "unrecognised method node" with(node)
        }
    }
}

def aGenericType : GenericTypeFactory is public = object{

    //Create a GenericType from
    class fromName(name' : String) parameters(typeParams' : List⟦String⟧)
                                objectType(oType' : ObjectType) → GenericType {
        //May be unnecessary since we already store 'name' as the key in scope
        def name : String is public = name'

        //The generic type names used within this type
        def typeParams : List⟦String⟧ is public = typeParams'

        //The ObjectType belonging to this generic type
        var oType : ObjectType is public:= oType'

        //Takes a list of replacement ObjectTypes and replaces references to the
        //typeParams stored in oType with their counterpart in the list
        method apply(replacementTypes : List⟦AstNode⟧) → ObjectType {
            if(replacementTypes.size ≠ typeParams.size) then {
                TypeError.raise("Wrong number of type parameters given when " ++
                            "instantiating generic type {name}. Attempted to" ++
                              " replace {typeParams} with {replacementTypes}.")
            }

            //First, resolve the oType if needed
            oType := oType.resolve

            //Create a mapping of GenericTypes-to-ObjectTypes
            def replacements : Dictionary⟦String, AstNode⟧ =
                                    makeDictionary(typeParams, replacementTypes)

            //Tells each method to replace references to any of the typeParams
            def appliedMethods : Set⟦MethodType⟧ = emptySet⟦MethodType⟧
            for (oType.methods) do { meth : MethodType →
                appliedMethods.add(meth.replaceGenericsWith(replacements))
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
        def name : String = typeDec.nameString

        def typeParams : List⟦String⟧ = emptyList

        var oType : ObjectType := anObjectType.definedByNode(typeDec.value)

        for (typeDec.typeParams.params) do { param : AstNode →
            typeParams.add(param.nameString)
        }

        fromName(name) parameters(typeParams) objectType(oType)
    }
}

// Object type information.
def anObjectType: ObjectTypeFactory is public = object {

    //Version of ObjectType that allows for lazy-implementation of type checking
    //Holds the AstNode that can be resolved to the real ObjectType
    class definedByNode (node': AstNode) -> ObjectType{
        var node : AstNode := node'

        // ObjectTypes that are defined by an opNode can store a TypeOp with
        // relevant information for type checking
        var storedOp : TypeOp

        method methods -> Set⟦MethodType⟧ {
            resolve.methods
        }

        method getMethod (name : String) → MethodType | noSuchMethod {
            resolve.getMethod(name)
        }

        //Process the AstNode to get its ObjectType
        method resolve -> ObjectType {
            fromDType(node)
        }

        //an ObjectType is unresolved if it has yet to collect its methods and,
        //optionally, variant types
        def isResolved : Boolean is public = false

        method isOp → Boolean{
            node.kind == "op"
        }

        def isDynamic : Boolean is public = false

        //The asString of a definedByNode contains the information needed to
        //determine equivalency
        method == (other:ObjectType) → Boolean {
            asString == other.asString
        }

        method isSubtypeOf (other : ObjectType) -> Boolean {
            isSubtypeHelper(emptyList⟦TypePair⟧, other).ans
        }

        method isSubtypeHelper(trials:List⟦TypePair⟧, other:ObjectType) → Answer{
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
                                                                  trials, other)
                    return answerConstructor((left.ans || right.ans), trials)
                } elseif {getOpNode.op == "|"} then {
                    if (left.ans.not) then {
                        return left
                    }
                    def right : Answer = getOpNode.right.isSubtypeHelper(
                                                                  trials, other)
                    return answerConstructor((left.ans && right.ans), trials)
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
            if (nd.kind == "op") then {
                def left : ObjectType = definedByNode(nd.left)
                def right : ObjectType = definedByNode(nd.right)
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
    }


    method fromMethods (methods' : Set⟦MethodType⟧) → ObjectType {
        object {
            // List of variant types (A | B | ... )
            var variantTypes : List⟦ObjectType⟧ := list[]

            // ObjectTypes that are defined by an opNode can store a TypeOp with
            // relevant information for type checking
            var storedOp : TypeOp

            def methods : Set⟦MethodType⟧ is public = (if (base == dynamic)then {
                emptySet } else { emptySet.addAll(base.methods) }).addAll(methods')

            method getMethod(name : String) → MethodType | noSuchMethod {
                for(methods) do { meth →
                    if (meth.nameString == name) then {
                        return meth
                    }
                }
                return noSuchMethod
            }

            method resolve -> ObjectType { self }

            def isResolved : Boolean is public = true

            var isOp : Boolean is public := false

            def isDynamic : Boolean is public = false

            //TODO: not sure we trust this. May have to refine == in the future
            //method == (other:ObjectType) → Boolean { self.isMe(other) }
            method == (other:ObjectType) → Boolean {
                asString == other.asString
            }

            //Check if 'self', which is the ObjectType calling this method, is
            //a subtype of 'other'
            method isSubtypeOf(other : ObjectType) → Boolean {
                isSubtypeHelper(emptyList⟦TypePair⟧, other.resolve).ans
            }

            //Make confidential
            //helper method for subtyping a pair of non-variant types
            method isNonvariantSubtypeOf(trials: List⟦TypePair⟧,
                                                  other:ObjectType) → Answer {
                //for each method in other, check that there is a corresponding
                //method in self
                for (other.methods) doWithContinue { otherMeth, continue→
                    for (self.methods) do { selfMeth→
                        def isSpec: Answer =
                              selfMeth.isSpecialisationOf(trials, otherMeth)
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
            //Param other - The ObjectType that self is checked against
            method isSubtypeHelper(trials:List⟦TypePair⟧, other':ObjectType)
                                                                      → Answer {
                def selfOtherPair : TypePair = typePair(self, other')

                //if trials already contains selfOtherPair, we can assume
                //self <: other
                if ((self == other') || {trials.contains(selfOtherPair)}) then{
                    return answerConstructor(true, trials)
                } else {
                    trials.add(selfOtherPair)
                }

                //If other is unresolved, then resolving it will give us either
                //a resolved ObjectType or an unresolved opNode
                var other : ObjectType := other'
                if ((other.isResolved.not) && {other.isOp.not}) then {
                    other := other.resolve
                }

                if((other.isDynamic)||{other == doneType}||{self == other})then{
                    return answerConstructor(true,trials)
                } elseif {self == doneType} then {
                    return answerConstructor(false, trials)
                }

                if ((other.isResolved.not) && (other.isOp)) then {
                    def otherOp : TypeOp = other.getOpNode
                    def left : Answer = self.isSubtypeHelper
                                                          (trials, otherOp.left)

                    if (otherOp.op == "&") then {
                        //Check left's result to see if right need be evaluated
                        if (left.ans.not) then {
                            return left
                        }
                        def right : Answer = self.isSubtypeHelper
                                                        (trials, otherOp.right)
                        return answerConstructor((left.ans && right.ans),trials)
                    } elseif {otherOp.op == "|"} then {
                        if (left.ans) then {
                            return left
                        }
                        def right : Answer = self.isSubtypeHelper
                                                        (trials, otherOp.right)
                        return answerConstructor((left.ans || right.ans),trials)
                    }
                }

                // Divides subtyping into 4 cases
                var helperResult : Answer := answerConstructor(false, trials)
                if (self.getVariantTypes.size == 0) then {
                    if (other.getVariantTypes.size == 0) then {
                        //Neither self nor other are variant types
                        return self.isNonvariantSubtypeOf(trials, other)
                    } else {
                        //self is not a variant type; other is
                        for (other.getVariantTypes) do {t:ObjectType →
                            helperResult := self.isNonvariantSubtypeOf(trials,t)
                            if (helperResult.ans) then{
                                return helperResult
                            }
                        }
                        return helperResult
                    }
                } else {
                    if (other.getVariantTypes.size == 0) then {
                        //self is a variant type; other is not
                        for (self.getVariantTypes) do {t:ObjectType →
                            helperResult := t.isNonvariantSubtypeOf(
                                                                  trials, other)
                            if (helperResult.ans.not) then {
                                return helperResult
                            }
                        }
                        return helperResult
                    } else {
                        //Both self and other are variant types
                        for (self.getVariantTypes) do {t:ObjectType →
                            helperResult := t.isSubtypeHelper(
                                                    helperResult.trials, other)
                            if (helperResult.ans.not) then {
                                return helperResult
                            }
                        }
                        return helperResult
                    }
                }
            }

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

            method getVariantTypes → List⟦ObjectType⟧ { variantTypes }

            method setVariantTypes(newVariantTypes:List⟦ObjectType⟧) → Done {
                variantTypes := newVariantTypes
            }

            method getOpNode → TypeOp {
                storedOp
            }

            method setOpNode (op : TypeOp) → Done {
                isOp := true
                storedOp := op
            }

            // Variant
            // Construct a variant type from two object types.
            // Note: variant types can only be created by this constructor.
            method |(other' : ObjectType) → ObjectType {
                def other : ObjectType = other'.resolve
                if (self == other) then { return self }
                if (other.isDynamic) then { return dynamic }

                // Collect methods that the two types have in common;
                // This only collects methods with the exact same param types
                // and return types. We could do more by collecting methods that
                // are specialisations.
                def combine: Set⟦MethodType⟧ = emptySet
                for(self.methods) doWithContinue { meth:MethodType, continue →
                    for(other.methods) do { meth':MethodType →
                        if (meth == meth') then {
                            combine.add(meth)
                            continue.apply
                        }
                    }
                }

                // Variant types of the new object.
                var newVariantTypes := list[]

                // If self is a variant type, add its variant types
                // to the new object type's variant types.
                // If self is not a variant type, add itself to the
                // new object type's variant types.
                if (self.getVariantTypes.size != 0) then {
                    newVariantTypes := newVariantTypes ++ self.getVariantTypes
                } else {
                    newVariantTypes.add(self)
                }

                // If other is a variant type, add its variant types
                // to the new object type's variant types.
                // If other is not a variant type, add itself to the
                // new object types's variant types.
                if (other.getVariantTypes.size != 0) then {
                    newVariantTypes := newVariantTypes ++ other.getVariantTypes
                } else {
                    newVariantTypes.add(other)
                }

                def op : TypeOp = typeOp("|", self, other)

                return object {

                    inherit anObjectType.fromMethods(combine)

                    // Set the new object type's variant types to equal
                    // the new variant types.
                    self.setVariantTypes(newVariantTypes)
                    self.setOpNode(op)

                    method asString → String is override {
                        "{outer} | {other}"
                    }
                }
            }

            method &(other' : ObjectType) → ObjectType {
                def other : ObjectType = other'.resolve

                if (other.isDynamic) then {
                    return dynamic
                }

                if (self == other) then {
                    return self
                }

                //components from performing &-operator on variant types
                def components : List⟦ObjectType⟧ = emptyList⟦ObjectType⟧
                if (self.getVariantTypes.size == 0) then {
                    if (other.getVariantTypes.size == 0) then {
                        //Neither self nor other are variant types
                        return self.andHelper(other)
                    } else {
                        //self is not a variant type; other is
                        for (other.getVariantTypes) do {t:ObjectType →
                            components.add(self.andHelper(t))
                        }
                    }
                } else {
                    if (other.getVariantTypes.size == 0) then {
                        //self is a variant type; other is not
                        for (self.getVariantTypes) do {t:ObjectType →
                            components.add(other.andHelper(t))
                        }
                    } else {
                        //Both self and other are variant types
                        for (self.getVariantTypes) do {t:ObjectType →
                            components.add(t&(other))
                        }
                    }
                }
                //Helper method does not work with Done
                def temp : ObjectType = fromObjectTypeList(components)
                def result : ObjectType = fromMethods(temp.methods)

                result.setVariantTypes(temp.getVariantTypes)
                result.setOpNode(typeOp("&", self, other))

                result
            }

            //Make confidential
            method andHelper(other: ObjectType) → ObjectType {
                def combine : Set⟦MethodType⟧ = emptySet
                def inBoth : Set⟦String⟧ = emptySet

                // Produce union between two object types.
                for(methods) doWithContinue { meth, continue →
                    for(other.methods) do { meth':MethodType →
                        if (meth.nameString == meth'.nameString) then {
                            if (meth.isSpecialisationOf(emptyList⟦TypePair⟧,meth').ans) then {
                                combine.add(meth)
                            } elseif {meth'.isSpecialisationOf(emptyList⟦TypePair⟧,meth).ans} then {
                                combine.add(meth')
                            } else {
                                // TODO: Perhaps generate lub of two types?
                                TypeError.raise("cannot produce union of " ++
                                    "incompatible types '{self}' and " ++
                                    "'{other}' because of {meth'}")
                            }
                            inBoth.add(meth.name)
                            continue.apply
                        }
                    }
                    combine.add(meth)
                }

                for(other.methods) do { meth : MethodType →
                    if (inBoth.contains(meth.name).not) then {
                        combine.add(meth)
                    }
                }

                def result : ObjectType = fromMethods(combine)
                result.setOpNode(typeOp("&", self, other))

                result
            }

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

    method fromMethods(methods' : Set⟦MethodType⟧) withName(name : String)
                                                                  → ObjectType {
        object {
            inherit fromMethods(methods')

            method asString → String is override {
                if (methods.size == base.methods.size) then { return "Object" }
                var out: String := name
                for(methods') do { mtype: MethodType →
                    out := "{out}\{ "
                    if (base.methods.contains(mtype).not) then {
                        out := "{out}\n    {mtype}; "
                    }
                    out := "{out}\n  \}"
                }
                return "{out}"
            }
        }
    }

    //takes an AstNode and returns its corresponding ObjectType
    method fromDType(dtype : AstNode) → ObjectType {
        if (debug) then {
            //io.error.write"\nTypes scope is {scope.types}"
        }
        match(dtype) case { (false) →
            dynamic

        } case { typeDec : share.TypeDeclaration →
            ProgrammingError.raise("Types cannot be declared inside other " ++
                                                            "types or objects")

        } case { typeLiteral : share.TypeLiteral →
            def meths : Set⟦MethodType⟧ = emptySet
            //collect MethodTypes
            for(typeLiteral.methods) do { mType : AstNode →
                meths.add(aMethodType.fromNode(mType))
            }
            anObjectType.fromMethods(meths)

        } case { op: share.Operator →
            // Operator takes care of type expressions: Ex. A & B, A | C
            // What type of operator (& or |)?
            var opValue: String := op.value

            // Left side of operator
            var left: AstNode := op.left
            var leftType: ObjectType := fromDType(left)

            // Right side of operator
            var right: AstNode := op.right
            var rightType: ObjectType := fromDType(right)

            match(opValue) case { "&" →
              leftType & rightType
            } case { "|" →
              leftType | rightType
            } case { _ →
              ProgrammingError.raise("Expected '&' or '|', got {opValue}")
                                                                        with(op)
            }

        } case { ident : share.Identifier →
            //look for identifier in the scope. Add/update it in the scope if
            //not already there.
            fromIdentifier(ident)

        } case { generic : share.Generic →
            //get the objecttype from instantiating the generic type and
            //add it to the types scope.
            fromGeneric(generic)

        } case { member : share.Member →
            //name of the receiver
            def recName : String = member.receiver.toGrace(0)
            var memberCall : String := "{recName}.{member.value}"

            //check if the receiver consists of multiple calls
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
                fromDType(ast.genericNode.new(ast.identifierNode.new(
                                          memberCall, false), member.generics))
            } else {
                scope.types.find(memberCall) butIfMissing {
                              ScopingError.raise("Failed to find {memberCall}")}
            }

        } case { _ →
            ProgrammingError.raise "No case for node of kind {dtype.kind}"
                                                                    with(dtype)
        }
    }

    //Currently only accept ast.genericNodes and will return the name of the
    //type if it was stored in the types scope
    method typesScopeName(generic : share.Generic) → String is confidential{
        var genName : String := generic.nameString
        for (generic.args) do { arg : AstNode →
            def argName : String = if (arg.kind == "typeliteral") then {
                fromDType(arg).asString
            } else {
                arg.nameString
            }
            genName := "{genName}${argName}"
        }
        genName
    }

    //Create an ObjectType from a GenericNode; the type used in the GenericNode
    //must have already been declared and put into the generics scope. The types
    //scope will also contain the returned ObjectType after this method is done.
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
                                  "generic type {node.nameString}.") with(node)
            }

            //Replace the typeParameters with replacementTypes
            def instantiated : ObjectType = genType.apply(node.args)

            //Update the types scope and return
            if (debug) then {
                io.error.write("\n1411 adding to {genName} with {instantiated}")
            }
            scope.types.addToGlobalAt(genName) put(instantiated)
            instantiated
        }
    }



    //Find ObjectType corresponding to the identifier in the scope. If not
    //already there, adds it to the scope.
    method fromIdentifier(ident : share.Identifier) → ObjectType {
        if (debug) then {
            io.error.write("\n1249 fromIdentifier - looking for {ident.value}"++
                                                      " inside {scope.types}")
        }
        //check if identifier is generic, and if so, turn it into a
        //generic node and recurse so the generic case can handle it.
        if(ident.generics ≠ false) then {
            return fromDType(ast.genericNode.new(ident, ident.generics))
        }

        def oType : ObjectType = scope.types.find(ident.value)
                butIfMissing{//ScopingError.raise("Failed to find {ident.value}")
                      base
        }

        //If the type we are referencing is unresolved and not an opNode,
        //then resolve it and update the types scope
        if (oType.isResolved || {oType.isOp}) then{
            return oType
        } else {
            def resolvedOType : ObjectType = oType.resolve
            scope.types.addToGlobalAt(ident.value) put (resolvedOType)
            resolvedOType
        }
    }


    method dynamic → ObjectType {
        object {
            def methods: Set⟦MethodType⟧ is public = sg.emptySet

            method getMethod(_ : String) → noSuchMethod { noSuchMethod }

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

            method |(_ : ObjectType) → dynamic { dynamic }

            method &(_ : ObjectType) → dynamic { dynamic }

            def asString : String is public, override = "Unknown"
        }
    }

    method bottom → ObjectType {
        object {
            inherit dynamic
            def isDynamic : Boolean is public, override = false
            def asString : String is public, override = "Bottom"
        }
    }

    method blockTaking(params : List⟦Parameter⟧)
            returning(rType : ObjectType) → ObjectType {
        def signature = list[aMixPartWithName("apply") parameters(params)]
        def meths: Set⟦MethodType⟧ = emptySet
        meths.add(aMethodType.signature(signature) returnType(rType))
        fromMethods(meths) withName("Block")
    }

    method blockReturning(rType : ObjectType) → ObjectType {
        blockTaking(list[]) returning(rType)
    }

    // add method to oType.  Only use this variant if method is parameterless
    method addTo (oType : ObjectType) name (name' : String)
                          returns (rType : ObjectType) → Done is confidential {
        def signature = list[aMixPartWithName(name') parameters(list[])]
        oType.methods.add (aMethodType.signature(signature) returnType(rType))
    }

    // add method to oType.  Only use this variant if one part with one or more
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
            param(ptype : ObjectType) returns (rType : ObjectType)
            → Done is confidential {
        def parameters = list[aParam.ofType(ptype)]

        def signature = list[aMixPartWithName(name') parameters(parameters)]

        oType.methods.add (aMethodType.signature(signature) returnType(rType))
    }

    // add method to oType.  Only use if more than one part.
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
             signature.add(aMixPartWithName(name.at(i)) parameters(parts.at(i)))
         }
         oType.methods.add(aMethodType.signature (signature) returnType (rType))
    }


    method extend(this : ObjectType) with(that : ObjectType)
            → Done is confidential {
        this.methods.addAll(that.methods)
    }

    // TODO: Make sure get everything from standardGrace and
    // StandardPrelude
    var base : ObjectType is readable := dynamic
    def doneType : ObjectType is public = fromMethods(sg.emptySet)
                                                                withName("Done")
    base := fromMethods(sg.emptySet) withName("Object")

    // Used for type-checking imports; please update when additional types are
    // added
    def preludeTypes: Set⟦String⟧ is public = ["Done", "Pattern", "Iterator",
                                  "Boolean", "Number", "String", "List", "Set",
                                  "Sequence", "Dictionary", "Point", "Binding",
                                  "Collection", "Enumerable", "Range", "Object"]

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
    def collection : ObjectType is public = fromMethods(sg.emptySet)
                                                  withName("Collection")
    def enumerable : ObjectType is public = fromMethods(sg.emptySet)
                                                  withName("Enumerable")
    def rangeTp : ObjectType is public = fromMethods(sg.emptySet)
                                                  withName("Range")
    //def prelude: ObjectType is public = fromMethods(sg.emptySet)
    //                                            withName("Prelude")
    def boolBlock: ObjectType is public = fromMethods(sg.emptySet)
                                                  withName("BoolBlock")
    def doneBlock: ObjectType is public = fromMethods(sg.emptySet)
                                                  withName("DoneBlock")
    def dynamicDoneBlock: ObjectType is public = fromMethods(sg.emptySet)
                                                  withName("DynamicDoneBlock")

//    addTo (base) name ("==") param(base) returns(boolean)
    addTo (base) name ("≠") param(base) returns(boolean)
    addTo (base) name ("hash") returns(number)
//    addTo (base) name ("match") returns(dynamic)
    addTo (base) name ("asString") returns(string)
    addTo (base) name ("basicAsString") returns(string)
    addTo (base) name ("asDebugString") returns(string)
//    addTo (base) name ("debugValue") returns(string)
//    addTo (base) name ("debugIterator") returns(iterator)
    addTo (base) name ("::") returns(binding)
    addTo (base) name ("list") param(collection) returns(listTp)

    extend (pattern) with (base)
    addTo (pattern) name ("match") param (base) returns (dynamic)
    addTo (pattern) name ("|") param (pattern) returns (pattern)
    addTo (pattern) name("&") param (pattern) returns (pattern)

    extend (iterator) with (base)
    addTo (iterator) name ("hasNext") returns (boolean)
    addTo (iterator) name ("next") returns (dynamic)

    def shortCircuit: ObjectType =
        blockTaking(list[aParam.ofType(blockReturning(dynamic))])returning(base)
    extend (boolean) with(base)
    addTo (boolean) name("&&") param(boolean) returns (boolean)
    addTo (boolean) name("||") param(boolean) returns (boolean)
    addTo (boolean) name("prefix!") returns (boolean)
    addTo (boolean) name("not") returns (boolean)
    addTo (boolean) name("andAlso") param(shortCircuit) returns (dynamic)
    addTo (boolean) name("orElse") param(shortCircuit) returns (dynamic)
    addTo (boolean) name ("==") param (base) returns (boolean)

    extend (number) with(base)
    addTo (number) name("+") param(number) returns(number)
    addTo (number) name("*") param(number) returns(number)
    addTo (number) name("-") param(number) returns(number)
    addTo (number) name("/") param(number) returns(number)
    addTo (number) name("^") param(number) returns(number)
    addTo (number) name("%") param(number) returns(number)
    addTo (number) name("@") param(number) returns(point)
    addTo (number) name("hashcode") returns(string)
    addTo (number) name("++") param(base) returns(string)
    addTo (number) name("<") param(number) returns(boolean)
    addTo (number) name(">") param(number) returns(boolean)
    addTo (number) name ("==") param(base) returns(boolean)
    addTo (number) name("<=") param(number) returns(boolean)
    addTo (number) name("≤") param(number) returns(boolean)
    addTo (number) name(">=") param(number) returns(boolean)
    addTo (number) name("≥") param(number) returns(boolean)
    addTo (number) name("..") param(number) returns(listTp)
    addTo (number) name("asInteger32") returns(number)
    addTo (number) name("prefix-") returns(number)
    addTo (number) name("inBase") param(number) returns(number)
    addTo (number) name("truncated") returns(number)
    addTo (number) name("rounded") returns(number)
    addTo (number) name("prefix<") returns(pattern)
    addTo (number) name("prefix>") returns(pattern)
    addTo (number) name("abs") returns(number)

    def ifAbsentBlock: ObjectType = blockTaking (list[]) returning (dynamic)
    def stringDoBlock: ObjectType = blockTaking (list[aParam.ofType(string)])
            returning(doneType)
    def stringKeysValuesDoBlock: ObjectType =
        blockTaking (list[aParam.ofType(number), aParam.ofType(string)])
           returning (doneType)
    extend (string) with(base)
    addTo (string) name("*") param(number) returns(string)
    addTo (string) name("&") param(pattern) returns(pattern)
    addTo (string) name("++") param(base) returns(string)
    addTo (string) name ("==") param(string) returns(boolean)
    addTo (string) name(">") param(string) returns(boolean)
    addTo (string) name(">=") param(string) returns(boolean)
    addTo (string) name("<") param(string) returns(boolean)
    addTo (string) name("<=") param(string) returns(boolean)
    addTo (string) name("≤") param(string) returns(boolean)
    addTo (string) name("≥") param(string) returns(boolean)
    addTo (string) name("at") param(number) returns(string)
    addTo (string) name("asLower") returns(string)
    addTo (string) name("asNumber") returns(number)
    addTo (string) name("asUpper") returns(string)
    addTo (string) name("capitalized") returns(string)
    addTo (string) name("compare") param(string) returns(boolean)
    addTo (string) name("contains") param(string) returns(boolean)
    addTo (string) name("encode") returns(string)
    addTo (string) name("endsWith") param(string) returns(boolean)
    addTo (string) name ("indexOf") param(string) returns (number)
    addTo(string) names (list["indexOf","startingAt"])
           parts(list [ list[string], list[number] ]) returns (number)
//    addTo(string) names(["indexOf","startingAt","ifAbsent"])
//       parts([ [string], [number],[ifAbsentBlock] ]) returns(number | dynamic)
//    addTo(string) names(["indexOf","startingAt","ifAbsent"])
//       parts([ [string], [number],[ifAbsentBlock] ]) returns(number | dynamic)
    addTo (string) name ("lastIndexOf") param(string) returns(number)
    addTo (string) names (list["lastIndexOf","startingAt"])
        parts(list[ list[string], list[number] ]) returns (number)
//    addTo(string) names(["lastIndexOf","ifAbsent"])
        //parts([ [string], [ifAbsentBlock] ]) returns(number | dynamic)
//    addTo(string) names(["lastIndexOf","startingAt","ifAbsent"])
        //parts([ [string], [number],[ifAbsentBlock] ])returns(number | dynamic)
    addTo(string) name ("indices") returns(listTp)
    addTo(string) name("isEmpty") returns(boolean)
    addTo(string) name("iterator") returns(base)
    addTo(string) name("lastIndexOf") param(string) returns(number)
//    addTo(string) name("lastIndexOf()ifAbsent") params(string, ifAbsentBlock)
                                                    //returns(number | dynamic)
//    addTo(string) name("lastIndexOf()startingAt()ifAbsent")
                      //params(string, ifAbsentBlock) returns(number | dynamic)
    addTo(string) name("ord") returns(number)
    addTo(string) names(list["replace","with"])
                      parts(list[ list[string], list[string] ]) returns(string)
    addTo(string) name("size") returns(number)
    addTo(string) name("startsWith") param(string) returns(boolean)
    addTo(string) name("startsWithDigit") returns(boolean)
    addTo(string) name("startsWithLetter") returns(boolean)
    addTo(string) name("startsWithPeriod") returns(boolean)
    addTo(string) name("startsWithSpace") returns(boolean)
    addTo(string) names(list["substringFrom","size"])
        parts(list[ list[number], list[number] ]) returns(string)
    addTo(string) names(list["substringFrom","to"])
        parts(list[ list[number], list[number] ]) returns(string)
    addTo(string) name("_escape") returns(string)
    addTo(string) name("trim") returns(string)
    addTo(string) name("do") param(stringDoBlock) returns (doneType)
    addTo(string) name("size") returns(number)
    addTo(string) name("iter") returns(iterator)

    extend(point) with(base)
    addTo (point) name ("==") param(point) returns(boolean)
    addTo(point) name("x") returns(number)
    addTo(point) name("y") returns(number)
    addTo(point) name("distanceTo") param(point) returns(number)
    addTo(point) name("+") param(point) returns(point)
    addTo(point) name("-") param(point) returns(point)
    addTo(point) name("*") param(point) returns(point)
    addTo(point) name("/") param(point) returns(point)
    addTo(point) name("length") returns(number)

    def fold: ObjectType =
              blockTaking(list[aParam.ofType(dynamic), aParam.ofType(dynamic)])
                      returning (dynamic)
    extend (listTp) with (base)
    addTo (listTp) name("at") param(number) returns(dynamic)
    addTo(listTp) names(list["at","put"])
                    parts(list[ list[number], list[dynamic] ]) returns(doneType)
    addTo(listTp) name("push") param(dynamic) returns(doneType)
    addTo(listTp) name("add") param(dynamic) returns(listTp)
    // need to add varparams(comment belongs to "addFirst")
    addTo(listTp) name("addFirst") param(dynamic) returns(listTp)
    addTo(listTp) name("addLast") param(dynamic) returns(listTp)
    addTo(listTp) name("addAll") param(listTp) returns(listTp)
    addTo(listTp) name("pop") returns(dynamic)
    addTo(listTp) name("size") returns(number)
    addTo(listTp) name("iter") returns(iterator)
    addTo(listTp) name("iterator") returns(iterator)
    addTo(listTp) name("contains") param(dynamic) returns(boolean)
    addTo(listTp) name("indexOf") param(dynamic) returns(number)
    addTo(listTp) name("indices") returns(listTp)
    addTo(listTp) name("first") returns(dynamic)
    addTo(listTp) name("last") returns(dynamic)
    addTo(listTp) name("prepended") param(dynamic) returns(listTp)
    addTo(listTp) name("++") param(listTp) returns (listTp)
    addTo(listTp) name("reduce") params(list[dynamic, fold]) returns (dynamic)
    addTo(listTp) name("reversed") returns(dynamic)
    addTo(listTp) name("removeFirst") returns(dynamic)
    addTo(listTp) name("removeLast") returns(dynamic)
    addTo(listTp) name("removeAt") param(number) returns(dynamic)
    addTo(listTp) name("remove") param(dynamic) returns(listTp)
    addTo(listTp) name("removeAll") param(listTp) returns(listTp)
    addTo(listTp) name("copy") returns(listTp)
    addTo(listTp) name("sort") returns(listTp)
    addTo(listTp) name("reverse") returns(listTp)
    addTo (listTp) name ("==") param(listTp) returns(boolean)

    extend(binding) with(base)
    addTo(binding) name("key") returns(dynamic)
    addTo(binding) name("value") returns(dynamic)

    addTo(boolBlock) name("apply") returns(boolean)
    addTo(doneBlock) name("apply") returns(doneType)


    //addTo (prelude) name("print") param(base) returns(doneType)
    //addTo (prelude) names(list["while", "do"])
              //parts(list[list[boolBlock], list[doneBlock] ]) returns(doneType)
    //addTo (prelude) names(list["for", "do"])
          //parts(list[list[listTp], list[dynamicDoneBlock] ]) returns(doneType)

    scope.types.at("Unknown") put(dynamic)
    scope.types.at("Done") put(doneType)
    scope.types.at("Object") put(base)
    scope.types.at("Pattern") put(pattern)
    scope.types.at("Boolean") put(boolean)
    scope.types.at("Number") put(number)
    scope.types.at("String") put(string)
    scope.types.at("List") put(listTp)
    scope.types.at("Set") put(set)
    scope.types.at("Sequence") put(sequence)
    scope.types.at("Dictionary") put(dictionary)
    scope.types.at("Point") put(point)
    scope.types.at("Binding") put(binding)
    scope.types.at("Range") put(rangeTp)

    scope.types.at("Iterator") put (iterator)
    scope.types.at("Collection") put (collection)
    scope.types.at("Enumerable") put (enumerable)

    //Since the type 'Object' can be overwritten by the programmer, '$Object$'
    //is a dummy type used for saving the type definition of the prelude
    //'Object'. This is used when checking specialisation of generic methods
    scope.types.at("$Object$") put (base)

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

    addVar("Iterator") ofType(iterator)
    addVar("Collection") ofType(collection)
    addVar("Enumerable") ofType(enumerable)

    addVar("done") ofType(self.doneType)
    addVar("true") ofType(boolean)
    addVar("false") ofType(boolean)
    //addVar("prelude") ofType (prelude)
}

// Adds name to variables and as parameterless method (really def, not var!)
method addVar (name : String) ofType(oType : ObjectType) → Done is confidential{
    scope.variables.at (name) put (oType)
    scope.methods.at (name) put (aMethodType.member(name) ofType(oType))
}

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
      var varType: ObjectType := oList.at(1)
      var index:Number := 2
      while {index <= oList.size} do {

        //Combine types that are subsets of one-another
        if (varType.isSubtypeOf (oList.at(index))) then {
            varType := oList.at(index)
        } elseif {oList.at(index).isSubtypeOf(varType).not} then {
            varType := varType | oList.at(index)
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
        dict.at(keys.at(index)) put (vals.at(index))
    }
    dict
}
