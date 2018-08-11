#pragma ExtendedLineups
#pragma noTypeChecks
dialect "none"
import "standardGrace" as sg

import "ast" as ast
import "lexer" as lex
import "parser" as parser
import "xmodule" as xmodule
import "io" as io
import "sys" as sys
import "SharedTypes" as share
import "ScopeModule" as sc
import "ObjectTypeModule" as ot

inherit sg.methods

type MethodType = share.MethodType
type MethodTypeFactory = share.MethodTypeFactory
type GenericType = share.GenericType
type GenericTypeFactory = share.GenericTypeFactory
type ObjectType = share.ObjectType
type ObjectTypeFactory = share.ObjectTypeFactory
type AstNode = share.AstNode
type MixPart = share.MixPart
type Param = share.Param
type Parameter = share.Parameter

def DialectError is public = xmodule.DialectError
def cache: Dictionary = sc.cache
def allCache: Dictionary = sc.allCache
def aMethodType : MethodTypeFactory = ot.aMethodType
def aGenericType : GenericTypeFactory = ot.aGenericType
def anObjectType : share.ObjectTypeFactory = ot.anObjectType
def scope: share.Scope = sc.scope
def aParam: Param = ot.aParam

def debug: Boolean = true

// Checker error

//def CheckerFailure is public = Exception.refine "CheckerFailure"

// return the return type of the block (as declared)
method objectTypeFromBlock(block: AstNode) → ObjectType {
        def bType = typeOf(block)

        if(bType.isDynamic) then { return anObjectType.dynamic }

        def numParams: Number = block.params.size
        def applyName: String = if (numParams == 0) then {
            "apply"
        } else {
            "apply({numParams})"
        }
        def apply: MethodType = bType.getMethod(applyName)

        match(apply) case { (ot.noSuchMethod) →
            def strip = {x → x.nameString}
            DialectError.raise ("1000: the expression " ++
                "`{share.stripNewLines(block.toGrace(0))}` of type '{bType}' " ++
                "on line {block.line} does not satisfy the type 'Block'") with(block)
        } case { meth : MethodType →
            return meth.retType
        }
}

// Return the return type of the block as obtained by type-checking
// the last expression in the block
method objectTypeFromBlockBody(body: Sequence⟦AstNode⟧) → ObjectType {
    if(body.size == 0) then {
        anObjectType.doneType
    } else {
        typeOf(body.last)
    }
}


// check the type of node and insert into cache associated with the node
method checkTypes (node: AstNode) → Done {
    if (debug) then {
        io.error.write "\n233: checking types of {node.nameString}"
    }
    cache.at (node) ifAbsent {
        if (debug) then {
           io.error.write "\n235: {node.nameString} not in cache"
        }
        node.accept (astVisitor)
    }
}

// check type of node, put in cache & then return type
method typeOf (node: AstNode) → ObjectType {
    checkTypes (node)
    cache.at (node) ifAbsent {
        DialectError.raise "cannot type non-expression {node} on line {node.line}" 
                with (node)
    }
}

// retrieve from cache the inheritable type of an object
method inheritableTypeOf (node: AstNode) → ObjectType {
    allCache.at (node) ifAbsent {
        DialectError.raise "cannot find confidential type of {node} on line {node.line}"
                with (node)
    }
}

// Exceptions while type-checking. (Currently not used)
//def ObjectError: outer.ExceptionKind = TypeError.refine("ObjectError")

// Class declaration error. (Currently not used)
//def ClassError: outer.ExceptionKind = TypeError.refine("Class TypeError")

// Declaration of method does not correspond to actual type
// def MethodError = TypeError.refine("Method TypeError")

// Def and var declarations.  Type of def or var declaration does not
// correspond to value associated with it
//def DialectError: outer.ExceptionKind = TypeError.refine("Def TypeError")

// Scoping error declaration with imports
//def ScopingError: outer.ExceptionKind = TypeError.refine("ScopingError")

// type of part of method request (actual call, not declaration)
type RequestPart = {
   args → List⟦AstNode⟧
   args:=(a: List⟦AstNode⟧) → Done
}

// Check if the signature and parameters of a request match
// the declaration, return the type of the result
method check (req : share.Request)
        against(meth' : MethodType) → ObjectType is confidential {

    var meth : MethodType := meth'
    if ((req.kind == "call") || (req.kind == "member")
                                          || (req.kind == "identifier")) then {
        if ((false ≠ req.generics) && (meth.hasTypeParams)) then {
            def replacementTypes : List⟦ObjectType⟧ = emptyList⟦ObjectType⟧
            for (req.generics) do { node : AstNode →
                replacementTypes.add(anObjectType.fromDType(node))
            }
            meth := meth.apply(replacementTypes)
        }
    }
    def name: String = meth.nameString

    for(meth.signature) and(req.parts) do { sigPart: MixPart, reqPart: RequestPart →
        def params: List⟦Param⟧ = sigPart.parameters
        def args: Collection⟦AstNode⟧   = reqPart.args

        def pSize: Number = params.size
        def aSize: Number = args.size

        if(aSize != pSize) then {
            def which: String = if (aSize > pSize) then { "many" } else { "few" }
            def where: Number = if (aSize > pSize) then {
                args.at (pSize + 1)
            } else {
            // Can we get beyond the final argument?
                req.value
            }

            DialectError
                .raise("too {which} arguments to method part " ++
                    "'{sigPart.name}' on line {req.line}, expected {pSize} but got {aSize}")
                    with(where)
        }

        for (params) and (args) do { param: Param, arg: AstNode →
            def pType: ObjectType = param.typeAnnotation
            def aType: ObjectType = typeOf(arg)
            if (debug) then {
                io.error.write ("\n1631 Checking {arg} of type {aType} is subtype of {pType}"++
                    "\nwhile checking {req} against {meth}")
            }
            if (aType.isConsistentSubtypeOf (pType).not) then {
                io.error.write(arg.asString ++ " at {arg.line}:{arg.linePos}")
                DialectError.raise("the expression " ++
 //               DialectError("the expression " ++
                    "`{stripNewLines(arg.toGrace(0))}` of type '{aType}' on "++
                    " line {args.at(1).line} does not " ++
                    "satisfy the type of parameter '{param}' in the " ++
                    "method '{name}'") with(arg)
            }
        }
    }
    meth.retType
}

method DialectError(message: String) with (node) → Done {
    io.error.write(message)
    sys.exit(2)
}

// Check the type of node to make sure it matches eType.
// Throw error only if type of node is not consistent subtype of eType
method check (node: AstNode) matches (eType : ObjectType)
        inMethod (name : String) → Done is confidential {
    def aType: ObjectType = typeOf(node)
    if (aType.isConsistentSubtypeOf (eType).not) then {
        DialectError.raise("the method '{name}' on line {node.line} declares a result of " ++
            "type '{eType}', but returns an expression of type " ++
            "'{aType}'") with (node)
    }
}

// break up input string into list of strings as divided by separator
method split (input : String, separator : String) → List⟦String⟧ {
    var start: Number := 1
    var end: Number := 1
    var output: List⟦ List⟦String⟧ ⟧ := list[]
    while {end < input.size} do {
        if (input.at(end) == separator) then {
            var cand := input.substringFrom(start)to(end-1)
            if (cand.size > 0) then {
                output.push(cand)
            }
            start := end + 1
        }
        end := end + 1
    }
    output.push(input.substringFrom(start)to(end))
    return output
}

// Pair of public and confidential types of an expression
// Generating objects
type PublicConfidential = {
    publicType → ObjectType
    inheritableType → ObjectType | false
}

// Returns pair of public and confidential type of expression that can be
// inherited from
class pubConf (pType: ObjectType, cType: ObjectType ) → PublicConfidential{
    method publicType → ObjectType {pType}
    method inheritableType → ObjectType {cType}
    method asString → String {
        "confidential type is {cType}\npublic type is {pType}"
    }
}


// Static type checker visitor
// methods return false if goes no further recursively
def astVisitor: ast.AstVisitor is public = object {
    inherit ast.baseVisitor

    // Default behavior serving as placeholder only for cases not yet implemented
    method checkMatch(node: AstNode) → Boolean {
        if (debug) then {
            io.error.write "1436: checkMatch in astVisitor"
        }
        true
    }

    // type-check if statement
    method visitIf (ifnode: share.If) → Boolean {
        def cond: AstNode = ifnode.value
        // make sure condition is compatible with Boolean
        if (typeOf (cond).isConsistentSubtypeOf (anObjectType.boolean).not) then {
            DialectError.raise ("1366: the expression "++
                "`{stripNewLines (cond.toGrace (0))}` on line {cond.line} does not " ++
                "satisfy the type 'Boolean' for an 'if' condition'") with (cond)
        }

        def thenType: ObjectType = objectTypeFromBlock(ifnode.thenblock)

        def hasElse: Boolean = ifnode.elseblock.body.size > 0
        def elseType: ObjectType = if (hasElse) then {
            objectTypeFromBlock(ifnode.elseblock)
        } else {  // if no else clause then type must be Done
            anObjectType.doneType
        }

        // type of expression is whichever branch has largest type.
        // If incompatible return variant formed by the two types
        def ifType: ObjectType = if (hasElse) then {
            if (thenType.isConsistentSubtypeOf (elseType)) then {
                elseType
            } elseif {elseType.isConsistentSubtypeOf(thenType)} then {
                thenType
            } else {
                thenType | elseType
            }
        } else {
            anObjectType.doneType
        }

        // save type in cache
        cache.at (ifnode) put (ifType)
        false
    }

    // Type check block.  Fails if don't give types to block parameters
    method visitBlock (block: AstNode) → Boolean {
        // Raise exception if block parameters not given types
        for (block.params) do {p→
            if (((p.kind == "identifier") || {p.wildcard.not}) && {p.decType.value=="Unknown"}) then {
                DialectError.raise("no type given to declaration"
                    ++ " of parameter '{p.value}' on line {p.line}") with (p)
            }
        }

        def body = sequence(block.body)
        // return type of block (computed)
        var retType: ObjectType

        // Type check body of block in new scope with parameters
        scope.enter {
            // add parameters & their types to new scope
            for(block.params) do { param →
                if (("string" ≠ param.dtype.kind)
                                      && {"num" ≠ param.dtype.kind}) then {
                    if (debug) then {
                        io.error.write("\n1517: {param.value} has {param.dtype}")
                    }
                    scope.variables.at(param.value)
                                      put(anObjectType.fromDType(param.dtype))
                }
            }

            // check type of all statements in block
            for(body) do { stmt: AstNode →
                checkTypes(stmt)
            }

            retType := objectTypeFromBlockBody(body)
        }
        // At this point, know block type checks.

        // Now compute type of block and put in cache
        def parameters = list[]
        for(block.params) do { param: AstNode →
            if (param.dtype.kind == "string") then {
                parameters.push(aParam.withName(param.value)
                                    ofType(anObjectType.string))
            } elseif {param.dtype.kind == "num"} then {
                parameters.push(aParam.withName(param.value)
                                    ofType(anObjectType.number))
            } else {
                parameters.push(aParam.withName(param.value)
                                    ofType(anObjectType.fromDType(param.dtype)))
            }
        }
        // The type of the block
        def blockType: ObjectType = anObjectType.blockTaking(parameters)
            returning(retType)

        cache.at (block) put (blockType)
        if (debug) then {
            io.error.write "block has type {blockType}"
        }
        false
    }

    //type checks match-case statements. Makes sure that the types of the
    //matchee and the params match, and puts the return type of the match-case
    //in the cache.
    method visitMatchCase (node: share.MatchCase) → Boolean {
        // expression being matched and its type
        def matchee = node.value
        var matcheeType: ObjectType := typeOf(matchee)
        //Note: currently only one matchee is supported

        // Keep track of parameter types in case as well as return types
        def paramTypesList: List⟦ObjectType⟧ = emptyList⟦ObjectType⟧
        def returnTypesList: List⟦ObjectType⟧ = emptyList⟦ObjectType⟧

        //goes through each case{} and accumulates its parameter and return types
        for(node.cases) do{block →

            if(block.isMatchingBlock.not) then{
              DialectError.raise("1518: The case you are matching to, " ++
                "{stripNewLines(block.toGrace(0))} on line {block.line}, has more than one argument "++
                "on the left side. This is not currently supported.") with (matchee)
            }

            //If param is a general case(ie. n:Number), accumulate its type to
            //paramTypesList; ignore if it is a specific case(ie. 47)
            def blockParam : AstNode = block.params.at(1)
            if (debug) then {
                io.error.write"\nMy dtype is {blockParam.dtype}"
            }
            if (("string" ≠ blockParam.dtype.kind)
                                  && {"num" ≠ blockParam.dtype.kind}) then {
                def typeOfParam = anObjectType.fromDType(blockParam.dtype)

                if (paramTypesList.contains(typeOfParam).not) then {
                    paramTypesList.add(typeOfParam)
                }
            }

            //Build return type collection
            def blockReturnType : ObjectType = objectTypeFromBlock(block)
            if (returnTypesList.contains(blockReturnType).not) then {
              returnTypesList.add(blockReturnType)
            }
        }
        // Type covered by parameters in case (types are variants)
        def paramType: ObjectType = ot.fromObjectTypeList(paramTypesList)

        // Type returned by variant of all return types in cases
        def returnType: ObjectType = ot.fromObjectTypeList(returnTypesList)

        if (debug) then {
            io.error.write "\nparamType now equals: {paramType}"
            io.error.write "\nreturnType now equals: {returnType}"
        }

        // If matchee not covered by cases then raise a type error
        if (matcheeType.isSubtypeOf(paramType).not) then {
            DialectError.raise("1519: the matchee `{stripNewLines(matchee.toGrace(0))}`"++
                " of type {matcheeType} on line {matchee.line} does not " ++
                "match the type(s) {paramTypesList} of the case(s)") with (matchee)
        }

        // returnType is type of the match-case statement
        cache.at(node) put (returnType)

        false
    }




    // not implemented yet
    method visitTryCatch (node: AstNode) → Boolean {
        if (debug) then {
            io.error.write "\n1544: TryCatch visit not implemented yet\n"
        }
        checkMatch (node)
    }

//    method visitMethodType (node) → Boolean {
//        io.error.write "\n1549: visiting method type {node} not implemented\n"
//
//        runRules (node)
//
//        node.parametersDo { param →
//            runRules (parameterFromNode(param))
//        }
//
//        return false
//    }

//    method visitType (node) → Boolean {
//        io.error.write "\n1561: visiting type {node} (not implemented)\n"
//        checkMatch (node)
////        io.error.write "432: done visiting type {node}"
//    }

    // type check method declaration
    method visitMethod (meth: AstNode) → Boolean {
        if (debug) then {
            io.error.write "\n1567: Visiting method {meth}\n"
        }
        // ensure all parameters have known types and method has return type
        for (meth.signature) do {s: AstNode →
            for (s.params) do {p: AstNode →
                if ((p.kind == "identifier") && {p.wildcard.not} && {p.decType.value=="Unknown"}) then {
                    DialectError.raise("no type given to declaration"
                        ++ " of parameter '{p.value}' on line {p.line}") with (p)
                }
            }
        }
        if (meth.decType.value=="Unknown") then {
            DialectError.raise ("no return type given to declaration"
                ++ " of method '{meth.value.value}' on line {meth.line}") with (meth.value)
        }

        // meth.value is Identifier Node
        def name: String = meth.value.value
        // declared type of the method being introduced
        def mType: MethodType = aMethodType.fromNode(meth)
        def returnType: ObjectType = mType.retType

        // Enter new scope with parameters to type-check body of method
        if (debug) then {
            io.error.write "\n1585: Entering scope for {meth}\n"
        }
        scope.enter {
            for (mType.typeParams) do { typeParamName : String →
                scope.types.at(typeParamName) put (anObjectType.base)
            }

            for(meth.signature) do { part: AstNode →
                for(part.params) do { param: AstNode →
                    scope.variables.at(param.value)
                                      put(anObjectType.fromDType(param.dtype))
                }
            }

            // We used to collect the type definitions in method bodies
            // but those are currently not allowed
            // collectTypes((meth.body))
            if (debug) then {
                io.error.write "\n1595: collected types for {list(meth.body)}\n"
            }

            // Check types of all methods in the body.  Special case for returns
            for(meth.body) do { stmt: AstNode →
                checkTypes(stmt)

                // Write visitor to make sure return statements have right type
                stmt.accept(object {
                    inherit ast.baseVisitor

                    // Make sure return statement return a value of same type
                    // as the method return type
                    method visitReturn(ret) → Boolean is override {
                        check (ret.value) matches (returnType) inMethod (name)
                        // note sure why record returnType?
                        cache.at(ret) put (returnType)
                        return false
                    }
                    // Don't check inside embedded methods as they have
                    // different return type from the outer method
                    method visitMethod(node) → Boolean is override {
                        false
                    }
                })
            }

            // If no body then the method must return type Done
            if(meth.body.size == 0) then {
                if(anObjectType.doneType.isConsistentSubtypeOf(returnType).not) then {
                    DialectError.raise("the method '{name}' on line {meth.line} declares a " ++
                        "result of type '{returnType}', but has no body") with (meth)
                }
            } else {
                // Calculate type of last expression in body and make sure
                // it is a subtype of the declared return type
                def lastNode: AstNode = meth.body.last
                if (share.Return.match(lastNode).not) then {
                    def lastType = typeOf(lastNode)
                    if(lastType.isConsistentSubtypeOf(returnType).not) then {
                        DialectError.raise("the method '{name}' on line {meth.line} declares a " ++
                            "result of type '{returnType}', but returns an " ++
                            "expression of type '{lastType}'") with (lastNode)
                    }
                }
                if (debug) then {
                    io.error.write ("\n2048 type of lastNode in method {meth.nameString}" ++
                                                    " is {lastNode.kind}")
                }
                // If last node is an object definition, the method can be inherited
                // from so calculate the supertype (confidential) and put in
                // allCache
                if (lastNode.kind == "object") then {
                    visitObject(lastNode)
                    def confidType: ObjectType = allCache.at(lastNode)
                    allCache.at(meth.nameString) put (confidType)
                    if (debug) then {
                        io.error.write "\n2053 confidType is {confidType} for {meth.nameString}"
                    }
                }
            }
        }

        // if method is just a member name then can record w/variables
        if (isMember(mType)) then {
            scope.variables.at(name) put(returnType)
        }

        // always record it as a method
        scope.methods.at(name) put(mType)

        // Declaration statement always has type Done
        cache.at(meth) put (anObjectType.doneType)
        false

    }

    // type check a method request
    method visitCall (req) → Boolean {
        // Receiver of request
        def rec: AstNode = req.receiver

        if (debug) then {
            io.error.write "\n1673: visitCall's call is: {rec.nameString}.{req.nameString}"
        }
//        // Look up (internal) type of self.  NO LONGER USED
//        var tempDef := scope.variables.find("$elf")
//                                              butIfMissing{anObjectType.dynamic}

        // type of receiver of request
        def rType: ObjectType = if (rec.nameString == "self") then {
            if (debug) then {
                io.error.write "\n1675: looking for type of self"
            }
            scope.variables.find("$elf") butIfMissing {
                DialectError.raise "type of self missing" with(rec)
            }
        } elseif {rec.nameString == "module()object"} then {
            // item from prelude
            if (debug) then {
                io.error.write "\n1676: looking for type of module"
            }
            scope.variables.findFromLeastRecent("$elf") butIfMissing {
                DialectError.raise "type of self missing" with(rec)
            }
        } else {  // general case returns type of the receiver
            if (debug) then {
                io.error.write "\n2085 rec.kind = {rec.kind}"
            }
            typeOf(rec)
        }
        //io.error.write "\n1680: type of receiver {rec} is {typeOf(rec)}"
        //io.error.write "\n1681: rType is {rType}"

        def callType: ObjectType = if (rType.isDynamic) then {
            if (debug) then {
                io.error.write "rType: {rType} is dynamic}"
            }
            anObjectType.dynamic
        } else {
            //Since we can't have a method or a type with the same name. A call
            //on a name can be searched in both method and type lists
            //Just have to assume that the programmer used nonconflicting names

            var name: String := req.nameString
            if (name.contains "$object(") then {
                //Adjust name for weird addition when used in inherit node
                req.parts.removeLast
                name := req.nameString
            }
            // String showing what call looks like
            def completeCall : String = req.toGrace(0)
            if (debug) then {
                io.error.write "\n2154: {completeCall}"
                io.error.write "\n2155: {req.nameString}"
                io.error.write "\nrequest on {name}"

                io.error.write "\n2000: rType.methods is: {rType.methods}"
            }
            // look for method name in type of receiver
            match(rType.getMethod(name))
              case { (ot.noSuchMethod) →
                if (debug) then {
                    io.error.write "\n2001: got to case noSuchMethod"
                    io.error.write "\n2002: scope here is {scope.variables}"
                }
                scope.types.find(completeCall) butIfMissing {
                    //Joe - possibly come back and change error msg maybe
                    //less informative, but less confusing msg

                    DialectError.raise("no such method or type'{name}' in " ++
                        "`{stripNewLines(rec.toGrace(0))}` of type\n" ++
                        "    '{rType}' \nin type \n  '{rType.methods}' used " ++
                        "on line {rec.line}")
                            with(req)
                }
            } case { meth : MethodType →
                // found the method, make sure arguments match parameter types
                if (debug) then {
                    io.error.write "\nchecking request {req} against {meth}"
                }
                // returns type of result
                check(req) against(meth)
            }
        }
        if (debug) then {
            io.error.write "\n1701: callType: {callType}"
        }
        cache.at(req) put (callType)
        // tells the callNode to typecheck its receiver, arguments, and generics
        true
    }

    // Type check an object.  Must get both public and confidential types
    method visitObject (obj :AstNode) → Boolean {
        // type check body of the method
        def pcType: PublicConfidential = scope.enter {
            processBody (list (obj.value), obj.superclass)
        }
        // Record both public and confidential methods (for inheritance)
        cache.at(obj) put (pcType.publicType)
        allCache.at(obj) put (pcType.inheritableType)
        if (debug) then {
            io.error.write "\n1971: *** Visited object {obj}"
            io.error.write (pcType.asString)
            io.error.write ("\n2153: Methods scope at end of visitObject is: " ++
                                                              scope.methods)
        }
        false
    }

    //Process dialects and import statements
    //TODO: handle dialects
    method visitModule (node: AstNode) → Boolean {  // added kim
        if (debug) then {
            io.error.write "\n1698: visiting module {node}"
        }
        // import statements in module
        def importNodes: List⟦AstNode⟧ = emptyList⟦AstNode⟧
        // All statements in module
        def bodyNodes: List⟦AstNode⟧ = list(node.value)

        //goes through the body of the module and processes imports
        for (bodyNodes) do{ nd : AstNode →
            match (nd)
              case {imp: share.Import →
                //visitimport processes the import and puts its type on
                //the variable scope and method scope
                visitImport(imp)
                importNodes.add(imp)
          //} case {dialect}
            } case {_:Object → }//Ignore non-import nodes
        }

        //removes import statements from the body of the module
        for(importNodes) do{nd : AstNode →
            bodyNodes.remove(nd)
        }

        // Create equivalent object without imports
        def withoutImport : AstNode = ast.moduleNode.body(bodyNodes)
                                named (node.nameString) scope (node.scope)
        if (debug) then {
            io.error.write "\n2186 Types scope before collecting types is {scope.types}"
        }
        // Collect types declared in module into scope
        collectTypes (list (withoutImport.body))
        if (debug) then {
            io.error.write "\n2186 Types scope after collecting types is {scope.types}"
        }
        // type check the remaining object (without import statements)
        visitObject (withoutImport)
    }

    // array literals represent collections (should fix to be lineups)
    method visitArray (lineUpLiteral) → Boolean {
        if (debug) then {
            io.error.write "\n1704: visiting array {lineUpLiteral}"
        }
        // FIX
        cache.at (lineUpLiteral) put (anObjectType.collection)
        false
    }

    // members are type-checked like calls
    method visitMember (node: AstNode) → Boolean {
        visitCall (node)
    }

    // NOT YET IMPLEMENTED
    method visitGeneric (node: AstNode) → Boolean {
        if (debug) then {
            io.error.write "\n1715: visiting generic {node} (not implemented)"
        }
        checkMatch (node)
    }

    // look up identifier's type in scope
    method visitIdentifier (ident: AstNode) → Boolean {
        //io.error.write "\nvisitIdentifier scope.variables processing node
        //    {ident} is {scope.variables}"
        def idType: ObjectType = if (ident.value == "outer") then {
            outerAt(scope.size)
        } else {
            scope.variables.find(ident.value)
                                          butIfMissing { anObjectType.dynamic }
        }
        cache.at (ident) put (idType)
        true

    }

    // Type check type declaration
    method visitTypeDec(node: share.TypeDeclaration) → Boolean {
        if (debug) then {
            io.error.write "visit type dec for {node}"
        }

        //check if the typedec is generic (type T⟦K,V⟧ = ...)
        if (false ≠ node.typeParams) then {
            //create GenericType to later be instantiated with real types
            def genType : GenericType = aGenericType.fromTypeDec(node)
            cache.at(node) put (genType.oType)
        } else {
            //get the type of the right-hand side of the equals sign
            def vType : ObjectType = anObjectType.fromDType(node.value)
            cache.at(node) put (vType)
        }
        io.error.write "\n656: type dec for node has in cache {cache.at(node)}"
        false
    }

    // TODO: Fix later
    method visitOctets (node: AstNode) → Boolean {
        if (debug) then {
            io.error.write "\n1736: visiting Octets {node} (not implemented)"
        }
        false
    }

    // type of string is String
    // Why do these return true?  Nothing to recurse on.
    method visitString (node: AstNode) → Boolean {
        cache.at (node) put (anObjectType.string)
        false
    }

    // type of number is Number
    method visitNum (node: AstNode) → Boolean {
        cache.at (node) put (anObjectType.number)
        false
    }

    // If the op is & or |, evaluate it
    // Otherwise treat it as a call
    method visitOp (node: AstNode) → Boolean {
        if (debug) then {
            io.error.write"\n2283 type checking op"
        }
        if(node.value == "&") then {
            cache.at(node) put (typeOf(node.left) & typeOf(node.right))
        } elseif {node.value == "|"} then {
            cache.at(node) put (typeOf(node.left) | typeOf(node.right))
        } else {
            visitCall(node)
        }
        false
    }

    method visitTypeLiteral(node: share.TypeLiteral) → Boolean {
        cache.at(node) put(anObjectType.fromDType(node))
        false
    }

    method visitBind (bind: AstNode) → Boolean {
        if (debug) then {
            io.error.write "\n 1758: Visit Bind"
        }
        // target of assignment
        def dest: AstNode = bind.dest

        match (dest) case { _ : share.Member →
            var nm: String := dest.nameString
            if (! nm.endsWith ":=(1)") then {
                nm := nm ++ ":=(1)"
            }
            // rec.memb
            def rec: AstNode = dest.in

            // Type of receiver
            // if receiver is self then look it up, else type check it
            def rType: ObjectType = if(share.Identifier.match(rec)
                                                && {rec.value == "self"}) then {
                scope.variables.find("$elf") butIfMissing {
                    Exception.raise "type of self missing" with(rec)
                }
            } else {
                typeOf(rec)
            }

            if (rType.isDynamic) then {
                anObjectType.dynamic
            } else {
                // look up type of the method in the receiver
                match(rType.getMethod(nm))
                  case { (ot.noSuchMethod) →
                    DialectError.raise("no such method '{nm}' in " ++
                        "`{stripNewLines(rec.toGrace(0))}` of type '{rType}'"++
                        " on line {bind.line}") with (bind)
                } case { meth : MethodType →
                    // create a new call node (instead of bind) and type check
                    def req = ast.callNode.new(dest,
                        list [ast.callWithPart.new(dest.value, list [bind.value])])
                    check(req) against(meth)
                }
            }

        } case { _ →
            // destination type
            def dType: ObjectType = typeOf(dest)

            def value: AstNode = bind.value
            // value type
            def vType: ObjectType = typeOf(value)

            // make sure value consistent with destination
            if(vType.isConsistentSubtypeOf(dType).not) then {
                DialectError.raise("the expression `{stripNewLines(value.toGrace(0))}` of type " ++
                    "'{vType}' on line {value.line} does not satisfy the type '{dType}' of " ++
                    "`{stripNewLines(dest.toGrace(0))}`") with (value)
            }
        }
        // type of bind is always Done
        cache.at (bind) put (anObjectType.doneType)
        false
    }


    // type check both def and var declarations
    method visitDefDec (defd: AstNode) → Boolean {
        if (defd.decType.value=="Unknown") then {
            // raise error if not type in declaration
            var typ: String := "def"
            if (share.Var.match(defd)) then { typ := "var" }
            DialectError.raise("no type given to declaration"
                ++ " of {typ} '{defd.name.value}' on line {defd.line}") with (defd.name)
        }
        // Declared type of feature
        var defType: ObjectType := anObjectType.fromDType(defd.dtype)
        if (debug) then {
            io.error.write "\n1820: defType is {defType}"
        }
        // initial value
        def value = defd.value

        if(false ≠ value) then {  // initial value provided
            def vType: ObjectType = typeOf(value)
            // infer type based on initial value if definition given w/out type
            if(defType.isDynamic && (defd.kind == "defdec")) then {
                defType := vType
            } elseif {vType.isConsistentSubtypeOf(defType).not} then {
                // initial value not consistent with declared type
                DialectError.raise("the expression `{stripNewLines(value.toGrace(0))}` of type " ++
                    "'{vType}'  on line {value.line} does not have type {defd.kind} " ++
                    "annotation '{defType}'") with (value)
            }
        }

        def name: String = defd.nameString
        scope.variables.at(name) put(defType)
        // If field is readable and/or writable, add public methods for getting
        // and setting
        if (defd.isReadable) then {
            scope.methods.at(name) put(aMethodType.member(name) ofType(defType))
        }
        if (defd.isWritable) then {
            def name' = name ++ ":=(1)"
            def param = aParam.withName(name) ofType(defType)
            def sig = list[ot.aMixPartWithName(name') parameters(list[param])]
            scope.methods.at(name')
                put(aMethodType.signature(sig) returnType(anObjectType.doneType))
        }
        cache.at (defd) put (anObjectType.doneType)
        false
    }

    // Handle variable declaration like definition declaration
    method visitVarDec (node: AstNode) → Boolean {
        visitDefDec (node)
    }

    // Grab information from gct file
    // Move processImport back into visitImport
    method visitImport (imp: AstNode) → Boolean {
        if (debug) then {
            io.error.write "\n1861: visiting import {imp}"
        }
        // headers of sections of gct form keys
        // Associated values are lines beneath the header
        def gct: Dictionary⟦String, List⟦String⟧⟧ = xmodule.parseGCT(imp.path)
        def impName : String = imp.nameString
        if (debug) then {
            io.error.write("\n1953 gct is {gct}")
            io.error.write("\n1954 keys are {gct.keys}\n")
        }

        //retrieves the names of public methods from imported module
        def importMethods : Set⟦MethodType⟧ = processGct(gct, impName)

        // Create the ObjectType and MethodType of import
        def impOType: ObjectType = anObjectType.fromMethods(importMethods)

        def sig: List⟦MixPart⟧ = list[ot.aMixPartWithName(impName)
                                                  parameters (emptyList⟦Param⟧)]
        def impMType: MethodType = aMethodType.signature(sig)
                                                          returnType (impOType)

        // Store import in scopes and cache
        scope.variables.at(impName) put(impOType)
        scope.methods.at(impName) put(impMType)
        cache.at(imp) put (impOType)
        if (debug) then {
            io.error.write"\n2421: ObjectType of the import {impName} is: {impOType}"
        }
        false
    }

    method processGct(gct: Dictionary⟦String, List⟦String⟧⟧, impName: String)
                                                              → Set⟦MethodType⟧ {
        def importMethods : Set⟦MethodType⟧ = emptySet

        def basicImportVisitor : ast.AstVisitor = xmodule.importVisitor(impName)
        gct.keys.do { key : String →
            //example key: 'typedec-of:MyType:'
            if (key.startsWith("typedec-of:")) then {
                //gets the name of the type
                def headerName : String = split(key, ":").at(2)
                def typeName : String = split(headerName, ".").last
                def prefx : String = headerName.substringFrom(1)
                                        to(headerName.size - typeName.size - 1)

                def tokens = lex.lexLines(gct.at(key))
                def typeDec = parser.typedec(tokens)

                if (prefx == "") then {
                    typeDec.accept(basicImportVisitor)
                } else {
                    typeDec.accept(xmodule.importVisitor("{impName}.{prefx}"))
                }

                // If the type name begins with a '$', then it is a type that
                // returns an object corresponding to a module that was publicly
                // imported by our own import. We use this type to construct the
                // method for accessing the imported module's public methods.
                if (typeName.at(1) == "$") then {
                    def importName : String = typeName.substringFrom (2)
                    def mixPart : MixPart = ot.aMixPartWithName(importName)
                                                    parameters(emptyList⟦Param⟧)

                    importMethods.add(aMethodType.signature(list[mixPart])
                        returnType (anObjectType.definedByNode(typeDec.value)))
                } else {
                    updateTypeScope(typeDec)
                }
            } elseif {key.startsWith("publicMethod:")} then {
                def tokens = lex.lexLines(gct.at(key))
                def methodType = parser.methodInInterface(tokens)
                methodType.accept(basicImportVisitor)
                importMethods.add(aMethodType.fromNode(methodType))
            }
        }
        importMethods
    }


    // type check expression being returned via return statement
    method visitReturn (node: AstNode) → Boolean {
        cache.at(node) put (typeOf(node.value))
        false
    }

    // Type check inherits clause in object
    method visitInherits (node: AstNode) → Boolean {
        if (debug) then {
            io.error.write "\n1999: visit inherits with {node} which has kind {node.kind}"
            io.error.write "\n1999: visit inherits with {node} which has receiver {node.value.receiver}"
            io.error.write "\n1999: visit inherits with {node} which has parts {node.value.parts.removeLast}"
        }
        cache.at(node) put (typeOf(node.value))
        if (debug) then {
            io.error.write "\n2000 has type {typeOf(node.value)}"
        }
        false
    }


    // TODO:  Not done
    // Should be treated like import, but at top level
    // Add to base type
    method visitDialect (node: AstNode) → Boolean {
        if (debug) then {
            io.error.write "\n1919: visiting dialect {node}"
        }
        checkMatch (node)
    }

}


// TODO: Outer not handled correctly yet.
// Not sure what this type is doing

method outerAt(i : Number) → ObjectType is confidential {
    // Required to cope with not knowing the prelude.
    if(i <= 1) then {
        return anObjectType.dynamic
    }
    if (debug) then {
        io.error.write "processing outer"
    }
    def vStack: List⟦ Dictionary⟦String, ObjectType⟧ ⟧ = scope.variables.stack

    def curr: Dictionary⟦String, ObjectType⟧ = vStack.at(i)

    return curr.at("outer") ifAbsent {
        def prev: ObjectType = outerAt(i - 1)

        def mStack: List⟦ Dictionary⟦String, MethodType⟧ ⟧ = scope.methods

        def vars: Dictionary⟦String, ObjectType⟧ = vStack.at(i - 1)
        def meths: Set⟦MethodType⟧ = mStack.at(i - 1).values

        def oType: ObjectType = anObjectType.fromMethods(meths)

        def mType: MethodType = aMethodType.member("outer") ofType(oType)

        curr.at("outer") put(oType)
        mStack.at(i).at("outer") put(mType)

        oType
    }
}

method updateTypeScope(typeDec : share.TypeDeclaration) → ObjectType {
    //check whether the typeDec is a GenericType and process accordingly
    var oType : ObjectType
    if(false ≠ typeDec.typeParams) then {
        def genType : GenericType = aGenericType.fromTypeDec(typeDec)
        oType := genType.oType
        scope.generics.addToGlobalAt(typeDec.nameString) put (genType)
    } else {
        oType := anObjectType.definedByNode(typeDec.value)
        scope.types.addToGlobalAt(typeDec.nameString) put(oType)
    }
    oType
}

method updateMethScope(meth : AstNode) → MethodType {
    def mType : MethodType = aMethodType.fromNode(meth)
    scope.methods.at(mType.nameString) put (mType)
    mType
}

// Type check body of object definition
method processBody (body : List⟦AstNode⟧, superclass: AstNode | false)
                                                → ObjectType is confidential {
    if (debug) then {
        io.error.write "\n1958: superclass: {superclass}\n"
    }
    var inheritedMethods: Set⟦MethodType⟧ := emptySet
    def hasInherits = false ≠ superclass
//    io.error.write "\n1965: hasInherits is {hasInherits}\n"
    var publicSuperType: ObjectType := anObjectType.base
    def superType: ObjectType = if(hasInherits) then {
        def inheriting: AstNode = superclass
        // TODO: make sure no self in "inherit" clause??

        if (debug) then {
            io.error.write "\nGT1981: checking types of inheriting = {inheriting}\n"
        }
        var name: String := inheriting.value.nameString
        if (name.contains "$object(") then {
            // fix glitch with method name in inheritance clause
            inheriting.value.parts.removeLast
            name := inheriting.value.nameString
        }

        // Handle exclusions and aliases
        // Unfortunately no type info with exclusions, so if code says
        // exclude p(s:String) and p in subclass takes a Number, it will
        // be dropped without any warning of the error.
        var inheritedType: ObjectType := allCache.at(name)
        inheritedMethods := inheritedType.methods.copy
        publicSuperType := typeOf(inheriting.value)
        // All public methods from supre type
        var pubInheritedMethods := publicSuperType.methods.copy

        // Throw away methods being excluded
        for (superclass.exclusions) do {ex →
            // First throw away from list of all inherited methods
            for (inheritedMethods) do {im →
                if (ex.nameString == im.nameString) then {
                    if (debug) then {
                        io.error.write "\n1126 removing {im}"
                    }
                    inheritedMethods.remove(im)
                }
            }
            // Throw away from public inherited methods
            for (pubInheritedMethods) do {im →
                if (ex.nameString == im.nameString) then {
                    pubInheritedMethods.remove(im)
                }
            }
        }

        if (debug) then {
            io.error.write "aliases: {superclass.aliases}"
        }

        // Add new confidential method for each alias given with super class
        def aliasMethods: List⟦MethodType⟧ = emptyList
        for (superclass.aliases) do {aliasPair →
            for (inheritedMethods) do {im →
                if (debug) then {
                    io.error.write "\n1144 comparing {aliasPair.oldName.value} and {im.nameString}"
                }
                if (aliasPair.oldName.value == im.nameString) then {
                    // Found a match for alias clause with im
                    // Build a new method type for alias
                    def oldSig: List⟦MixPart⟧ = im.signature
                    var aliasNm: String := aliasPair.newName.value
                    // unfortunately name has parens at end -- drop them
                    def firstParen: String = aliasNm.indexOf("(")
                    if (firstParen > 0) then {
                        aliasNm := aliasNm.substringFrom(1) to (firstParen)
                    }
                    def newFirst: MixPart = ot.aMixPartWithName(aliasNm)
                        parameters (oldSig.at(1).parameters)
                    def newSig: List⟦MixPart⟧ = oldSig.copy.at (1) put (newFirst)
                    // method type for alias
                    def newMethType: MethodType = ot.aMethodType.signature (newSig)
                                    returnType (im.returnType)
                    aliasMethods.add(newMethType)
                    if (debug) then {
                        io.error.write "\n1154: just added alias {newMethType}"
                    }
                }
            }
        }
        // Add all aliases to inherited methods, but not public ones
        inheritedMethods.addAll(aliasMethods)

        //Node of inheritedType could be inheriting.value or inheriting
        inheritedType := anObjectType.fromMethods(inheritedMethods)
        publicSuperType := anObjectType.fromMethods(pubInheritedMethods)

        if (debug) then {
            io.error.write "\1144: public super type: {publicSuperType}"
            io.error.write "\n1145: inherited type: {inheritedType}"
        }
        inheritedType
    } else {
        anObjectType.base
    }
    // Finished computing supertype, but don't associate with "super"
    if (debug) then {
        io.error.write "\n1989: superType is {superType}\n"
    }

    // Type including all confidential features
    var internalType: ObjectType

    // Type including only public features
    def publicType: ObjectType = if (superType.isDynamic) then {
        // if supertype is dynamic, then so is public type
        scope.variables.at("$elf") put (superType)
        superType
    } else {
        // Collect the method types to add the two self types.
        // Start with "isMe" method
        def isParam: Param = aParam.withName("other") ofType (anObjectType.base)
        def part: MixPart = ot.aMixPartWithName("isMe")parameters(list[isParam])

        // add isMe method as confidential
        def isMeMeth: MethodType = aMethodType.signature(list[part]) returnType (anObjectType.boolean)

        def publicMethods: Set⟦MethodType⟧ = publicSuperType.methods.copy
        def allMethods: Set⟦MethodType⟧ = superType.methods.copy
        // isMe is confidential
        allMethods.add(isMeMeth)

        // collect embedded types in these dictionaries
        def publicTypes: Dictionary⟦String,ObjectType⟧ = emptyDictionary
        def allTypes: Dictionary⟦String,ObjectType⟧ = emptyDictionary

        // gather types for all methods in object
        for(body) do { stmt: AstNode →
            if (debug) then {
                io.error.write "\n2009: processing {stmt}"
                io.error.write "\n1375: stmt's toGrace is: {stmt.toGrace(0)}"
            }

            match(stmt) case { meth : share.Method →
                // ensure any method overriding one from super class is
                // compatible
                def mType: MethodType = updateMethScope(meth)
                checkOverride(mType,allMethods,publicMethods,meth)
                
                // add new method to the collection of methods
                allMethods.add(mType)
                if (debug) then {
                    io.error.write "\n1158 Adding {mType} to allMethods"
                    io.error.write "\n1159 AllMethods: {allMethods}"
                }
                if(isPublic(meth)) then {
                    publicMethods.add(mType)
                }

                //A method that is a Member has no parameter and is identical to
                //a variable, so we also store it inside the variables scope
                if(isMember(mType)) then {
                    scope.variables.at(mType.name) put(mType.retType)
                }

            } case { defd : share.Def | share.Var →
                // Create type of method giving access to def or var value
                def mType: MethodType = aMethodType.fromNode(defd)
                if (allMethods.contains(mType)) then {
                    DialectError.raise ("A var or def {mType}  on line {defd.line} "
                        ++ "may not override an existing method from the superclass}") 
                        with (mType)
                }
                allMethods.add(mType)
                if (debug) then {
                    io.error.write "\n1177 AllMethods: {allMethods}"
                }
                //create method to access def/var if it is readable
                if(defd.isReadable) then {
                    publicMethods.add(mType)
                }

                //update scope with reference to def/var
                scope.methods.at(mType.name) put(mType)
                scope.variables.at(mType.name) put(mType.retType)

                //constructs setter method for writable vars
                if(defd.isWritable) then {
                    def name': String = defd.nameString ++ ":=" //(1)"  ?? is name right?
                    def dType: ObjectType = anObjectType.fromDType(defd.dtype)
                    def param: Param = aParam.withName(defd.nameString) ofType(dType)
                    def sig: List⟦MixPart⟧ = list[ot.aMixPartWithName(name') parameters(list[param])]

                    def aType: MethodType = aMethodType.signature(sig) returnType(anObjectType.doneType)
                    scope.methods.at(name') put(aType)
                    allMethods.add(aType)
                    if (debug) then {
                        io.error.write "\n1197 AllMethods: {allMethods}"
                    }
                    publicMethods.add(aType)
                }

            } case { td : share.TypeDeclaration →
                //Now does nothing if given type declaration; might make this
                //raise an error as embedded types are disallowed.
            } case { _ →
                    if (debug) then {
                        io.error.write"\n2617 ignored {stmt}"
                    }
            }
        }
        if (debug) then {
            io.error.write "\n1201 allMethods: {allMethods}"
        }
        internalType := anObjectType.fromMethods(allMethods)
        // Type of self as a receiver of method calls
        scope.variables.at("$elf") put (internalType)
        if (debug) then {
            io.error.write "\n1204: Internal type is {internalType}"
        }
        anObjectType.fromMethods(publicMethods)
    }
    // end creating publicType

    // External type for self -- i.e., if self is used as a parameter
    // in a method request -- can't see confidential features
    scope.variables.at("self") put(publicType)
    if (debug) then {
        io.error.write "\n2744: Type of self is {publicType}"
    }
    // Type-check the object body, dropping the inherit clause
    def indices: Collection⟦Number⟧ = if(hasInherits) then {
        2..body.size
    } else {
        body.indices
    }

    for(indices) do { i: Number →
        if (debug) then {
            io.error.write "\n2070: checking index {i} at line {body.at(i).line}"
        }
        checkTypes(body.at(i))
        if (debug) then {
            io.error.write "\n2072: finished index {i}\n"
        }
    }

    if (debug) then {
        io.error.write "\n 2674 types scope is: {scope.types}"
        io.error.write "\n 2675 methods scope is: {scope.methods}"
    }
    // return pair of public and internal type (so can inherit from it)
    pubConf(publicType,internalType)
}

// If either allMethods or publicMethods contains a method with the same name
// and number of parameters as mType then make sure mType is a specialization,
// and remove the old one from allMethods and publicMethods
method checkOverride(mType: MethodType, allMethods: Set⟦MethodType⟧,
                            publicMethods: Set⟦MethodType⟧, meth: AstNode) → Done {
    def oldMethType: MethodType = allMethods.find{m:MethodType →
        mType.nameString == m.nameString
    } ifNone {return}  // Nothing to be done if corresponding method not there
    if (debug) then {
        io.error.write "\n1233 Found new method {mType} while old was {oldMethType}"
    }
    if(mType.isSpecialisationOf(emptyList⟦TypePair⟧, oldMethType).ans.not) then {
        DialectError.raise ("Type of overriding method {mType} on line {meth.line} is not"
            ++ " a specialization of existing method {oldMethType}") with (meth)
    }
    // remove the old method type
    allMethods.remove(oldMethType)
    // TODO: Should this be public only if declared so.  Probably should be
    // error to change from public to confidential rather than ignore as here.
    if (publicMethods.contains(oldMethType)) then {
        publicMethods.remove(oldMethType)
    }
}

def TypeDeclarationError = TypeError.refine "TypeDeclarationError"

// The first pass over a body, collecting all type declarations so that they can
// reference one another declaratively.
method collectTypes(nodes : Collection⟦AstNode⟧) → Done is confidential {
    def names: List⟦String⟧ = list[]

    for(nodes) do { node →
        if(node.kind == "typedec") then {
            if (debug) then {
                io.error.write"\nmatched as typeDec"
            }
            if(names.contains(node.nameString)) then {
                DialectError.raise("The type {node.nameString} on line {node.line} uses " ++
                    "the same name as another type in the same scope") with (node)
            }

            names.push(node.nameString)

            updateTypeScope(node)
        }
    }
    io.error.write "\nGenerics scope is: {scope.generics}"
    // io.error.write "1881: done collecting types"
}


// Determines if a node is publicly available.
method isPublic(node : share.Method | share.Def | share.Var) → Boolean is confidential {
    match(node) case { _ : share.Method →
        for(node.annotations) do { ann →
            if(ann.value == "confidential") then {
                return false
            }
        }

        true
    } case { _ →
        for(node.annotations) do { ann →
            if((ann.value == "public") || (ann.value == "readable")) then {
                return true
            }
        }

        false
    }
}


// Determines if a method will be accessed as a member.
method isMember(mType : MethodType) → Boolean is confidential {
    (mType.signature.size == 1) && {
        mType.signature.first.parameters.size == 0
    }
}


// Helper methods.

// For loop with break.
method for(a) doWithBreak(bl) → Done {
    for(a) do { e →
        bl.apply(e, {
            return
        })
    }
}

// For loop with continue.
method for(a) doWithContinue(bl) → Done {
    for(a) do { e →
        continue'(e, bl)
    }
}

method continue'(e, bl) → Done is confidential {
    bl.apply(e, { return })
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

//class parameterFromNode (node) → Parameter is confidential {
//    inherit ast.identifierNode.new (node.name, node.dtype)
//    method kind { "parameter" }
//}

def thisDialect is public = object {
    method astChecker (moduleObj) { moduleObj.accept(astVisitor) }
}


class importVisitor(impName : String) → ast.AstVisitor {
    inherit ast.baseVisitor

    method visitTypeDec(typeDec:AstNode) → Boolean {
        io.error.write("\n Checking typeDec {typeDec.name}")
        //typeDec.name is a identifierBinding
        typeDec.name.name := "{impName}.{typeDec.nameString}"

        if (false ≠ typeDec.typeParams) then {
            for (typeDec.typeParams.params) do { tp : share.Identifier →
                io.error.write("\nChanging name to {impName}.{tp.name}")
                tp.name := "{impName}.{tp.name}"
            }
        }
        true
    }

    method visitIdentifier(ident:AstNode) → Boolean {
        io.error.write("\n Checking ident {ident.name}")
        if ((anObjectType.preludeTypes.contains(ident.name).not) &&
                                        (ident.isBindingOccurrence.not)) then {
            io.error.write("\nChanging name to {impName}.{ident.name}")
            ident.name := "{impName}.{ident.name}"
        }

        if (false ≠ ident.generics) then {
            Exception.raise("\n need to handle generics in visitIdentifier")
        }
        true
    }

    method visitMember(member:AstNode) → Boolean {
        if ((member.receiver == "self") || (member.receiver == "module()Object")) then {
            member.receiver := ""
        }
        if (anObjectType.preludeTypes.contains(member.request).not) then {
            member.receiver := "{impName}.{member.receiver}"
        }
        if (false ≠ member.generics) then {
            Exception.raise("\n need to handle generics in visitMember")
        }
        false
    }

    //not sure about visitcall



}
