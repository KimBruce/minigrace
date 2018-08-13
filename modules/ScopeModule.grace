#pragma ExtendedLineups
#pragma noTypeChecks
dialect "none"
import "standardGrace" as sg

import "ast" as ast
import "xmodule" as xmodule
import "io" as io
import "SharedTypes" as share

inherit sg.methods

type MethodType = share.MethodType
type GenericType = share.GenericType
type ObjectType = share.ObjectType

// The cached public type assignments.
def cache: Dictionary is readable = emptyDictionary
// cache holding confidential types for inheritance
def allCache: Dictionary is readable = emptyDictionary

// Holds dictionaries for names in scope
// Will be used for defs/vars, methods, and types
type StackOfKind⟦V⟧ = {
    stack → List⟦Dictionary⟧
    // push <name,value> into most recent level of stack
    at (name : String) put (value:V) → Done

    // add <name,value> to least recent level of stack
    addToGlobalAt(name : String) put (value : V) → Done

    // Starting from the most recent level of the scope, find name & return its
    // value. If it is not there perform action in bl.
    find (name : String) butIfMissing (bl: Function0⟦V⟧) → V
    
    findOuter (levels : Number) butIfMissing (bl: Function0⟦V⟧) → V


    // Starting from the least recent level of the scope, find name & return its
    // value. If it is not there perform action in bl.
    findFromLeastRecent (name : String) butIfMissing (bl: Function0⟦V⟧) → V
}

// class creating stack of given kind (e.g., types) holding values of type V
class stackOfKind⟦V⟧(kind : String) → StackOfKind⟦V⟧{

    // represented as stack of dictionaries
    def stack: List⟦Dictionary⟧ is public = list[emptyDictionary]

    // add <name,value> to most recent level of the scope
    method at (name : String) put (value:V) → Done {
        stack.last.at(name) put(value)
    }

    // adds to the least recent level of the scope
    method addToGlobalAt(name : String) put (value:V) → Done {
        stack.first.at(name) put(value)
    }

    // Starting from the most recent level of the scope, find name & return its
    // value. If it is not there perform action in bl.
    method find (name : String) butIfMissing (bl: Function0⟦V⟧) → V {
        var i: Number := stack.size
        while { i > 0 } do {
            var found: Boolean := true
            def val = stack.at(i).at(name) ifAbsent {
                found := false
            }
            if(found) then {
                return val
            }

            i := i - 1
        }

        return bl.apply
    }
    
    // Starting from the current scope, go outside "level"
    // number of nested objects and return the type of self there. 
    // If it is not there perform action in bl before returning.
    method findOuter (levels : Number) butIfMissing (bl: Function0⟦V⟧) → V {
        var i: Number := stack.size
        io.error.write "\n81 stack = {stack}"
        for (1..levels) do {current: Number →
            var found: Boolean := false
            while { (i > 0) && !found} do {
                io.error.write "looking for outer at level {i}"
                io.error.write "stack.at(i) is {stack.at(i)}"
                if (stack.at(i).containsKey("outer")) then {
                    found := true
                    io.error.write "\n86 Found outer at {i}"
                }
                i := i - 1
            }
            io.error.write("\n90: "++ asString)
            if (!found) then {
                return bl.apply
            }
        }
        io.error.write "final at level {i+1}"
        def outerType: V = stack.at(i+1).at("outer")
        io.error.write "\n104: type of outer: {outerType}"
        outerType
    }

    // Starting from the least recent level of the scope, find name & return its
    // value. If it is not there perform action in bl.
    method findFromLeastRecent(name: String) butIfMissing(bl: Function0⟦V⟧) → V {
        var i: Number := 1
        while { i <= stack.size } do {
            var found: Boolean := true
            def val = stack.at(i).at(name) ifAbsent {
                found := false
            }
            if(found) then {
                return val
            }

            i := i + 1
        }

        return bl.apply
    }

    // Return string representing contents of each dictionary in the stack
    method asString → String is override {
        var out: String := ""

        for(stack) do { dict:Dictionary⟦String, Object⟧ →
            out := "{out}\ndict⟬"

            dict.keysAndValuesDo { key:String, value:Object →
                out := "{out}\n  {key}::{value}"
            }
            out := "{out}\n⟭"
        }
        out
    }
}

// Data structure keeping track of all names currently accessible in program
type Scope = {
    // return stack representing current scope of each kind of object
    variables → StackOfKind⟦ObjectType⟧
    methods → StackOfKind⟦MethodType⟧
    types → StackOfKind⟦ObjectType⟧
    generics → StackOfKind⟦GenericType⟧

    // number of levels on each stack (all the same)
    size → Number
    // Enter new scope to execute block bl and then delete scope afterwards
    // returns value of bl
    enter⟦V⟧(bl: Function0⟦V⟧) → V
}

// scope consists of stacks of scopes for each of variables, methods, & types
def scope: Scope is public = object {
    // keep track of each kind of expression separately
    def variables is public = stackOfKind⟦ObjectType⟧ ("variable")
    def methods is public = stackOfKind ⟦MethodType⟧("method")
    def types is public = stackOfKind ⟦ObjectType⟧("type")
    def generics is public = stackOfKind ⟦GenericType⟧("generic")

    // number of items on stack
    method size → Number {
        variables.stack.size
    }

    // Enter new scope to execute block bl and then delete afterwards
    // returns value of bl
    method enter⟦V⟧ (bl:Function0⟦V⟧) → V {
        // create new empty environment
        variables.stack.push (sg.emptyDictionary)
        methods.stack.push (sg.emptyDictionary)
        types.stack.push (sg.emptyDictionary)

        // execute bl in the new environment
        def result: V = bl.apply

        // release new environment as going out of scope
        variables.stack.pop
        methods.stack.pop
        types.stack.pop

        result
    }

    // Returns string including number of levels in scope
    method asString → String is override {
        "scope<{size}>"
    }
}
