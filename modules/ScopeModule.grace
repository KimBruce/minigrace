#pragma ExtendedLineups
#pragma noTypeChecks
dialect "none"
import "standardGrace" as sg

import "ast" as ast
import "xmodule" as xmodule
import "io" as io
import "SharedTypes" as share

inherit sg.methods

type ObjectType = share.ObjectType
type MethodType = share.MethodType
type GenericType = share.GenericType

// The cached public type assignments.
def cache: Dictionary is readable = emptyDictionary
// cache holding confidential types for inheritance
def allCache: Dictionary is readable = emptyDictionary

// Holds dictionaries for names in scope
// Will be used for defs/vars, methods, and types
type StackOfKind⟦V⟧ = {
    stack → List⟦Dictionary⟧
    // push <name,value> into current level of stack
    at (name : String) put (value:V) → Done

    // add <name,value> to base level of stack
    addToTopAt(name : String) put (value : V) → Done

    // Find name in stack of current scopes & return its value
    // If not there perform action in bl. Starts looking from lowest level
    findFromBottom (name : String) butIfMissing (bl: Function0⟦V⟧) → V

    // Find name in stack of current scopes & return its value
    // If not there perform action in bl. Starts looking from global level
    findFromTop (name : String) butIfMissing (bl: Function0⟦V⟧) → V

    // Find name in the single highest level of scope & return its value
    // If not there perform action in bl. Should only be used to find types
    findType (name : String) butIfMissing (bl: Function0⟦V⟧) → V


}

// class creating stack of given kind (e.g., types) holding values of type V
class stackOfKind⟦V⟧(kind : String) → StackOfKind⟦V⟧ is confidential {
    // represented as stack of dictionaries
    def stack: List⟦Dictionary⟧ is public = list[emptyDictionary]

    // add <name,value> to current scope
    method at (name : String) put (value:V) → Done {
        stack.last.at(name) put(value)
    }

    //adds to the first layer of the scope
    method addToTopAt(name : String) put (value:V) → Done {
        stack.first.at(name) put(value)
    }

    // Find name in stack of current scopes & return its value
    // If not there perform action in bl
    method findFromBottom (name : String) butIfMissing (bl: Function0⟦V⟧) → V {
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

    // Find name in stack of current scopes & return its value
    // If not there perform action in bl
    method findFromTop (name : String) butIfMissing (bl: Function0⟦V⟧) → V {
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

    //specifically used to find types saved in the highest level of the scope
    method findType (name : String) butIfMissing (bl: Function0⟦V⟧) → V {
        var found: Boolean := true
        def val = stack.at(1).at(name) ifAbsent {
            found := false
        }
        if(found) then {
            return val
        } else {
            return bl.apply
        }
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
