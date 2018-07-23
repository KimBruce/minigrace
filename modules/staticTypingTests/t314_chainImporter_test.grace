dialect "StaticTyping"
import "t308_complicatedImportee_test" as im1
import "t313_chainImportee_test" as im2
//im2 imports im1, so we can test whether chained imports are passed correctly

//These 2 types should be equivalent
type T = im1.A & im1.B
type U = im2.AB

class t -> T {
    method m -> Number { 47 }
    method p -> String { "Hello" }
}
class u -> U {
    method m -> Number { 48 }
    method p -> String { "World" }
}

//If both of these defdecs pass type-checking, then T is a subtype of U and
//U is a subtype of T, so they are equal and imports pass their types correctly.
def x : U = t
def y : T = u
print ("{x.p} {y.p}")
