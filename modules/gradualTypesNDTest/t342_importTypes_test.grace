dialect "gradualTypesND"
import "t335A_basicImportee_test" as im

class m -> im.MyType {
    method a -> Number{47}
}

var x : Number := m.a
print "test succeeded"
