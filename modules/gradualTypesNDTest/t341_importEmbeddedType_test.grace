dialect "gradualTypesND"
import "t335B_embeddedImportee_test" as im

class embTypeConstructor -> im.EmbType.Inner {
    method a -> Number { 47 }
}

if (embTypeConstructor.a == 47) then {
    print "test succeeded"
} else {
    print "test failed"
}
