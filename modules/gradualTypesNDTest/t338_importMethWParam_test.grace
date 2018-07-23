dialect "StaticTyping"
import "t335A_basicImportee_test" as im

if (im.myMethWithParam(47) == 47) then {
    print "test succeeded"
} else {
    print "test failed"
}
