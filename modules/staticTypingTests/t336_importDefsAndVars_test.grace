dialect "StaticTyping"
import "t335A_basicImportee_test" as im

if((im.myDef ++ im.myVar) == "MiniGrace") then {
    print "test succeeded"
} else {
    print "test failed"
}
