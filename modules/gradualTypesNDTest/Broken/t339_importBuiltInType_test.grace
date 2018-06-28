dialect "gradualTypesND"
import "t335A_basicImportee_test" as im

//This test currently fails, as we cannot import types that are not
//strictly type literals.

def x : im.NumberCopy = 47
print "test succeeded"
