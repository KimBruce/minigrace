dialect "StaticTyping"

import "identImportee_test" as im1
import "t335A_basicImportee_test" as im2
import "t308_complicatedImportee_test" as im3

type MyType = im2.MyType & (im1.U | im3.A)
print "test succeeded"
