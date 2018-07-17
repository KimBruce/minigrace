dialect "gradualTypesND"
import "t308_complicatedImportee_test" as im

type Test = im.ComplicatedType
type Test2 = im.ComplicatedType | Number | im.A
print "test succeeded"
