dialect "StaticTyping"
import "t335A_basicImportee_test" as im

//This test does not currently work because variant types
//are not supported in imports.

//Recreate the first and second halves of the VariantType
type FirstHalf  = { firstMeth -> Number }
type SecondHalf = { secondMeth -> Number }

class firstClass  -> FirstHalf {
    method firstMeth  -> Number { 47 }
}
class secondClass -> SecondHalf{
    method secondMeth -> Number{ 48 }
}

def test1 : im.VariantType = firstClass
def test2 : im.VariantType = secondClass
print "test succeeded"
