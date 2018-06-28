dialect "gradualTypesND"
import "t335A_basicImportee_test" as im

//This test currently fails as & is not supported in imports.

//Recreate the first and second halves of the AmpersandType
type FirstHalf  = { firstMeth -> Number }
type SecondHalf = { secondMeth -> Number }

class ampersandClass -> FirstHalf & SecondHalf {
    method firstMeth  -> Number { 47 }
    method secondMeth -> Number { 48 }
}

def test : im.AmpersandType = ampersandClass

//47 then 48, if methods from imported types aren't saved then prints would
//generate error
print (test.firstMeth)
print (test.secondMeth)
