dialect "gradualTypesND"
//This test is broken. It only works if we write T instead of self.T 
type T = Number

def x: self.T = 47
print "{x}"
print "Test Succeeded"
