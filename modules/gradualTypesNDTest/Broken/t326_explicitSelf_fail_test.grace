dialect "gradualTypesND"
//This test is broken. Only works if self.T is changed to T.

type T = Number

def x: self.T = "Hello World!"
print "{x}"
print "Test should have failed"
