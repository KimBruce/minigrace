dialect "StaticTyping"

type A = {
    n -> Number
}

type B = {
    m -> A
}


class alpha -> A {
   method n -> Number{
       47    
   }
}


class beta -> B {
   def pavle:A = alpha
   method m -> A {
       pavle
   }
}

def objectAlpha:A = alpha
def objectBeta:B = beta

print ("LOOOOOOOOOOOOOOK HEEEEEEEEEEREEEEEEEEEEEE {objectAlpha.n}")