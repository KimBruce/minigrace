
dialect "StaticTyping"

type A = {
    n -> Number
}

type B = {
    m -> Number
}

type C = A|B

class c -> C {
   // method n -> Number{
   //     47    
   // }

   method m -> Number {
       7
   }
}

// class a -> A {
//     method n -> Number {
//         47
//     }
// }

print (c.n)