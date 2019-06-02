type T⟦V⟧ = {
   m(v:V) -> Done
}
//class s {
class t⟦V⟧ → T⟦V⟧ {
   method m(v:V) -> Done {
       print(v)
   }
}

def x : T⟦Number⟧ = t⟦Number⟧
x.m(3)
