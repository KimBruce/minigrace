dialect "StaticTyping"


type Person = {
    greet (name: String) -> Done 
}

type Superman = {
    greet (name:String) -> Done
    fly -> Done
    giveBirth -> Superman
}



class personNamed (nameVal: String)  -> Person {
    method greet (name: String) -> Done {
        print ("Hello there, {name}. My name is {nameVal}.")
    }
}

class superNamed (nameVal: String)  -> Superman {
    method greet (name: String) -> Done {
        print ("Hello there, {name}. My name is {nameVal}.")
    }

    method fly -> Done {
        print ("Flying")
    }

    method giveBirth -> Superman {
        self
    }
}




def firstPerson: Person= personNamed ("Shezad") 

def secondPerson: Person = personNamed("Charles") 

def superMan: Superman = superNamed ("Iren")

firstPerson.greet("Pauvle")
superMan.fly
superMan.giveBirth