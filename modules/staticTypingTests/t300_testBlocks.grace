dialect "StaticTyping"

// Should raise an exception and it does
// def testBlock2: Object = object {
//    def value: Number = 5

//    var result: String := match(value)
//        case { "Hello World" -> "Hello World" }
//        case { s: String -> "" }
// }

// Should raise an exception and it does
// def testBlock3: Object = object {
//    def value: String = "Hello World"

//    var result: String := match(value)
//        case { "Hello", "World" -> "Hello World" }
//        case { s: String -> "" }
// }

// Should not raise an exception and it does not
// def testBlock4: Object = object {
//    def value: String | Number = "Hello World"

//    var result: String := match(value)
//        case { s: String -> "" }
//        case { n: Number -> "" }
// }

// Should raise an exception but DOES NOT
// def testBlock5: Object = object {
//    def value: String | Number = "Hello World"

//    var result: String := match(value)
//        case { s: String -> "" }
//        case { b: Boolean -> "" }
// }

// Should not raise an exception
// def testBlock6: Object = object {
//     def value: String = "Hello World"
//     var result: String | Boolean := match(value)
//         case { "Hello World" -> "Hello World" }
//         case { s: String -> true }
// }

// Should raise an exception
def testBlock7: Object = object {
   def value: String = "Hello World"
   var result: String | Boolean := match(value)
       case { "Hello World" -> "Hello World" }
       case { s: String -> 5 }
}