dialect "StaticTyping"
import "minitest" as mt

method a -> Number {
   ProgrammingError.raise "oops"
   5
} 

mt.testSuite {
    mt.test "exception raised" by {
        mt.assert (a) shouldRaise (ProgrammingError)
    }
}