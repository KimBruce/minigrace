dialect "minitest"
import "mirrors" as mi
import "option" as option
def mm = mi.reflect(option)
print "option's methods are: "
for (mm.methodNames.asList.sort) do { each ->
    print "    {each}"
}

testSuiteNamed "import option" with {
    test "some 3" by {
        def q = option.some(3)
        assert (q.value) shouldBe 3
    }
    test "none" by {
        def w = option.none
        assert {w.value} shouldRaise (ProgrammingError)
    }
    test "do" by {
        def q = option.some 3
        var counter := 0
        q.do { each ->
            assert (each) shouldBe 3
            counter := counter + 1
        }
        assert (counter) shouldBe 1
    }
}

