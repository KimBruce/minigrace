dialect "gradualTypesND"

method test_concat → String  {

    ("abc" ++ "def")
}

method test_compare_identity_concat  → Boolean  {
    def s: String = "abcd" ++ "ef"
    (s.compare(s) == 0)
}

method test_compare_same_concat  → Boolean{
    def s: String = "abcd" ++ "ef"
    def t: String = "abcdef"
    (s.compare(t) == 0)
}

method test_compare_identity_flat  → Boolean  {
    def s: String = "rstuvwxyz"
    (s.compare(s) == 0)
}

method test_comparelessThan_concat_first  → Boolean  {
    def s: String = "abcd" ++ "ef"
    def t: String ="bcdef"
    (s.compare(t) == -1)
}

method test_compareLessThan_concat_second  → Boolean {
    def s: String ="abcd" ++ "ea"
    def t: String ="abcd" ++  "ef"
    (s.compare(t) == -1)
}

method test_compareLessThan_concat_shorter  → Boolean {
    def s: String ="abcd" ++ "e"
    def t: String ="abcd" ++ "ef"
    (s.compare(t) == -1)
}

method test_compareLessThan_flat  → Boolean {
    def s: String ="abcdea"
    def t: String ="abcdef"
    (s.compare(t) == -1)
}

method test_comparegreaterThan_concat_first  → Boolean {
    def s: String ="abcd" ++ "ef"
    def t: String ="aacd"
    (s.compare(t) == 1)
}

method test_compareGreaterThan_concat_second  → Boolean {
    def s: String ="abcd" ++ "ef"
    def t: String ="abcd" ++ "ea"
    (s.compare(t) == 1)
}

method test_compareGreaterThan_concat_shorter  → Boolean {
    def s: String ="abcd" ++ "x"
    def t: String ="abcd" ++ "ef"
    (s.compare(t) == 1)
}

method test_compareGreaterThan_flat  → Boolean {
    def s: String ="abcdef"
    def t: String ="abcdea"
    (s.compare(t) == 1)
}

method test_lessThan_concat_first  → Boolean {
    def s: String ="abcd" ++ "ef"
    def t: String ="bcdef"
    (s < t)
}

method test_lessThan_concat_second  → Boolean {
    def s: String ="abcd" ++ "ea"
    def t: String ="abcd" ++ "ef"
    (s < t)
}

method test_lessThan_concat_shorter  → Boolean {
    def s: String ="abcd" ++ "e"
    def t: String ="abcd" ++ "ef"
    (s < t)
}

method test_lessThan_flat  → Boolean {
    def s: String ="abcdea"
    def t: String ="abcdef"
    (s < t)
}

method test_greaterThan_concat_first  → Boolean {
    def s: String ="abcd" ++ "ef"
    def t: String ="aacd"
    (s > t)
}

method test_greaterThan_concat_second  → Boolean {
    def s: String ="abcd" ++ "ef"
    def t: String ="abcd" ++ "ea"
    (s > t)
}

method test_greaterThan_concat_shorter  → Boolean {
    def s: String ="abcd" ++ "x"
    def t: String ="abcd" ++ "ef"
    (s > t)
}

method test_greaterThan_flat  → Boolean {
    def s: String ="abcdef"
    def t: String ="abcdea"
    (s > t)
}

method test_lessThanOrEqual_concat_first  → Boolean {
    def s: String ="abcd" ++ "ef"
    def t: String ="bcdef"
    (s ≤ t) &&(s <= t)
}

method test_lessThanOrEqual_concat_second  → Boolean {
    def s: String ="abcd" ++ "ea"
    def t: String ="abcd" ++ "ef"
    (s ≤ t) &&(s <= t)
}

method test_lessThanOrEqual_concat_shorter  → Boolean {
    def s: String ="abcd" ++ "e"
    def t: String ="abcd" ++ "ef"
    (s ≤ t) &&(s <= t)
}

method test_lessThanOrEqual_flat  → Boolean {
    def s: String ="abcdea"
    def t: String ="abcdef"
    (s ≤ t) &&(s <= t)
}

method test_lessThanOrEqual_equal_flat  → Boolean {
    def s: String ="abcdef"
    def t: String ="abcdef"
    (s ≤ t) && (s <= t)
}

method test_greaterThanOrEqual_concat_first  → Boolean {
    def s: String ="abcd" ++ "ef"
    def t: String ="aacd"
    (s >= t) && (s ≥ t)
}

method test_greaterThanOrEqual_concat_second  → Boolean {
    def s: String ="abcd" ++ "ef"
    def t: String ="abcd" ++ "ea"
    (s >= t) && (s ≥ t)
}

method test_greaterThanOrEqual_concat_shorter  → Boolean {
    def s: String ="abcd" ++ "x"
    def t: String ="abcd" ++ "ef"
    (s >= t) && (s ≥ t)
}

method test_greaterThanOrEqual_flat  → Boolean {
    def s: String ="abcdef"
    def t: String ="abcdea"
    (s >= t) &&  (s ≥ t)
}

method test_greaterThanOrEqual_equal_flat  → Boolean {
    def s: String ="abcdef"
    def t: String ="abcdef"
    (s >= t) &&(s ≥ t)
}

method test_equal_equal_flat  → Boolean {
    def s: String ="abcdef"
    def t: String ="abcdef"
    (s == t) &&((s ≥ t) &&(s ≤ t))
}

test_greaterThan_concat_first

print "test succeeded"
