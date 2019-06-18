dialect "StaticTyping"

method tryExceptional -> String | Done {
    try {
        ProgrammingError.raise "an error occurred"
        "hello"
    } catch {e: ProgrammingError ->
        print (4)
    }
}

print (tryExceptional)