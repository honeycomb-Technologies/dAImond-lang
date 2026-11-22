module own_eq_method

-- A type with a method `eq` is compared by it, also as part of
-- another value; `!=` is `not eq` where there is no `ne`

struct Word {
    text: string
}

impl Word {
    fn eq(self, other: Word) -> bool {
        return string_to_lower(self.text) == string_to_lower(other.text)
    }
}

struct Version {
    major: int,
    build: int
}

impl Version {
    -- The build does not count
    fn eq(self, other: Version) -> bool {
        return self.major == other.major
    }
    fn ne(self, other: Version) -> bool {
        println("ne is called")
        return self.major != other.major
    }
}

enum Level {
    Low(int),
    High(int)
}

impl Level {
    -- Only the variant counts
    fn eq(self, other: Level) -> bool {
        match self {
            Level.Low(a) => {
                match other {
                    Level.Low(b) => { return true }
                    Level.High(b) => { return false }
                }
            }
            Level.High(a) => {
                match other {
                    Level.Low(b) => { return false }
                    Level.High(b) => { return true }
                }
            }
        }
        return false
    }
}

struct Title {
    word: Word,
    size: int
}

fn show(what: string, b: bool) {
    println(what + ": " + bool_to_string(b))
}

fn main() {
    let a = Word { text: "Hello" }
    let b = Word { text: "hello" }
    let c = Word { text: "help" }
    show("a == b", a == b)
    show("a == c", a == c)
    show("a != b", a != b)
    show("a != c", a != c)
    show("versions", Version { major: 1, build: 7 } == Version { major: 1, build: 9 })
    show("versions differ", Version { major: 1, build: 7 } != Version { major: 2, build: 7 })
    show("levels", Level.Low(1) == Level.Low(2))
    show("other levels", Level.Low(1) == Level.High(1))
    show("levels differ", Level.Low(1) != Level.High(1))
    -- As a field, a list element and an Option's value
    show("titles", Title { word: a, size: 1 } == Title { word: b, size: 1 })
    show("other titles", Title { word: a, size: 1 } == Title { word: c, size: 1 })
    show("lists", [a, c] == [b, c])
    let o: Option[Word] = Some(a)
    show("options", o == Some(b))
    show("contains", [c, b].contains(a))
}
