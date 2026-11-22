module options_results_lists

struct Point {
    x: int,
    y: int
}

enum Kind {
    Small,
    Big(int)
}

fn show(what: string, b: bool) {
    println(what + ": " + bool_to_string(b))
}

fn find(n: int) -> Option[int] {
    if n > 0 { return Some(n) }
    return None
}

fn parse(s: string) -> Result[int, string] {
    if s == "" { return Err("empty") }
    return Ok(len(s))
}

fn main() {
    -- Options
    let o: Option[int] = Some(3)
    let none: Option[int] = None
    show("Some(3) == Some(3)", o == Some(3))
    show("Some(3) == Some(4)", o == Some(4))
    show("Some(3) == None", o == None)
    show("None == None", none == None)
    show("None on the left", None == none)
    show("o != None", o != None)
    show("calls", find(2) == find(2))
    show("calls that differ", find(2) == find(0))
    let name: Option[string] = Some("ab")
    show("strings", name == Some("a" + "b"))
    let at: Option[Point] = Some(Point { x: 1, y: 2 })
    show("structs", at == Some(Point { x: 1, y: 2 }))
    show("other structs", at == Some(Point { x: 2, y: 1 }))

    -- Results
    show("Ok(2) == Ok(2)", parse("ab") == parse("cd"))
    show("Ok(2) == Ok(3)", parse("ab") == parse("abc"))
    show("Ok == Err", parse("ab") == parse(""))
    show("Err == Err", parse("") == parse(""))
    let r: Result[int, string] = Err("empty")
    show("Err and a variable", parse("") == r)
    show("!=", parse("a") != r)

    -- Lists
    let xs = [1, 2, 3]
    let mut ys: List[int] = []
    ys.push(1)
    ys.push(2)
    show("a shorter list", xs == ys)
    ys.push(3)
    show("equal lists", xs == ys)
    show("a list and a literal", xs == [1, 2, 3])
    show("another element", xs == [1, 2, 4])
    show("an empty list", xs == [])
    let empty: List[int] = []
    show("two empty lists", empty == [])
    show("!=", xs != [3, 2, 1])
    let words = ["a", "bc"]
    show("strings", words == ["a", "b" + "c"])
    let grid = [[1, 2], [3]]
    show("lists of lists", grid == [[1, 2], [3]])
    show("other lists of lists", grid == [[1, 2], [4]])
    let ps = [Point { x: 1, y: 2 }, Point { x: 3, y: 4 }]
    show("lists of structs", ps == [Point { x: 1, y: 2 }, Point { x: 3, y: 4 }])
    let maybe = [Some(1), None]
    show("lists of options", maybe == [Some(1), None])
    show("other lists of options", maybe == [None, Some(1)])

    -- contains looks for an equal element
    show("contains a struct", ps.contains(Point { x: 3, y: 4 }))
    show("contains another struct", ps.contains(Point { x: 4, y: 3 }))
    let kinds = [Kind.Small, Kind.Big(2)]
    show("contains a variant", kinds.contains(Kind.Big(2)))
    show("contains another variant", kinds.contains(Kind.Big(3)))
    show("contains a list", grid.contains([3]))
    show("contains an option", maybe.contains(None))
    show("contains a string", words.contains("b" + "c"))
    show("contains an int", xs.contains(4))
}
