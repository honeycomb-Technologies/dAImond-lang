module error_type_nesting

-- A type nested one level deeper than the parser allows
-- (Written by a script: the lines are long because the limits are.)

struct Step { v: int }

impl Step {
    fn next(self) -> Step { return Step { v: self.v + 1 } }
}

fn main() {
    let lists: List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[List[int]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]] = []
    println(int_to_string(lists.len()))
}
