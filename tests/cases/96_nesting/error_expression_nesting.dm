module error_expression_nesting

-- An expression nested one level deeper than the parser allows
-- (Written by a script: the lines are long because the limits are.)

struct Step { v: int }

impl Step {
    fn next(self) -> Step { return Step { v: self.v + 1 } }
}

fn main() {
    let deep = ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((1))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    println(int_to_string(deep))
}
