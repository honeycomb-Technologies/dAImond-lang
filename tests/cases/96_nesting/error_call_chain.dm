module error_call_chain

-- One more call in a row than the parser allows
-- (Written by a script: the lines are long because the limits are.)

struct Step { v: int }

impl Step {
    fn next(self) -> Step { return Step { v: self.v + 1 } }
}

fn main() {
    let start = Step { v: 0 }
    let last = start.next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next().next()
    println(int_to_string(last.v))
}
