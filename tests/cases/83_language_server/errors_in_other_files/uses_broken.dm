module uses_broken

import broken

fn main() -> int {
    return broken_value() + missing_here
}
