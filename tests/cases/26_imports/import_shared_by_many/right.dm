module right

import core
import base
import left

fn right_value() -> int {
    return left_value() + core_value()
}
