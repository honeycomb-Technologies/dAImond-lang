module error_branch_literal_too_wide

-- A literal in one branch takes the type of the other branch, and
-- must fit it

fn opt(n: i32) -> Option[i32] {
    if n != 0 { return Some(n) }
    return None
}

fn main() -> int {
    let x: i32 = 7
    let yes = x > 0
    let a = if yes { x } else { 3000000000 }
    let b = match opt(x) { Some(v) => v, None => -3000000000 }
    let small: i8 = 1
    let c = match x { 0 => small, _ => 200 }
    println(a as int)
    println(b as int)
    println(c as int)
    return 0
}
