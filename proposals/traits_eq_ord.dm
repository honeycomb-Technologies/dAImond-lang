-- PROPOSAL -- NOT IMPLEMENTED, NOT A TEST
--
-- Supertraits (`trait Ord: Eq`) and `@derive`.
-- The rest of what this file once proposed works: a trait's method
-- that takes `other: Self`, generic functions with trait bounds
-- (`fn max[T: Ord]`, tests/cases/94_trait_bounds) and patterns such as
-- `Ordering.Greater =>`. `==` compares structs part by part without a
-- trait (tests/cases/92_equality).
--
-- What the compiler says today: `trait Ord: Eq` is a syntax error
-- ("expected '{' but got ':'"), and so is `@derive(Eq)` ("expected a
-- declaration but got '@'").

module proposals.traits_eq_ord

-- Trait example
trait Eq {
    fn eq(self, other: Self) -> bool
}

trait Ord: Eq {
    fn cmp(self, other: Self) -> Ordering
}

enum Ordering {
    Less,
    Equal,
    Greater,
}

-- Implement traits for Point
@derive(Eq)
struct Point3D {
    x: float,
    y: float,
    z: float,
}

-- Generic function with trait bounds
fn max[T: Ord](a: T, b: T) -> T {
    return match a.cmp(b) {
        Ordering.Greater => a,
        _ => b,
    }
}

fn min[T: Ord](a: T, b: T) -> T {
    return match a.cmp(b) {
        Ordering.Less => a,
        _ => b,
    }
}
