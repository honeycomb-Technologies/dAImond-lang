-- PROPOSAL -- NOT IMPLEMENTED, NOT A TEST
--
-- Eq/Ord-style traits: supertraits (trait Ord: Eq), trait methods
-- taking Self-typed arguments, @derive, and generic functions with
-- trait bounds (fn max[T: Ord]).
-- Moved from tests/generics.dm in Stage 5 Phase 3: this file does not
-- compile and is not run by any test suite. Plain traits whose
-- methods take only a &Self receiver work (see trait_tests in
-- stage0/tests/runner.zig).
--
-- What breaks today (Stage 0):
-- - `@derive(Eq)` is a parse error ("Expected declaration ... found: @").
-- - An impl of a trait method with an `other: Self` parameter emits
--   the C type name `dm_Self` unsubstituted in the method prototype
--   and in the dyn-trait vtable ("error: unknown type name 'dm_Self'"),
--   even when the impl spells the parameter with the concrete type.
-- - Supertrait declarations and [T: Ord] bounds parse, but there is
--   no instantiation path that could call max/min (see above), so
--   they are declaration-only today.
-- - Qualified match patterns `Ordering.Greater =>` are a parse
--   error ('=>' after match pattern, found '.'); the supported
--   pattern form is `Ordering::Greater =>`.

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
    match a.cmp(b) {
        Ordering.Greater => a,
        _ => b,
    }
}

fn min[T: Ord](a: T, b: T) -> T {
    match a.cmp(b) {
        Ordering.Less => a,
        _ => b,
    }
}
