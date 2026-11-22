module error_trait_bound_syntax

trait Show {
    fn show(self) -> string
}

fn shown[T: ](x: T) -> string {
    return "x"
}

fn main() {
    println("unreachable")
}
