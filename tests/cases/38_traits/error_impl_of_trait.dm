module error_impl_of_trait

-- An impl of a trait has the trait's methods, as the trait gives them

trait Greeter {
    fn name(self) -> string
    fn rank(self, scale: int) -> int
    fn rename(mut self, to: string)
    fn greet(self) -> string {
        return "hello, " + self.name()
    }
}

struct Person {
    first: string
}

struct Robot {
    id: int
}

impl Greeter for Person {
    fn rank(self, scale: string) -> string {
        return scale
    }
    fn rename(self, to: string) {
    }
}

impl Greeter for Robot {
    fn name(self) -> string {
        return "robot"
    }
    fn rank(self) -> int {
        return self.id
    }
    fn rename(mut self, to: string) {
    }
}

impl Missing for Person {
    fn other(self) -> int {
        return 1
    }
}

fn takes(g: Greeter) -> string {
    return "g"
}

fn main() {
    let p = Person { first: "a" }
    println(p.greet())
}
