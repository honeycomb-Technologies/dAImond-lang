module test

trait Describable {
    fn describe(self: &Self) -> string
}

struct Dog {
    name: string
}

impl Describable for Dog {
    fn describe(self: &Self) -> string {
        return "Dog: " + self.name
    }
}

fn main() {
    let d = Dog { name: "Rex" }
    let obj: dyn Describable = d
    println(obj.describe())
}