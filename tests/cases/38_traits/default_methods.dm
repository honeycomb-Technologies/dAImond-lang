module default_methods

-- A trait's method with a body is the method of every type that
-- implements the trait and does not define it

trait Greeter {
    fn name(self) -> string

    fn greet(self) -> string {
        return "hello, " + self.name()
    }

    fn greet_twice(self, between: string) -> string {
        return self.greet() + between + self.greet()
    }

    fn letters(self) -> int {
        let n = self.name()
        return len(n)
    }
}

struct Person {
    first: string
}

struct Robot {
    id: int
}

enum Pet {
    Cat,
    Dog(string)
}

impl Greeter for Person {
    fn name(self) -> string {
        return self.first
    }
}

impl Greeter for Robot {
    fn name(self) -> string {
        return "unit " + int_to_string(self.id)
    }

    -- Its own greet takes the place of the trait's
    fn greet(self) -> string {
        return "BEEP " + self.name()
    }
}

impl Greeter for Pet {
    fn name(self) -> string {
        match self {
            Pet.Cat => { return "cat" }
            Pet.Dog(n) => { return n }
        }
    }
}

fn welcome[T: Greeter](x: T) -> string {
    return "[" + x.greet() + "]"
}

fn main() {
    let p = Person { first: "Ada" }
    let r = Robot { id: 7 }
    println(p.greet())
    println(r.greet())
    println(p.greet_twice(" / "))
    println(r.greet_twice(" / "))
    println(int_to_string(p.letters()))
    println(int_to_string(r.letters()))
    println(Pet.Cat.greet())
    println(Pet.Dog("rex").greet_twice(" & "))
    println(welcome(p))
    println(welcome(r))
    println(welcome(Pet.Dog("fido")))
}
