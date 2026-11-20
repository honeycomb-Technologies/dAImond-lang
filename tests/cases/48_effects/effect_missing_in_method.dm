module test

struct Logger {
    prefix: string
}

impl Logger {
    fn save(self, path: string) -> int with [Console] {
        if len(path) > 0 {
            file_write(path, self.prefix)
        }
        return 0
    }
}

fn main() -> int {
    let l = Logger { prefix: "log" }
    return l.save("")
}
