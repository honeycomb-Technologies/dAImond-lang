module test

struct Log { lines: int }

impl Log {
    fn write(self, text: string) with [Console] {
        println(text)
    }
    fn save(self) with [FileSystem] {
        file_write("/tmp/daimond_never_written", "x")
    }
    fn count(self) -> int {
        return self.lines
    }
}

fn shout(text: string) with [Console] {
    println(text)
}

fn store(text: string) with [FileSystem] {
    file_write("/tmp/daimond_never_written", text)
}

fn each(f: fn(string), text: string) {
    f(text)
}

-- Declares Console only: saving needs FileSystem, and so does the
-- function it names as a value
fn report(log: Log) with [Console] {
    log.write("fine")
    log.save()
    let n = log.count()
    each(shout, "fine")
    each(store, "not fine")
    "also fine".shout()
}

-- Declares nothing, and is held to nothing
fn free(log: Log) {
    log.save()
}

fn main() {
    let log = Log { lines: 0 }
    report(log)
    free(log)
}
