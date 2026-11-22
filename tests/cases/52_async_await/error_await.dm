module test

async fn add(a: int, b: int) -> int {
    return a + b
}

async fn name() -> string {
    return "n"
}

fn plain() -> int {
    return 1
}

struct Job { id: int }

impl Job {
    async fn run(self) -> int {
        return self.id
    }
}

fn main() -> int {
    let a = await add(1, 2)
    let b: string = await add(1, 2)
    let c = add(1, 2)
    let d = await plain()
    let e = await 5
    let f = await add(await add(1, 2), 3)
    let g: int = await name()
    let job = Job { id: 4 }
    let h = await job.run()
    let i = job.run()
    let k = await (add(1, 2))
    return a + f + h
}
