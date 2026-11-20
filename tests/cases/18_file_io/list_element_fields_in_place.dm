module test
struct Block { label: string, insts: List[int], done: bool, n: int }
struct Func { name: string, blocks: List[Block] }
fn main() {
    let mut f = Func { name: "f", blocks: [] }
    f.blocks.push(Block { label: "entry", insts: [], done: false, n: 0 })
    f.blocks.push(Block { label: "exit", insts: [], done: false, n: 0 })
    let i = 1
    f.blocks[i].insts.push(5)
    f.blocks[i].insts.push(6)
    f.blocks[i].done = true
    f.blocks[0].n = f.blocks[0].n + 3
    f.blocks[0].label = "start"
    let mut grid: List[List[int]] = []
    let mut row: List[int] = []
    row.push(1)
    grid.push(row)
    grid[0].push(2)
    grid[0][1] = 9
    println(f.blocks[0].label)
    println(int_to_string(f.blocks[0].n))
    println(int_to_string(f.blocks[1].insts.len()))
    println(int_to_string(f.blocks[1].insts[1]))
    println(bool_to_string(f.blocks[1].done))
    println(int_to_string(grid[0].len()))
    println(int_to_string(grid[0][1]))
}