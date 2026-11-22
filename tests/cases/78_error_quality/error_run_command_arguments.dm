module main

fn main() {
    let a = run_command("ls")
    let b = run_command(["ls"], "out")
    let c = run_command_io(["ls"], "", "")
    let d = run_command_io(["ls"], "", "", 3)
}
