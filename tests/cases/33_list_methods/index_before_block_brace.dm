module test
fn main() {
    let mut words: List[string] = []
    words.push("b")
    words.push("a")
    words.push("b")
    let mut nums: List[int] = []
    nums.push(3)
    nums.push(1)
    let mut eqs = 0
    let mut lts = 0
    let mut i = 0
    while i < 3 {
        let mut j = 0
        while j < 3 {
            if words[i] == words[j] { eqs = eqs + 1 }
            if words[i] < words[j] {
                lts = lts + 1
            }
            j = j + 1
        }
        i = i + 1
    }
    let k = 1
    if nums[0] > nums[k] { println("gt") }
    while nums[k] < nums[0] { nums[k] = nums[k] + 1 }
    println(int_to_string(eqs) + " " + int_to_string(lts) + " " + int_to_string(nums[k]))
}