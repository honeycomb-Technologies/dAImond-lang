module sort_bench

fn main() {
    let mut arr: List[int] = []

    -- Generate 2,000,000 random ints using LCG
    let mut seed = 42
    let mut i = 0
    while i < 2000000 {
        seed = (seed * 1103515245 + 12345) % 2147483648
        arr.push(seed)
        i = i + 1
    }

    -- Iterative quicksort using explicit stack
    let mut stack: List[int] = []
    stack.push(0)
    stack.push(1999999)

    while stack.len() > 0 {
        let high: int = stack.pop()
        let low: int = stack.pop()
        if low < high {
            -- Lomuto partition
            let pivot = arr[high]
            let mut ii = low - 1
            let mut j = low
            while j < high {
                if arr[j] <= pivot {
                    ii = ii + 1
                    let tmp = arr[ii]
                    arr[ii] = arr[j]
                    arr[j] = tmp
                }
                j = j + 1
            }
            ii = ii + 1
            let tmp = arr[ii]
            arr[ii] = arr[high]
            arr[high] = tmp
            let p = ii

            -- Push sub-problems
            if p - 1 > low {
                stack.push(low)
                stack.push(p - 1)
            }
            if p + 1 < high {
                stack.push(p + 1)
                stack.push(high)
            }
        }
    }

    -- Print first 5 sorted elements
    i = 0
    while i < 5 {
        println("sorted[" + int_to_string(i) + "]: " + int_to_string(arr[i]))
        i = i + 1
    }

    -- Compute checksum: sum of first 1000 elements
    let mut checksum = 0
    i = 0
    while i < 1000 {
        checksum = checksum + arr[i]
        i = i + 1
    }
    println("checksum: " + int_to_string(checksum))
}
