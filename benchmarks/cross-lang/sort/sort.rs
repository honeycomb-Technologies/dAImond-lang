static mut SEED: i64 = 42;

fn next_rand() -> i64 {
    unsafe {
        SEED = (SEED.wrapping_mul(1103515245).wrapping_add(12345)) & 0x7FFFFFFF;
        SEED
    }
}

fn partition(arr: &mut [i64], low: usize, high: usize) -> usize {
    let pivot = arr[high];
    let mut i = low as isize - 1;
    for j in low..high {
        if arr[j] <= pivot {
            i += 1;
            arr.swap(i as usize, j);
        }
    }
    arr.swap((i + 1) as usize, high);
    (i + 1) as usize
}

fn quicksort(arr: &mut [i64], low_in: isize, high_in: isize) {
    if low_in < high_in {
        let p = partition(arr, low_in as usize, high_in as usize);
        quicksort(arr, low_in, p as isize - 1);
        quicksort(arr, p as isize + 1, high_in);
    }
}

fn main() {
    let n: usize = 2_000_000;
    let mut arr: Vec<i64> = Vec::with_capacity(n);

    // Generate random numbers
    for _ in 0..n {
        arr.push(next_rand());
    }

    // Sort
    let len = arr.len() as isize;
    quicksort(&mut arr, 0, len - 1);

    // Print first 5 sorted elements
    for i in 0..5 {
        println!("sorted[{}]: {}", i, arr[i]);
    }

    // Compute checksum: sum of first 1000 elements
    let mut checksum: i64 = 0;
    for i in 0..1000 {
        checksum += arr[i];
    }
    println!("checksum: {}", checksum);
}
