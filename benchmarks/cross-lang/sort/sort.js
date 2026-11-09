let seed = 42;

function nextRand() {
    // Split multiplication to avoid exceeding Number.MAX_SAFE_INTEGER
    // seed * 1103515245 = seed * (1103515245)
    // Split into: seed * 1103515245 mod 2^31
    // Use: (a * b) mod m, with a,b potentially large
    // Since seed < 2^31 and multiplier < 2^31, product < 2^62, which overflows.
    // Use BigInt for the multiplication step only.
    seed = Number((BigInt(seed) * 1103515245n + 12345n) & 0x7FFFFFFFn);
    return seed;
}

function partition(arr, low, high) {
    const pivot = arr[high];
    let i = low - 1;
    for (let j = low; j < high; j++) {
        if (arr[j] <= pivot) {
            i++;
            const tmp = arr[i];
            arr[i] = arr[j];
            arr[j] = tmp;
        }
    }
    const tmp = arr[i + 1];
    arr[i + 1] = arr[high];
    arr[high] = tmp;
    return i + 1;
}

function quicksort(arr, low, high) {
    // Iterative quicksort to avoid stack overflow with 2M elements
    const stack = [];
    stack.push(low);
    stack.push(high);

    while (stack.length > 0) {
        const hi = stack.pop();
        const lo = stack.pop();
        if (lo < hi) {
            const p = partition(arr, lo, hi);
            stack.push(lo);
            stack.push(p - 1);
            stack.push(p + 1);
            stack.push(hi);
        }
    }
}

function main() {
    const n = 2000000;
    const arr = new Array(n);

    // Generate random numbers
    for (let i = 0; i < n; i++) {
        arr[i] = nextRand();
    }

    // Sort
    quicksort(arr, 0, n - 1);

    // Print first 5 sorted elements
    for (let i = 0; i < 5; i++) {
        console.log(`sorted[${i}]: ${arr[i]}`);
    }

    // Compute checksum: sum of first 1000 elements
    let checksum = 0;
    for (let i = 0; i < 1000; i++) {
        checksum += arr[i];
    }
    console.log(`checksum: ${checksum}`);
}

main();
