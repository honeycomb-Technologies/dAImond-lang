function main() {
    const m = new Map();

    // Insert 1,000,000 entries
    for (let i = 0; i < 1000000; i++) {
        m.set("key_" + i, i);
    }
    console.log("inserted: 1000000");

    // Lookup all entries, sum values
    let sum = 0;
    for (let i = 0; i < 1000000; i++) {
        sum += m.get("key_" + i);
    }
    console.log("lookup sum: " + sum);

    // Delete entries where i % 3 == 0
    for (let i = 0; i < 1000000; i++) {
        if (i % 3 === 0) {
            m.delete("key_" + i);
        }
    }
    console.log("final: " + m.size);
}

main();
