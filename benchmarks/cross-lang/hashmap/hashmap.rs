use std::collections::HashMap;

fn main() {
    let mut m: HashMap<String, i64> = HashMap::new();

    // Insert 1,000,000 entries
    for i in 0..1_000_000i64 {
        m.insert(format!("key_{}", i), i);
    }
    println!("inserted: 1000000");

    // Lookup all entries, sum values
    let mut sum: i64 = 0;
    for i in 0..1_000_000i64 {
        sum += m[&format!("key_{}", i)];
    }
    println!("lookup sum: {}", sum);

    // Delete entries where i % 3 == 0
    for i in 0..1_000_000i64 {
        if i % 3 == 0 {
            m.remove(&format!("key_{}", i));
        }
    }
    println!("final: {}", m.len());
}
