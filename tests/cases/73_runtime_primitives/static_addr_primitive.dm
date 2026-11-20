module test

fn counter() -> int {
    unsafe {
        return static_addr("counter", 8)
    }
}

fn next() -> int {
    unsafe {
        let n = load_i64(counter()) + 1
        store_i64(counter(), n)
        return n
    }
}

fn main() {
    unsafe {
        -- Storage starts as zeros and keeps what is stored in it
        println(int_to_string(load_i64(counter())))
        let a = next()
        let b = next()
        let c = next()
        println(int_to_string(a) + " " + int_to_string(b) + " " + int_to_string(c))
        -- One name is one address; another name is other storage
        let table = static_addr("table", 100)
        println(bool_to_string(table == static_addr("table", 100)) + " " + bool_to_string(table != counter()))
        -- All 100 bytes are there, and the address suits any load
        let mut i = 0
        let mut zeros = 0
        while i < 100 {
            if load_u8(table + i) == 0 { zeros = zeros + 1 }
            store_u8(table + i, i + 1)
            i = i + 1
        }
        println(int_to_string(zeros) + " " + int_to_string(load_u8(table + 99)) + " " + int_to_string(table % 8))
        println(int_to_string(load_i64(counter())))
    }
}