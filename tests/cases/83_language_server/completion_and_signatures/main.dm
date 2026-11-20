module main

struct Account {
    owner: string,
    balance: int
}

impl Account {
    fn deposit(self, amount: int, note: string) -> Account {
        return Account { owner: self.owner, balance: self.balance + amount }
    }
}

fn transfer(from: Account, to: Account, amount: int) -> int {
    let fee = 1
    let fee = 2
    let moved = amount - fee
    return moved
}

fn main() -> int {
    let acct = Account { owner: "ann", balance: 5 }
    let other = acct.deposit(3, "gift, (wrapped)")
    println(int_to_string(transfer(acct, other, 2)))
    println(substr("hello", 1, 3))
    return 0
}
