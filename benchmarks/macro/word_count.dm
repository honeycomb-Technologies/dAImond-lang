-- Macro Benchmark: Word Count
-- Counts word frequencies in a large generated text.
-- Tests string operations, map operations, and iteration.

module word_count_bench

fn generate_text() -> string {
    let words = "the quick brown fox jumps over the lazy dog and the cat sat on the mat while the bird flew high in the sky above the green forest where the river flows gently through the meadow near the old stone bridge that connects the two villages"
    let mut text = ""
    let mut i = 0
    while i < 1000 {
        if i > 0 {
            text = text + " "
        }
        text = text + words
        i = i + 1
    }
    return text
}

fn add_word(mut counts: Map[string, int], word: string) -> Map[string, int] {
    let mut val = 1
    if counts.contains(word) {
        val = counts.get(word) + 1
    }
    counts.insert(word, val)
    return counts
}

fn count_words(text: string) -> int {
    let mut counts: Map[string, int] = Map_new()
    let mut word = ""
    let mut total_words = 0

    let mut i = 0
    while i < len(text) {
        let ch = char_at(text, i)
        if ch == " " or ch == "\n" or ch == "\t" {
            if len(word) > 0 {
                counts = add_word(counts, word)
                total_words = total_words + 1
                word = ""
            }
        } else {
            word = word + ch
        }
        i = i + 1
    }
    -- Handle last word
    if len(word) > 0 {
        counts = add_word(counts, word)
        total_words = total_words + 1
    }

    println("unique words: " + int_to_string(counts.len()))
    println("total words: " + int_to_string(total_words))

    -- Find most frequent
    let keys = counts.keys()
    let mut max_count = 0
    let mut max_word = ""
    let mut k = 0
    while k < keys.len() {
        let w = keys[k]
        let c = counts.get(w)
        if c > max_count {
            max_count = c
            max_word = w
        }
        k = k + 1
    }
    println("most frequent: \"" + max_word + "\" (" + int_to_string(max_count) + " times)")

    return total_words
}

fn main() {
    let text = generate_text()
    println("text length: " + int_to_string(len(text)))
    let total = count_words(text)
    println("done, total = " + int_to_string(total))
}
