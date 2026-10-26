#!/usr/bin/env python3
"""
Fix IR generation to track completed blocks.
Adds g.completed_blocks.push(block_name) after specific patterns.
"""
import re

def fix_file(filepath):
    with open(filepath, 'r') as f:
        content = f.read()

    # Patterns to fix - we want to add g.completed_blocks.push(X) after X.has_terminator = true
    # for specific block patterns (not the final blk in generate_function_body)

    # Pattern 1: Safe division/modulo - push b and panic_block
    # Find: b.terminator = Box_new(term_br_cond(...)) \n b.has_terminator = true \n\n -- Panic block
    content = re.sub(
        r'(    b\.terminator = Box_new\(term_br_cond\(ir_val_inst\(zero_check_id\), panic_label, ok_label\)\)\n    b\.has_terminator = true)\n(\n    -- Panic block)',
        r'\1\n    g.completed_blocks.push(b)\2',
        content
    )

    # Pattern 2: Panic blocks in div/mod
    # Find: panic_block.has_terminator = true \n\n -- OK block
    content = re.sub(
        r'(    panic_block\.terminator = Box_new\(term_unreachable\(\)\)\n    panic_block\.has_terminator = true)\n(\n    -- OK block)',
        r'\1\n    g.completed_blocks.push(panic_block)\2',
        content
    )

    # Pattern 3: If expression - entry block
    # Find: b.has_terminator = true \n }\n\n -- Generate then block
    content = re.sub(
        r'(    -- Terminate current block with conditional branch\n    if expr\.has_else \{\n        b\.terminator = Box_new\(term_br_cond\(ir_val_inst\(cond_id\), then_label, else_label\)\)\n        b\.has_terminator = true)\n(    \} else \{)',
        r'\1\n        g.completed_blocks.push(b)\n\2',
        content
    )
    content = re.sub(
        r'(    \} else \{\n        b\.terminator = Box_new\(term_br_cond\(ir_val_inst\(cond_id\), then_label, join_label\)\)\n        b\.has_terminator = true)\n(    \})',
        r'\1\n        g.completed_blocks.push(b)\n\2',
        content
    )

    # Pattern 4: If expression - then block
    content = re.sub(
        r'(    -- Terminate then block with branch to join\n    if then_block\.has_terminator == false \{\n        then_block\.terminator = Box_new\(term_br\(join_label\)\)\n        then_block\.has_terminator = true\n    \})\n(\n    -- Generate else block)',
        r'\1\n    g.completed_blocks.push(then_block)\2',
        content
    )

    # Pattern 5: If expression - else block
    content = re.sub(
        r'(        if else_block\.has_terminator == false \{\n            else_block\.terminator = Box_new\(term_br\(join_label\)\)\n            else_block\.has_terminator = true\n        \})\n(    \} else \{)',
        r'\1\n        g.completed_blocks.push(else_block)\n\2',
        content
    )
    content = re.sub(
        r'(        else_block\.terminator = Box_new\(term_br\(join_label\)\)\n        else_block\.has_terminator = true)\n(    \})',
        r'\1\n        g.completed_blocks.push(else_block)\n\2',
        content
    )

    # Pattern 6: Error propagate
    # Entry block
    content = re.sub(
        r'(    -- Branch: if tag == 0 go to ok, else go to err\n    b\.terminator = Box_new\(term_br_cond\(ir_val_inst\(is_ok_id\), ok_label, err_label\)\)\n    b\.has_terminator = true)\n(\n    -- Err path)',
        r'\1\n    g.completed_blocks.push(b)\2',
        content
    )
    # Err block
    content = re.sub(
        r'(    err_block\.terminator = Box_new\(term_ret\(ir_val_inst\(operand_id\)\)\)\n    err_block\.has_terminator = true)\n(\n    -- Ok path)',
        r'\1\n    g.completed_blocks.push(err_block)\2',
        content
    )
    # Ok block
    content = re.sub(
        r'(    ok_block\.terminator = Box_new\(term_br\(cont_label\)\)\n    ok_block\.has_terminator = true)\n(\n    -- Continue block)',
        r'\1\n    g.completed_blocks.push(ok_block)\2',
        content
    )

    # Pattern 7: If statement
    content = re.sub(
        r'(    -- Conditional branch\n    if stmt\.has_else_branch \{\n        b\.terminator = Box_new\(term_br_cond\(ir_val_inst\(cond_result\.val_id\), then_label, else_label\)\)\n        b\.has_terminator = true)\n(    \} else \{)',
        r'\1\n        g.completed_blocks.push(b)\n\2',
        content
    )
    content = re.sub(
        r'(    \} else \{\n        b\.terminator = Box_new\(term_br_cond\(ir_val_inst\(cond_result\.val_id\), then_label, join_label\)\)\n        b\.has_terminator = true)\n(    \}\n\n    -- Then block)',
        r'\1\n        g.completed_blocks.push(b)\n\2',
        content
    )
    # Then block
    content = re.sub(
        r'(    let then_result = generate_block_stmts\(g, then_block, stmt\.if_then\)\n    g = then_result\.gen\n    then_block = then_result\.block\n    if then_block\.has_terminator == false \{\n        then_block\.terminator = Box_new\(term_br\(join_label\)\)\n        then_block\.has_terminator = true\n    \})\n(\n    -- Else block)',
        r'\1\n    g.completed_blocks.push(then_block)\2',
        content
    )
    # Else block
    content = re.sub(
        r'(        if else_block\.has_terminator == false \{\n            else_block\.terminator = Box_new\(term_br\(join_label\)\)\n            else_block\.has_terminator = true\n        \})\n(    \}\n\n    -- Join block)',
        r'\1\n        g.completed_blocks.push(else_block)\n\2',
        content
    )

    # Pattern 8: While loop
    # Entry block
    content = re.sub(
        r'(    -- Branch to header\n    b\.terminator = Box_new\(term_br\(header_label\)\)\n    b\.has_terminator = true)\n(\n    -- Header block)',
        r'\1\n    g.completed_blocks.push(b)\2',
        content
    )
    # Header block
    content = re.sub(
        r'(    header_block\.terminator = Box_new\(term_br_cond\(ir_val_inst\(cond_result\.val_id\), body_label, exit_label\)\)\n    header_block\.has_terminator = true)\n(\n    -- Body block)',
        r'\1\n    g.completed_blocks.push(header_block)\2',
        content
    )
    # Body block - this is already handled by sed earlier for for-loops, but apply here too
    content = re.sub(
        r'(    if body_block\.has_terminator == false \{\n        body_block\.terminator = Box_new\(term_br\(header_label\)\)\n        body_block\.has_terminator = true\n    \})\n(\n    -- Restore break/continue)',
        r'\1\n    g.completed_blocks.push(body_block)\2',
        content
    )

    with open(filepath, 'w') as f:
        f.write(content)

    print(f"Fixed {filepath}")

if __name__ == '__main__':
    fix_file('/home/burgess/Work/dAImond-lang/stage4/main.dm')
