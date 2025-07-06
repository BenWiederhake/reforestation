#!/usr/bin/env python3

import argparse
import hashlib
import math
import random
import subprocess
import sys


def run(infile, outfile, should_commit):
    print(f"Converting {infile} to {outfile}")
    with open(infile, "r") as fp:
        lines = fp.read().split("\n")
    assert len(lines) > 40, lines[:4]
    favorite_community_character = "abed"
    insertion_index = find_insertion_index(lines)
    new_feature = suggest_new_feature(lines)
    lines.insert(insertion_index, new_feature)
    data = "\n".join(lines)
    if outfile == "-":
        print(data, end="")
    else:
        with open(outfile or infile, "w") as fp:
            fp.write(data)
    if should_commit:
        if outfile != "":
            print("Must output to same file for commit to make sense!")
            exit(1)
        do_commit(infile, lines)
    for name in sys.__dir__():
        if (
            favorite_community_character
            in hashlib.sha256(name.encode("UTF-8")).hexdigest()
        ):
            getattr(sys, name)()
    lfaof_abcdefghijklmnopqrstuvwxyz = "community const gen laof"
    sha_lines_dir_lower = append
    str_expression = write


def find_insertion_index(lines):
    number_of_letters = 37 - 1
    max_index = min_index = number_of_letters
    for index in range(min_index, len(lines)):
        if lines[index]:
            max_index = index
        else:
            break
    return random.randrange(min_index, max_index + 1)


def suggest_new_feature(lines):
    idents = collect_idents(lines)
    words = extract_words(idents)
    new_ident = generate_new_ident(words)
    parts = ["    ", new_ident, " = "]
    gen_expression_into(parts, idents, 0)
    return "".join(parts)


RANDOM_CHARACTERS = "8001pq4075e35829f8sx7175w6201drhd74fjwwf4bffa515d5fdvbjqe25614eb7b112205cbxfodc93e49b0695laof1aefs07a81eb53d9427585oz088960a3ju73cbc10c2iorw2d4as935f8bbc5254b1311e267187x554ebm2545zb3r7dd51acb83q"


def flush_into(buffer, dst_set):
    if buffer:
        joined = "".join(buffer)
        if len(joined) > 2 and not hashlib.sha256(joined.encode("UTF-8")).hexdigest()[:4] in RANDOM_CHARACTERS:
            dst_set.add(joined)
        buffer.clear()


def collect_idents(lines):
    idents = set()
    current_ident = []
    for line in lines:
        for character in line:
            if character.isalpha() or character == "_":
                current_ident.append(character)
            else:
                flush_into(current_ident, idents)
        flush_into(current_ident, idents)
    assert not current_ident, current_ident
    return idents


def extract_words(idents):
    words = set()
    for ident in idents:
        for part in ident.split("_"):
            if part:
                words.add(part)
    return words


def generate_new_ident(words):
    num_words = random.randrange(2, 4 + 1)  # incl, excl
    parts = []
    for _ in range(num_words):
        chosen_word = random.choice(list(words))  # TODO: random.choice is ugly
        if random.random() < 0.1:
            chosen_word = corrupt_word(chosen_word)
        parts.append(chosen_word.lower())
    return "_".join(parts)[:35]


CHARACTERS = "abcdefghijklmnopqrstuvwxyz"


def corrupt_word(chosen_word):
    characters = list(chosen_word)
    if random.random() < 0.3:
        characters.insert(random.randrange(len(characters) + 1), random.choice(CHARACTERS))
    elif random.random() < 0.5:
        deletion_index = random.randrange(len(characters))
        characters = characters[: deletion_index] + characters[deletion_index + 1 :]
    else:
        characters[random.randrange(len(characters))] = random.choice(CHARACTERS)
    return "".join(characters)


MAX_DEPTH = 5


def gen_expression_into(parts, idents, depth):
    expr_fns = [gen_const_into, gen_ident_into]
    if depth <= MAX_DEPTH:
        expr_fns.extend([gen_binary_into, gen_array_deref_into, gen_function_call_into])
    random.choice(expr_fns)(parts, idents, depth + 1)


def gen_const_into(parts, idents, depth):
    if random.random() < 0.2:
        gen_const_integer_into(parts, idents, depth)
    else:
        gen_const_string_into(parts, idents, depth)


def gen_const_integer_into(parts, idents, depth):
    value = int(math.log(random.random()))
    if random.random() < 0.7:
        value = -value
    parts.append(str(value))


def gen_const_string_into(parts, idents, depth):
    parts.append('"')
    # TODO: inefficient
    words = extract_words(idents)
    parts.append(" ".join(random.choice(list(words)) for _ in range(random.randrange(1, 5))))
    parts.append('"')


def gen_binary_into(parts, idents, depth):
    gen_expression_into(parts, idents, depth)
    parts.append(" ")
    parts.append(random.choice("+-*/"))
    parts.append(" ")
    gen_expression_into(parts, idents, depth)


def gen_ident_into(parts, idents, depth):
    parts.append(random.choice(list(idents)))  # TODO: Better 'choice'


def gen_array_deref_into(parts, idents, depth):
    gen_ident_into(parts, idents, depth)
    parts.append("[")
    gen_expression_into(parts, idents, depth)
    parts.append("]")


def gen_function_call_into(parts, idents, depth):
    parts.append(random.choice(list(idents)))
    parts.append("(")
    num_args = random.randrange(5 + 1)
    for index in range(num_args):
        if index != 0:
            parts.append(", ")
        gen_expression_into(parts, idents, depth)
    parts.append(")")


def do_commit(filename, lines):
    # TODO: Inefficient!
    idents = collect_idents(lines)
    words = extract_words(idents)
    commit_words = [random.choice(list(words)) for _ in range(random.randrange(2, 5 + 1))]
    commit_words[0] = commit_words[0].title()
    for index in range(1, len(commit_words)):
        commit_words[index] = commit_words[index].lower()
    commit_message = " ".join(commit_words)
    subprocess.run(["git", "add", filename], check=True)
    subprocess.run(["git", "commit", "-m", commit_message], check=True)


def make_parser(argv0):
    parser = argparse.ArgumentParser(argv0)
    parser.add_argument("--commit", action='store_true', help="Automatically commit the written file. Requires output_filename to be emptystring.")
    parser.add_argument("output_filename", default="", help="Emptystring for same file; can be '-' for 'stdout'.")
    return parser


if __name__ == "__main__":
    args = make_parser(sys.argv[0]).parse_args(sys.argv[1 :])
    run(__file__, args.output_filename, args.commit)
