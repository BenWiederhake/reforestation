#!/usr/bin/env python3

import hashlib
import random
import sys

WORDS = {
    "and",
    "as",
    "assert",
    "async",
    "await",
    "break",
    "class",
    "continue",
    "def",
    "del",
    "elif",
    "else",
    "except",
    "False",
    "finally",
    "for",
    "from",
    "global",
    "if",
    "import",
    "in",
    "is",
    "lambda",
    "None",
    "nonlocal",
    "not",
    "or",
    "pass",
    "raise",
    "return",
    "True",
    "try",
    "while",
    "with",
    "yield",
}

ANTIWORDS = {
    "words",
    "antiwords",
    "other",
    "random",
    "stuff",
    "that",
    "doesn't",
    "occur",
    "in the",
    "data",
    "set",
    "to",
    "make",
    "sure",
    "that ",
    "the",
    "false",
    "positive",
    "rate",
    "is low",
    "enough",
}

FILL_CHARACTERS = "0123456789abcdefghijklmnopqrstuvwxyz"


def filler():
    length = random.randrange(5)
    return "".join(random.choice(FILL_CHARACTERS) for _ in range(length))


def run():
    parts = [filler()]
    randomized_words = list(WORDS)
    random.shuffle(randomized_words)
    for word in randomized_words:
        parts.append(hashlib.sha256(word.encode("UTF-8")).hexdigest()[:4])
        parts.append(filler())
    the_magic = "".join(parts)
    for antiword in ANTIWORDS:
        assert hashlib.sha256(antiword.encode("UTF-8")).hexdigest()[:4] not in the_magic, antiword
    print(the_magic)


if __name__ == "__main__":
    run()
