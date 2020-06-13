# cl-lox
cl-lox is a fast fully-compliant [Lox](https://www.craftinginterpreters.com/the-lox-language.html) language implementation in Common Lisp with extra *goodies*.

*If you're curious check the amazing [Crafting Interpreters book](https://craftinginterpreters.com/contents.html) to learn how to write a fully featured language from scratch.*

# Installation

Run cl-lox inside a docker container or install it on your machine.

### Docker Environment

Spin up a docker container with cl-lox:
```bash
bash run.sh
```
The first run takes longer, but then it's very fast.

### Local Installation

With [Roswell](https://github.com/roswell/roswell) you don't even need to clone the repo, just run:

```bash
ros install albertoeaf/cl-lox
```

You can also install manually using Quicklisp or ASDF.


# Usage

```bash
# Calling lox scripts (this demo prints the first 20 fibonacci numbers):
cl-lox --script demos/demo6-fun-fib.lox

# Running the interpreter (every empty line triggers the commands before to run):
cl-lox
```

# Language extensions

Safety:
- Evaluating uninitialized variables is forbidden.

Standard Library:
- clock() -> Timestamp in seconds
- readline() -> reads a string from the user
- readfile(path) -> whole file as a string

Extensions:
- `+` can sum numbers or concatenate strings with numbers.



# TODO

- [ ] Finish implementing chapter 13
