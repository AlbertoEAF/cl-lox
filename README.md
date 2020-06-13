# cl-lox
cl-lox is a fast fully-compliant Common Lisp implementation of Lox with additional extensions.

Lox is the language created by Robert Nystrom which is detailed in the excellent Crafting interpreters book: https://craftinginterpreters.com/contents.html

The master branch always contains the latest stable release.
You can already use it, it's stable.

# Language extensions

Safety:
- Evaluating uninitialized variables is forbidden.

Standard Library:
- clock() -> Timestamp in seconds
- readline() -> reads a string from the user
- readfile(path) -> whole file as a string

Under test:
- For now, `+` applies to mix of strings and numbers.
  - Although practical I might remove it to avoid silent errors like in JavaScript.

# Installation

There are 3 options. Install manually, through roswell or run inside docker.

If you're familiar with Lisp, we recommend installation through Roswell as its the fastest way to get working and you don't even need to clone the repo:

```bash
# Install
ros install albertoeaf/cl-lox
```

If instead you prefer a docker environment just use `run.sh`, it will take a while for the first time, but then be very fast:
```bash
# Launch environment
bash run.sh

cl-lox --script myscript.lox
```

If not, you can install manually.

# Usage

```bash
# Calling lox scripts:
cl-lox --script myscript.lox

# Running the interpreter (2 enters to finish a prompt):
cl-lox
```



# TODO

- [ ] Finish implementing chapter 13
