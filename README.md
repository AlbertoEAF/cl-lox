# cl-lox
cl-lox is a fast fully-compliant [Lox](https://www.craftinginterpreters.com/the-lox-language.html) language implementation in Common Lisp with extra *goodies*.

If you're curious check the [Crafting Interpreters book](https://craftinginterpreters.com/contents.html) and learn how to write a complete language from scratch.

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

# Demos

You can try out all the demos without installing by using docker:
```bash
bash run.sh # spin up a docker instance with cl-lox
ros run # start a Lisp environment
```
Now run the following commands inside the Lisp environment:

```common-lisp
;; Load cl-lox:
(asdf:make :cl-lox)
;; Run all the demos and time it:
(time (lox:run-demos "demos/"))
```
You should see the outputs of the several demos and the total time:

```bash
...

Creating bacon!
Crunch crunch crunch!
Only 1 slice(s) left of me!
Crunch crunch crunch!
Only 0 slice(s) left of me!
Only 0 slice(s) left of me!
Adding 3 slices of bacon!
Only 3 slice(s) left of me!
NIL


------------------------ Demo demo9-class-inheritance -------------------------
Running file /home/alberto.ferreira/code/external/AlbertoEAF/cl-lox/demos/demo9-class-inheritance.lox.

Fry until golden brown.
Pipe full of custard and coat with chocolate.
NIL
Evaluation took:
  0.164 seconds of real time
  0.164993 seconds of total run time (0.161349 user, 0.003644 system)
  [ Run times consist of 0.005 seconds GC time, and 0.160 seconds non-GC time. ]
  100.61% CPU
  477,229,056 processor cycles
  68,793,072 bytes consed
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



# Future work

For now I'm happy with it, but I might decide later to:
- Add enhancements to the language.
- Extend standard library.
- Further benchmark and improve performance.
