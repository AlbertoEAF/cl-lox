# cl-lox
cl-lox is a fast fully-compliant Common Lisp implementation of Lox with additional extensions. 

Lox is the language created by Robert Nystrom which is detailed in the excellent Crafting interpreters book: https://craftinginterpreters.com/contents.html

The master branch always contains the latest stable release.

# Current state

You can already use it, it's stable.

If you're familiar with Lisp, we recommend installation through Roswell as its the fastest way to get working:

```bash
# Install
ros install albertoeaf/cl-lox

# Use
cl-lox --script myscript.lox
```

If not, you can install manually, and a dockerized version will also be made available.

# TODO

- [ ] Add install documentation
  - [ ] Add install instructions for Roswell
  - [ ] Add install through docker for those less 
- [ ] Finish implementing chapter 12
- [ ] Finish implementing chapter 13
