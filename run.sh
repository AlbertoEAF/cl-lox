#!/usr/bin/env BASH
set -e # abort immediately if any error arises.


docker build -t cl-lox .
docker run -it --rm cl-lox

