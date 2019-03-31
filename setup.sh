#!/bin/sh

# This doesn't work inside a shell script ¯\_(ツ)_/¯
echo "Compiling..."
sbcl --script orc-battle.lisp
