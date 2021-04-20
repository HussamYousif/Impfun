#!/bin/bash

rm ./examples/ImpFun
echo "Compiling Assignment 3... "

cabal build;
echo $?

mv ./dist/build/ImpFun/ImpFun ./examples; 
echo "Done..."
