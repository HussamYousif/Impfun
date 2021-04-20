#!/bin/bash

examples='/home/hussam/git/impfun/examples/'
compiler='./ImpFun'

cd $examples

for filename in *.impf
do
    echo "Currently at $filename"
    $compiler $filename
    printf "\n"
done


