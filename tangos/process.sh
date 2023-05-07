#!/bin/bash

m-mecab.sh $1 | sort -u | python3 ./reformat.py 
