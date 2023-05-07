#!/bin/bash

cp -f ../db/empty.db /tmp/jp-status.db
echo  "delete from words;" | sqlite3 /tmp/jp-status.db
m-mecab.sh $1 | sort -u | python3 ./reformat.py
cp -f /tmp/jp-status.db $2
