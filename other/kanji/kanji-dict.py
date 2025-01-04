#!/usr/bin/env python3

import sqlite3
import os
import sys
import regex as re


if len(sys.argv) != 2:
    print("Usage: python script.py <kanji>")
    sys.exit(1)

japKanji = re.compile(r'\p{IsHan}', re.UNICODE)
japHiragana = re.compile(r'\p{IsHiragana}', re.UNICODE)
japKatakana = re.compile(r'\p{IsKatakana}', re.UNICODE)

def IsKanji(w):
    return japKanji.search(w)

def IsHiragana(w):
    return japHiragana.search(w)

def IsKatakana(w):
    return japKatakana.search(w)



st = sys.argv[1]
fileDb = "/opt/homebrew/anaconda3/lib/python3.11/site-packages/jamdict_data/jamdict.db"

if not os.path.exists(fileDb):
    print(f"Database file {fileDb} does not exist.")
    sys.exit(1)

connection = sqlite3.connect(fileDb)
cursor = connection.cursor()


def print_kanji(kanji):
    cursor.execute("SELECT * FROM character WHERE literal = ?", (kanji,))
    result = cursor.fetchone()

    if not result:
        print(f"No match found for kanji: {kanji}")
        return

    gid = result[0]

    print("%s Strokes: %s Grade: %s Freq: %s JLPT: %s"
          % (result[1], result[2], result[3], result[4], result[5]))

    cursor.execute("SELECT * FROM meaning WHERE gid = ? and m_lang = ''", (gid,))
    for result in cursor:
        print("   ",  result[1], end='')
    print()

    cursor.execute("SELECT * FROM reading WHERE gid = ? and r_type in ('ja_on','ja_kun') ", (gid,))
    for result in cursor:
        print("       %s: %s"%(result[1],result[2]), end='')
        if result[3] != "":
            print("  ontype: %s   status: %s"%(result[3], result[4]),end='')
        print()

for k in st:
    if IsKanji(k):
        print_kanji(k)

print("\nDone.")
connection.close()
