#!/usr/bin/env python
import sys
import re

for line in sys.stdin:
    line = line.strip()
    words = re.split('\W+', line) #matches any non-alphanumeric character; this is equivalent to the set [^a-zA-Z0-9_]
    for word in words:
        if word and word[0].isupper():
            print '%s\t%s' % (word, 1)