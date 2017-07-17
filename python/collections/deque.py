#!/usr/bin/env python2
from collections import deque

def nosuch(*args):
    pass

def runCommand(d, command, args):
    getattr(d, command, nosuch)(*args)
        
def parseInput(s):
    xs = s.split(' ')
    return xs[0], xs[1:] if len(xs) > 1 else []

if __name__ == '__main__':
    dq = deque()
    n = int(raw_input())
    while n > 0:
        c, a = parseInput(raw_input().strip())
        runCommand(dq, c, a)
        n = n - 1
    print ' '.join(dq)
