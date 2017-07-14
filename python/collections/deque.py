#!/usr/bin/env python2
from collections import deque

commands = {
    'append' : (lambda d,x: d.append(x)),
    'appendleft' : (lambda d,x: d.appendleft(x)),
    'pop' : (lambda d: d.pop()),
    'popleft' : (lambda d: d.popleft()),
}

def runCommand(d, command, arg = None):
    f = commands[command]
    if arg == None:
        f(d)
    else:
        f(d,arg)
        
def parseInput(s):
    xs = s.split(' ')
    return xs[0], xs[1] if len(xs) > 1 else None

if __name__ == '__main__':
    dq = deque()
    n = int(raw_input())
    while n > 0:
        c, a = parseInput(raw_input().strip())
        runCommand(dq, c, a)
        n = n - 1
    print ' '.join(dq)
