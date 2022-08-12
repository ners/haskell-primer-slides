with open('counter.txt', 'r') as handle:
    x = handle.readline()

with open('counter.txt', 'w') as handle:
    handle.write(int(x) + 1)
