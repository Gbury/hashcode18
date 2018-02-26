import sys

n = sys.stdin.readline().strip()
print(n)
for line in sys.stdin:
    a, b, c, d = line.strip().split()
    print(b, a, d, c)

