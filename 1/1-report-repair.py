import sys

mapping = {}
numbers = [int(line) for line in sys.stdin.readlines()]


pairs = [(n1, n2) for n1 in numbers for n2 in numbers]

for p in pairs:
    addition = sum(p)
    mapping[addition] = p

for num in numbers:
    comp = 2020 - num
    if comp in mapping:
        print(mapping[comp][0] * mapping[comp][1] * num)
