import sys

valid = 0

for line in sys.stdin.readlines():
    raw_counts, raw_char, password = line.strip().split(" ")
    min_count, max_count = [int(x) - 1 for x in  raw_counts.split("-")]
    char = raw_char[0]

    bool_1 = password[min_count] == char if min_count < len(password) else None
    bool_2 = password[max_count] == char if max_count < len(password) else None

    if min_count >= len(password) or max_count >= len(password):
        print(len(password), min_count, max_count)
    if (bool_1 is True or bool_2 is True) and not (bool_1 is True and bool_2 is True):
        valid += 1
print(valid)




