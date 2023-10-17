inp = None
with open('1-input', 'r') as fp:
    inp = fp.read()
sums = sorted([sum([int(entity) for entity in entity_str.split("\n") if entity != '']) for entity_str in inp.split("\n\n")])
print(f"round 1: {sums[-1]}")
print(f"round 2: {sums[-1]} {sums[-2]}, {sums[-3]}, {sum(sums[-3:])}")


