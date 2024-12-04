lines = []
with open("2-input", "r") as fp:
    lines = fp.readlines()

levels = [list(map(int, line.strip().split(" "))) for line in lines]

def is_good_level_1(level):
    # 1) Is decreasing
    # 2) Sliding window of two cells (x, y) => 1 <= |x-y| <= 3
    # figure out if decreasing from first two
    decreasing = level[0] > level[1]
    for i in range(len(level) - 1):
        x = level[i]
        y = level[i + 1]
        diff = abs(x - y)
        if (decreasing and x < y) or (not decreasing and x > y) or not (diff <= 3 and diff >= 1):
            return False
    return True

good_levels = [level for level in levels if is_good_level_1(level)]
print(f"Round 1: {len(good_levels)}")

def check_two_levels(x, y, decreasing):
    diff = abs(x - y)
    return not ((decreasing and x < y)\
           or (not decreasing and x > y) \
           or not (diff <= 3 and diff >= 1))

def is_good_level_2(level):
    # 1) Is decreasing
    # 2) Sliding window of two cells (x, y) => 1 <= |x-y| <= 3
    # 3) Can remove any one item to make it safe
    if is_good_level_1(level):
        return True
    # Consider slices of the level and check if they're good
    slices = [level[:i] + level[i + 1:] for i in range(len(level))]
    for s in slices:
        if is_good_level_1(s):
            return True
    return False

good_levels = [level for level in levels if is_good_level_2(level)]
print(f"Round 2: {len(good_levels)}")
