from math import floor

def fuel(mass):
    return (floor(mass / 3)) - 2

def read_input():
    with open("1-input", "r") as fp:
        return [int(line) for line in fp.readlines()]

def round_1():
    print(f"round 1: {sum([fuel(mass) for mass in read_input()])}")

def recursive_fuel(mass):
    new_fuel = (floor(mass / 3)) - 2
    if new_fuel <= 0:
        return 0
    return new_fuel + recursive_fuel(new_fuel)

def round_2():
    print(f"round 2: {sum([recursive_fuel(mass) for mass in read_input()])}")

if __name__ == '__main__':
    round_1()
    round_2()
