
def part1():
    ans = 0
    x, y = 0, 0
    with open('input2.txt', 'r') as f:
        for line in f:
            instruction, value = line.split()
            if instruction == 'forward':
                x += int(value)
            elif instruction == 'down':
                y += int(value)
            else:
                y -= int(value)
    ans = x * y
    print(ans)

def part2():
    ans = 0
    x, y, aim = 0, 0, 0
    with open('input2.txt', 'r') as f:
        for line in f:
            instruction, value = line.split()
            if instruction == 'forward':
                x += int(value)
                y += aim * int(value)
            elif instruction == 'down':
                aim += int(value)
            else:
                aim -= int(value)
    ans = x * y
    print(ans)

part1()
part2()