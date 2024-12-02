
def part1():
    ans = 0
    with open('input1.txt', 'r') as f:
        for line in f:
            for c in line:
                if c == '(':
                    ans += 1
                else:
                    ans -= 1
    print(ans)

def part2():
    ans = 0
    floor = 0
    with open('input1.txt', 'r') as f:
        for line in f:
            for c in line:
                if floor >= 0:
                    ans += 1
                    if c == '(':
                        floor += 1
                    else:
                        floor -= 1
    print(ans)

part1()
part2()