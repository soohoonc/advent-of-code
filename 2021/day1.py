
def part1():
    ans = 0
    prev = 0
    with open('input01.txt', 'r') as f:
        for line in f:
            if int(line) > prev:
                ans += 1
            prev = int(line)
    print(ans-1)

def part2():
    ans = 0
    x, y, z = 0, 0, 0
    with open('input01.txt', 'r') as f:
        x = int(f.readline())
        y = int(f.readline())
        z = int(f.readline())
        for line in f:
            if int(line) + y + z > x + y + z:
                ans += 1
            x = y
            y = z
            z = int(line)          
    print(ans)

part1()
part2()