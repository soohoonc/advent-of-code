
def part1():
    ans = 0
    with open('input03.txt', 'r') as f:
        for line in f:
            line = line.strip()
            r1, r2 = line.strip()[:len(line)//2], line[len(line)//2:]
            d = {}
            for c in r1:
                d[c] = ord(c) - (32 if c == c.lower() else - 26) - 64
            for c in r2:
                if d.get(c) is not None:
                    ans += d[c]
                    break
                    
    print(ans)

def part2():
    ans = 0
    with open('input03.txt', 'r') as f:
        lines = f.readlines()
        assert len(lines) % 3 == 0
        for i in range(len(lines) // 3):
            group = lines[i*3:i*3+3]
            d1, d2 = {}, {}
            for c in group[0].strip():
                d1[c] = ord(c) - (32 if c == c.lower() else - 26) - 64
            for c in group[1].strip():
                d2[c] = ord(c) - (32 if c == c.lower() else - 26) - 64
            for c in group[2].strip():
                if c in d1 and c in d2:
                    ans += d1[c]
                    break
    print(ans)

part1()
part2()