
binary = [0] * 20

def part1(binary):
    ans = 0
    total = 0
    gamma = 0
    epsilon = 0
    with open('input03.txt', 'r') as f:
        for line in f:
            l = list(map(lambda x : int(x), line.strip()))
            binary = [sum(x) for x in zip(l, binary)]
            total += 1
    power = 1
    binary = list(map(lambda x : True if x >= total / 2 else False, binary))
    for b in binary[::-1]:
        gamma += power * b
        epsilon += power * (b^True)
        power *= 2
    ans = gamma * epsilon  
    print(ans)
    return binary

def part2(binary):
    ans = 0
    o2, co2 = 0, 0
    o2_max, co2_max = 0, 0
    with open('input03.txt', 'r') as f:
        binaries = list(map(lambda x : list(x), f.readlines()))
        for b in binaries:
            o2_count, co2_count = 0, 0
            for i in range(len(b)):
                if (binary[i] and b[i] == '1') or (not binary[i] and b[i] == '0'):
                    o2_count += 1
                else:
                    if o2_count > o2_max:
                        o2 = b
                        o2_max = o2_count
                    break
            for j in range(len(b)):
                if (not binary[j] and b[j] == '1') or (binary[j] and b[j] == '0'):
                    co2_count += 1
                else:
                    if co2_count > co2_max:
                        co2 = b
                        co2_max = co2_count
                    break
    power = 1
    o, c = 0, 0
    for i in range(len(binary)-1, -1, -1):
        o += int(o2[i]) * power
        c += int(co2[i]) * power
        power *= 2
    ans = o * c
    print(ans)

binary = part1(binary)
part2(binary)