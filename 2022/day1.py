from heapq import heappop, heappush

def part1():
    ans = 0
    calories = 0
    with open('input1.txt', 'r') as f:
        for line in f:
            if line != "\n":
                calories += int(line)
            else:
                ans = max(ans, calories)
                calories = 0
    print(ans)

def part2():
    most_calories = []
    calories = 0
    with open('input1.txt', 'r') as f:
        for line in f:
            if line != "\n":
                calories += int(line)
            else:
                heappush(most_calories, -calories)
                calories = 0
    ans = 0
    for _ in range(3):
        ans -= heappop(most_calories)
    print(ans)

part1()
part2()  