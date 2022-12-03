
score = {'r': 1, 'p': 2, 's': 3}
mp = {'A': 'r', 'X': 'r', 'B': 'p', 'Y': 'p', 'C': 's', 'Z': 's'}
win = {'r': 's', 's': 'p', 'p': 'r'}

def part1():
    ans = 0
    with open('input2.txt', 'r') as f:
        for line in f:
            op, me = line.split()
            op, me = mp[op], mp[me]
            if win[me] == op:
                ans += 6
            elif me == op:
                ans += 3
            ans += score[me]
    print(ans)

def part2():
    ans = 0
    with open('input2.txt', 'r') as f:
        for line in f:
            op, strategy = line.split()
            if strategy == 'X': # Lose
                move = win[mp[op]]
                ans += score[move]
            elif strategy == 'Y': # Draw
                move = mp[op]
                ans += 3 + score[move]
            else:
                move = win[win[mp[op]]]
                ans += 6 + score[move]
    print(ans)

part1()
part2()