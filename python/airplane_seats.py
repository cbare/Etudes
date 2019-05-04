"""
One Seat Left

You're one of a hundred people standing in line to get onto an airplane that
has 100 seats. There's a seat for every person in line and each of you has a
boarding pass with a seat assignment. The first person to walk onto the plane
drops his boarding pass and, instead of picking it up, decides, "I'm just
going to sit anyplace." He takes a seat at random.

Now, every other passenger will either take his or her assigned seat or, if
that seat is taken, will sit in any seat at random.

Because you are a kind and generous person, you are the last passenger to
board the place. Obviously, there's going to be one seat left, because
everyone else is sitting either in the correct seat or not.

The question is: What are the chances that you get to sit in your assigned
seat?

Apparently, this puzzle appears in the book "Mathematical Puzzles" by Peter
Winkler and also in Bela Bollobas' "The Art of Mathematics" and is also the
subject of an episode of Car Talk on NPR. 
"""
import random

# solve by brute force
n = 1000
m = 100
count = 0

for x in range(n):
    seats = [None]*m
    open_seats = set(range(m))

    # first passenger chooses randomly
    j = random.sample(open_seats, 1)[0]
    seats[j] = 0
    open_seats.remove(j)

    for i in range(1,m):
        if seats[i] is None:
            seats[i] = i
            open_seats.remove(i)
        else:
            j = random.sample(open_seats, 1)[0]
            seats[j] = i
            open_seats.remove(j)

    if seats[99]==99:
        count += 1

print(f'You sit in your own seat {count} times out of {n}.')

