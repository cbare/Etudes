"""
Generate a random data file for the average_tast_time script to consume.
"""
from collections import defaultdict, namedtuple
import random
import sys


User = namedtuple('User', 'id country_code')
Task = namedtuple('Task', 'id user_id seconds')


country_codes = 'AR AU AT BS BE BA KH CA CL CR HR CZ EG FJ FR DE GR' \
    ' HN HK HU IT JP LA LU MA MY MX MC MM NL PE PT SG ES CH TH TR GB US'.split()


def main():
    users = [User(i, random.choice(country_codes)) for i in range(20)]
    tasks = [Task(i, random.choice(users).id, 5*random.betavariate(2, 5)) for i in range(1000)]

    print(len(users))
    for user in users:
        print(f'{user.id} {user.country_code}')

    print(len(tasks))
    for task in tasks:
        print(f'{task.id} {task.user_id} {task.seconds:0.02f}')


if __name__ == "__main__":
    main()
