"""
Compute average task duration by user and by country
"""
from collections import defaultdict, namedtuple
import sys

User = namedtuple('User', 'id country_code')
Task = namedtuple('Task', 'id user_id seconds')


def user_from_line(line):
    """
    Given a line of text containing user ID and country code, return a User object
    """
    user_id, country_code = line.strip().split()
    return User(int(user_id), country_code)


def task_from_line(line):
    """
    Given a line of text containing task ID, user ID, and country code, return a Task object
    """
    task_id, user_id, seconds = line.strip().split()
    return Task(int(task_id), int(user_id), float(seconds))


def read_task_times(f):
    """
    Read users and tasks from file 'f'.
    """
    n = int(next(f))
    users = [user_from_line(line) for _, line in zip(range(n), f)]

    t = int(next(f))
    tasks = [task_from_line(line) for _, line in zip(range(t), f)]

    return users, tasks


def average(xs):
    """
    Return the average of a list of numbers or None for empty lists
    """
    return sum(xs)/len(xs) if len(xs) > 0 else None


def user_average_time(users, tasks):
    """
    return a list of tuples (user_id, seconds) where the seconds is
    the average task completion time for that user or None for no tasks completed
    """
    user_times = {user.id:[] for user in users}
    for task in tasks:
        user_times[task.user_id].append(task.seconds)
    
    return sorted((user_id, average(task_times)) for user_id, task_times in user_times.items())


def country_average_time(users, tasks):
    """
    return a list of tuples (country_code, seconds) where the seconds is
    the average task completion time for users in the country or None for
    no tasks completed
    """
    user_countries = {user_id:country_code for user_id, country_code in users}
    countries = {country_code for user_id, country_code in users}

    country_times = {country_code: [] for country_code in countries}
    for task in tasks:
        country_times[user_countries[task.user_id]].append(task.seconds)
    
    return sorted((country_code, average(task_times)) for country_code, task_times in country_times.items())


def format_seconds(seconds):
    """
    Round seconds to 2 decimal places or return '0.00' for no data.
    """
    # TODO it would be better to show something like NA for no data
    return f'{seconds:0.2f}' if seconds else '0.00'


def main():
    users, tasks = read_task_times(sys.stdin)

    user_average_times = user_average_time(users, tasks)
    for user_id, seconds in user_average_times:
        print(f'{user_id} {format_seconds(seconds)}')

    country_average_times = country_average_time(users, tasks)
    for country_code, seconds in country_average_times:
        print(f'{country_code} {format_seconds(seconds)}')


if __name__ == "__main__":
    main()
