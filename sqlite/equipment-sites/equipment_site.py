"""
Equipment at sites. Fun with SQL.
"""
import datetime as dt
import random
import sqlite3
import string

def timestamp_gen(start, end):
    t = start
    while t <= end:
        yield t
        t += dt.timedelta(
            hours=random.betavariate(0.7, 0.5)*72
        )


con = sqlite3.connect("./sqlite/data/equipment.sqlite.db")

sql = """
    insert into equipment
    (serial_no, description)
    values
    (?, ?)
"""
# cur = con.cursor()
# cur.executemany(sql, [
#     (((43*83+i*29*67*41) % 89999 + 10000), a)
#     for i,a in zip(range(10), string.ascii_lowercase)
# ])
# con.commit()

sql = """
    insert into sites
    (name)
    values
    (?)
"""
# cur = con.cursor()
# cur.executemany(sql, [
#     (f'site-{a}',) for a in string.ascii_uppercase[0:5]
# ])
# con.commit()


equipment_site = {
    1: [1, 4, 5, 1],
    2: [2, 2, 4, 9],
    3: [3, 6, 7, 7],
    4: [4, 8, 8, 10],
    5: [5, 5, 5, 10],
    6: [1, 6, 9, 9],
    7: [1, 4, 1, 4],
    8: [2, 6, 8, 6],
    9: [3, 7, 8, 10],
    10:[3, 3, 10, 3],
}

job_types = {
    0:('bar', 'bat'),
    1:('foo', 'quux'),
}

sql = """
    insert into jobs
    (equipment_id, site_id, timestamp, description)
    values
    (?,?,?,?)
"""
ts_gen = timestamp_gen(dt.datetime(2019,1,1), dt.datetime(2022,11,1))

cur = con.cursor()
cur.executemany(sql, [
    (i, equipment_site[i][k], next(ts_gen).date(), random.choice(job_types[i%2]))
    for k in range(4)
        for j in range(2)
            for i in range(1,11)
])
con.commit()
