import datetime as dt
import logging
import random
import sqlite3


logger = logging.getLogger(__file__)


with open('data/first_names.txt') as f:
    first_names = [n.strip() for n in f]

with open("data/last_names.txt") as f:
    last_names = [n.strip() for n in f]


CLASS_NAMES = [
    "Mineral Psychology",
    "Underwater Basket Weaving",
    "The History of Mud",
    "Algorithms and Data Structures",
    "Programming Languages",
    "Natural Language Processing",
    "Computational Linguistics",
    "Machine Learning",
    "Deep Learning",
    "The Literature of Science Fiction",
    "Creative Writing",
    "Statistics",
    "Writing Creative Non-fiction",
    "Jazz Composition",
    "Jazz Piano",
    "Classical Piano",
    "Music Theory",
    "Intro to Biology",
    "Computational Biology",
    "The Molecular Biology of the Cell",
    "Cancer Biology",
    "Number Theory",
    "The History of Technology",
]


def insert_students(db, n):
    sql = """
    INSERT INTO students
    (first_name, last_name, dob)
    VALUES
    (?,?,?)
    """
    for i in range(1, n+1):
        first_name = random.choice(first_names)
        last_name = random.choice(last_names)
        dob = dt.date.today() - dt.timedelta(days=
            (random.normalvariate(19,2) + random.paretovariate(3))
            * 365.2421897)
        db.execute(sql, (first_name, last_name, dob))
        if i % 100 == 0:
            db.commit()

    db.commit()

    sql = """
    SELECT id FROM students
    """
    cursor = db.execute(sql)
    return [row[0] for row in cursor]


def insert_classes(db):
    sql = """
    INSERT INTO classes
    (name)
    VALUES
    (?)
    """
    for name in CLASS_NAMES:
        db.execute(sql, (name,))

    db.commit()

    sql = """
    SELECT id FROM classes
    """
    cursor = db.execute(sql)
    return [row[0] for row in cursor]


def enroll(db, student_id, class_id):
    sql = """
    INSERT INTO enrollments
    (student_id, class_id)
    VALUES
    (?,?)
    """
    db.execute(sql, (student_id, class_id,))


def enrollments(db, student_ids, class_ids):
    for student_id in student_ids:
        for class_id in random.sample(class_ids, k=random.randint(0,3)):
            logger.debug("enrolling %s in %s", student_id, class_id)

            # nobody signs up for this class
            if class_id == 2: continue

            enroll(db, student_id, class_id)

    db.commit()


with sqlite3.connect("data/class_enrollments.sqlite.db") as db:
    student_ids = insert_students(db, n=1000)
    class_ids = insert_classes(db)
    enrollments(db, student_ids, class_ids)
