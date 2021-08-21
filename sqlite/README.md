# Playing with sqlite

## Create DB

Create a SQLite database of classes and students for playing around.

``` sh
sqlite3 data/class_enrollments.sqlite.db ".read drop_tables.sql"
sqlite3 data/class_enrollments.sqlite.db ".read create_tables.sql"
python insert_data.py
```

## Things to try

Try some simple and more complicated [queries][2]. Read [how SQLite works][4].

### Explain

Try [EXPLAIN QUERY PLAN][3].

``` sql
sqlite> EXPLAIN QUERY PLAN select * from students where last_name="Smith" and first_name="Chris";
QUERY PLAN
`--SCAN TABLE students
```

[1]: https://sqlite.org/
[2]: queries.sql
[3]: https://sqlite.org/eqp.html
[4]: https://sqlite.org/howitworks.html
