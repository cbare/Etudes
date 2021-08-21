
create table if not exists students (
    id integer primary key autoincrement,
    first_name text not null,
    last_name text not null,
    dob text not null
);

create table if not exists classes (
    id integer primary key autoincrement,
    name text not null
);

create table if not exists enrollments (
    student_id integer not null,
    class_id integer not null
);
