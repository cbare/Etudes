-- counts the number of classes being taken
select n_classes, count(student_id)
from (
  select
    s.id as student_id,
    count(e.class_id) as n_classes
  from students s 
    left join enrollments e
    on e.student_id=s.id
  group by s.id
)
group by n_classes;

-- counts number of students in each class
select
  c.id as class_id,
  c.name as class_name,
  count(e.student_id) as n_students
from classes c
  left join enrollments e
  on c.id = e.class_id
group by c.id;

-- list students in a class
select id, name from classes where id=19;
select s.id, s.first_name, s.last_name
from students s
  join enrollments e
  join classes c
  on s.id = e.student_id and e.class_id = c.id
where c.id=19
order by s.last_name, s.first_name;

-- show schedule for a student
select id, first_name, last_name from students where id=99;
select c.id, c.name
from students s
  join enrollments e
  join classes c
  on s.id = e.student_id and e.class_id = c.id
where s.id = 99
order by c.name;

-- show students not signed up for classes
select s.id, s.first_name, s.last_name, count(e.class_id) as n_classes
from students s
  left join enrollments e
  on s.id = e.student_id
group by s.id
having n_classes = 0;

-- show students not signed up for classes, second way
select s.id, s.first_name, s.last_name
from students s
where s.id not in (
  select distinct student_id from enrollments
);
