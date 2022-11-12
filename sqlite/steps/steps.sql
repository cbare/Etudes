create table if not exists users (
    user_id text primary key,
    first_name text,
    last_name text,
    street_number text,
    street text,
    city text
);

create table if not exists steps (
    date text,
    user_id text,
    steps integer
);

.import --skip 1 --csv data/users.csv users
.import --skip 1 --csv data/steps.csv steps

-- total steps per user during each month
select
  strftime('%Y-%m', s.date) as ym,
  u.city,
  u.first_name,
  u.last_name,
  sum(s.steps) as steps
from users u
inner join steps s on u.user_id=s.user_id
group by city, ym, u.user_id, first_name, last_name
order by ym, u.city, steps desc
;

-- top 3 steppers in each city during each month
with ranked_steps_per_month as
(
    select
        strftime('%Y-%m', s.date) as ym,
        u.city,
        u.first_name,
        u.last_name,
        sum(s.steps) as steps,
        rank() over (
            partition by city, strftime('%Y-%m', s.date)
            order by sum(s.steps) desc
        ) as steps_rank
    from users u
    inner join steps s on u.user_id=s.user_id
    group by ym, city, u.user_id, first_name, last_name
)
select
    ym,
    city,
	first_name,
    last_name,
    steps
from ranked_steps_per_month
where steps_rank <= 3
order by ym, city, steps desc, last_name, first_name
;
