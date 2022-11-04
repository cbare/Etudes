select
  date(j.timestamp),
  eq.serial_no,
  s.name
from
  jobs j
  join equipment eq on j.equipment_id = eq.id
  join sites s on j.site_id = s.id
order by
  eq.serial_no


select
  eq.serial_no,
  s.name,
  min(j.timestamp) as start,
  max(j.timestamp) as end
from
  jobs j
  join equipment eq on j.equipment_id = eq.id
  join sites s on j.site_id = s.id
group by
  eq.serial_no, s.name
order by
  eq.serial_no, start

with grouped_jobs as (
  select
    eq.serial_no,
    s.name,
    j.timestamp,
    (
      row_number() over (partition by eq.serial_no order by j.timestamp) -
      row_number() over (partition by eq.serial_no, s.name order by j.timestamp)
    ) as grp
  from
    jobs j
    join equipment eq on j.equipment_id = eq.id
    join sites s on j.site_id = s.id
  order by
    eq.serial_no, j.timestamp
)
select
    serial_no,
    name,
    min(timestamp) as start,
    max(timestamp) as end
from
    grouped_jobs
group by
    serial_no, name, grp
order by
    serial_no, start
