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
;

-- first attempt that doesn't work when equipment leaves and returns
-- to the same site.
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
;


-- Find time interval each piece of equipment spent at each work
-- site. The trick here is the two window functions. We partition
-- by equipment serial number and partition again by serial number
-- and site name. Subtracting gives a group where consecutive rows
-- for the same equipment and site will have the same group number.
-- When the equipment changes site, it gets a new group number. The
-- most tricky aspect is that when the equipment returns to a site
-- previously visited, that gets a new group number as well. That's
-- the difference between this technique and grouping by serial and
-- site.
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
;
