-- sqlite3

create table if not exists studies (
    study_id int primary key,
    patient_id int,
    study_date text,
    notes text
);

-- .import --skip 1 --csv data/mammo_studies.csv studies

-- histogram of number of studies per patient
with counts as (
  select
    patient_id,
    count(*) as visits
  from
    studies
  group by
    patient_id 
)
select
  visits,
  count(*) as n
from
  counts
group by visits;


-- count the number of screenings prior to a biopsy for each
-- patient who had a biopsy
select
  study_id,
  patient_id,
  study_date,
  notes,
  count()
  FILTER (WHERE notes = 'screen')
  over (
    partition by patient_id
    order by study_date
    rows between unbounded preceding and 1 preceding
  ) as prior_screens
from
  studies
where
  patient_id < 900000020


-- find patients who had a biopsy with at least 3 prior screening mammograms
with biopsy_with_prior_screen as (
  select
    study_id,
    patient_id,
    study_date,
    notes,
    count()
    filter (where notes = 'screen')
    over (
      partition by patient_id
      order by study_date
      rows between unbounded preceding and 1 preceding
    ) as prior_screens
  from
    studies
)
select
  count(*)
from
  biopsy_with_prior_screen
where
  notes = 'biopsy'
and
  prior_screens >= 3
;
