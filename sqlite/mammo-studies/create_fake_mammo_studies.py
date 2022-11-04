"""
Gin up some fake mammography data.

Pretend we are a large clinic with a panel of patients that are eligible for
screening mammograms periodically (every year? two years?) Sometimes, the
results of the study are suspicious, in which case our clinic schedules a
biopsy.

Could we train a model to predict, given the images from a screening mammo,
whether or not biopsy is indicated? To do that, we'd want to find biopsies
preceded by one or more screening studies.

This script generates a csv with columns: study_id, patient_id, study_date,
and notes. We could manipulate the data with Pandas or read it into sqlite and
do a little exercise in window functions.
"""
from collections import namedtuple
import random
import datetime as dt
import pandas as pd

class Patient:
    def __init__(self, patient_id, last_mammo):
        self.patient_id = patient_id
        self.last_mammo = last_mammo

patients = [
    Patient(900000000 + i, None)
    for i in range(10000)
]
eligible_patients = []
lost_patients = []
biopsy_patients = []

# screening every 2 years
interval = dt.timedelta(days=365*1)

def partition(patients, pred):
    return (
        [p for i,p in enumerate(patients) if pred(i,p)],
        [p for i,p in enumerate(patients) if not pred(i,p)],
    )

def find_eligible_patients(patients, today, interval):
    def is_past_interval(_, p):
        return p.last_mammo is None or today - p.last_mammo > interval
    return partition(patients, is_past_interval)


def in_sample(indices):
    def _in_sample(i, _):
        return i in indices
    return _in_sample


def todays_patients(patients):
    k = min(len(patients), random.randint(20,50))
    inds = random.sample(range(len(patients)), k)
    return partition(patients, in_sample(inds))


def todays_biopsies(patients):
    k = random.randint(0,min(2, len(patients)))
    inds = random.sample(range(len(patients)), k)
    return partition(patients, in_sample(inds))


class Sequence:
    def __init__(self, x):
        self.x = x
    def __call__(self) -> int:
        self.x += 1
        return self.x

nxt_study_id = Sequence(1000000)

#with open("data/mammo_studies.csv", "w") as f:
rows = []
for year in range(2016,2022):
    sd = f"{year}-01-02"
    ed = f"{year}-12-20"
    print(year)
    for d in pd.bdate_range(sd, ed):
        eps, patients = find_eligible_patients(patients, d, interval)
        eligible_patients.extend(eps)
        tps, eligible_patients = todays_patients(eligible_patients)
        tbs, biopsy_patients = todays_biopsies(biopsy_patients)

        # columns:
        #  study_id
        #  patient_id
        #  study_date
        #  notes

        for p in tps:
            
            # if a screening mammogram is suspicious, we'll biopsy
            sus = random.uniform(0, 1) < 0.02

            # if patients never come back, they are lost to follow-up
            lost = random.uniform(0, 1) < 0.05

            rows.append((nxt_study_id(), p.patient_id, d.strftime("%Y-%m-%d"), "screen"))
            #print(nxt_study_id(), p.patient_id, d.strftime("%Y-%m-%d"), "screen", sep=",", file=f)
            p.last_mammo = d
            if sus:
                biopsy_patients.append(p)
            elif lost:
                lost_patients.append(p)
            else:
                patients.append(p)
        for p in tbs:
            rows.append((nxt_study_id(), p.patient_id, d.strftime("%Y-%m-%d"), "biopsy"))
            #print(nxt_study_id(), p.patient_id, d.strftime("%Y-%m-%d"), "biopsy", sep=",", file=f)
            patients.append(p)

df = pd.DataFrame(rows, columns=["study_id", "patient_id", "study_date", "notes"])
df.to_csv("data/mammo_studies.csv", index=False)
