##
## Split rows of a data table
##
## Credit for this nice little trick goes to: https://stackoverflow.com/a/39221472/199166
##
library(data.table)


# Pharmaceuticals are annoyingly complex. One issue is that drugs come in combination
# products - one pill, two or more active ingredients. If we want to parse out who
# is taking what, we'll end up dealing with something like this:
drugs <- c('insulin', 'metformin', 'liraglutide', 'empagliflozin', 'sitagliptin',
           'insulin and liraglutide', 'sitagliptin and metformin')

# Let's say we have some patients taking some blood sugar medications. Note that we
# do not recommend a treatment plan based on randomly sampling drugs.
n <- 7
patient_ids = replicate(n, paste(sample(0:9, 8, replace=TRUE), collapse=''))
dt <- data.table(
    patient_id=sample(patient_ids, n, replace=TRUE),
    meds=sample(drugs, n, replace=TRUE))

message('\n\n--------- original data table ---------')
print(dt)

#    patient_id                      meds
# 1:   26485819 sitagliptin and metformin
# 2:   26485819               liraglutide
# 3:   82095628   insulin and liraglutide
# 4:   21009806                 metformin
# 5:   26485819               sitagliptin
# 6:   18647602               sitagliptin
# 7:   17484136               sitagliptin


# R's vectorized string split function returns a list of character vectors.
s = strsplit(dt$meds, ' and ', fixed=TRUE)

# We'll make clever use of the lengths of those vectors to pad out the new data table.
dt2 <- dt[rep(1:.N, lengths(s)), c(.(patient_id=patient_id, medication = unlist(s)))][order(patient_id, medication)]

message('\n\n--------- improved data table ---------')
print(dt2)

#    patient_id  medication
# 1:   17484136 sitagliptin
# 2:   18647602 sitagliptin
# 3:   21009806   metformin
# 4:   26485819 liraglutide
# 5:   26485819   metformin
# 6:   26485819 sitagliptin
# 7:   26485819 sitagliptin
# 8:   82095628     insulin
# 9:   82095628 liraglutide

