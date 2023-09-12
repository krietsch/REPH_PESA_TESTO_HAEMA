#==============================================================================================================
# Data and code from ""
# Contributor: Johannes Krietsch
# ❗This script is provided as reference only. It contains links to the internal database of the Max Planck 
# Institute for Ornithology, from which it pulls the data and exports all the collected data to ./DATA
#==============================================================================================================

sapply( c('data.table', 'sdb', 'magrittr', 'ggplot2', 'DBI'),
        require, character.only = TRUE)

#-------------------------------------------------------------------------------------------------------------------------
# REPH data
#-------------------------------------------------------------------------------------------------------------------------

con = dbcon('jkrietsch', db = 'REPHatBARROW')  

# data
dc = dbq(con, 'select * FROM CAPTURES')
dt = dbq(con, 'select * FROM TESTO')
ds = dbq(con, 'select * FROM SEX')
dbDisconnect(con)

# merge with lab sex
dc = merge(dc, ds[, .(ID = as.integer(ID), sex)], by = 'ID', all.x = TRUE)

# add date
dt[, date_ := as.Date(date_)]
dc[, date_ := as.Date(caught_time)]

dc = merge(dc[, !c('GnRH'), with = FALSE], dt[, .(ID, date_, GnRH, volume, T)], by = c('ID', 'date_'), all.x = TRUE)

dR = dc[!is.na(T), .(species = 'REPH', ID, date_, caught_time, bled_time, sex = sex, 
                     testo = T, volume, GnRH, haema)]

#-------------------------------------------------------------------------------------------------------------------------
# PESA data
#-------------------------------------------------------------------------------------------------------------------------

con = dbcon('jkrietsch', db = 'PESAatBARROW')  

# data
dc = dbq(con, 'select * FROM CAPTURES')
dt = dbq(con, 'select * FROM TESTO')
ds = dbq(con, 'select * FROM SEX')
dbDisconnect(con)

# merge with lab sex
dc = merge(dc, ds[, .(ID = as.integer(ID), sex)], by = 'ID', all.x = TRUE)
dc[sex == 1, sex_genetic := 'M']
dc[sex == 2, sex_genetic := 'F']

setnames(dc, c('start_capture_date_time'), c('caught_time'))
dc[, sex_observed := as.character(sex_observed)]
dc[sex_observed == 1, sex_observed := 'M']
dc[sex_observed == 2, sex_observed := 'F']

# add date
dt[, date_ := as.Date(date_)]
dc[, date_ := as.Date(caught_time)]

dc = merge(dc, dt[, .(ID, date_, GnRH, volume, T)], by = c('ID', 'date_'), all.x = TRUE)

dP = dc[!is.na(T), .(species = 'PESA', ID, date_, caught_time = caught_date_time, bled_time = bled_date_time, 
                     sex = sex_genetic, testo = T, volume, GnRH, haema = hematocrit)]

#-------------------------------------------------------------------------------------------------------------------------
# SESA data
#-------------------------------------------------------------------------------------------------------------------------

con = dbcon('jkrietsch', db = 'EXTRA_AVESatBARROW')  

# data
dt = dbq(con, 'select * FROM 2004_HORMONES')
dbDisconnect(con)

con = dbcon('jkrietsch', db = 'AVESatBARROW')  

# data
dc = dbq(con, 'select * FROM CAPTURES')
ds = dbq(con, 'select * FROM SEX')
dbDisconnect(con)

# add date
dt[, date_ := as.Date(date_time)]
dc[, date_ := as.Date(caught_date_time)]

# exclude NA
dc = dc[!is.na(ID)]
ds[, ID := as.integer(ID)]
ds = ds[!is.na(ID)]

# merge with lab sex
dc = merge(dc, ds[, .(ID = as.integer(ID), sex)], by = 'ID', all.x = TRUE)
dc[sex == 1, sex_genetic := 'M']
dc[sex == 2, sex_genetic := 'F']

# merge with testo
dc = merge(dc, dt[, .(ID, date_, GnRH = NA, volume = NA, testo = testo_1)], by = c('ID', 'date_'), all.x = TRUE)

dS = dc[!is.na(testo), .(species = 'SESA', ID, date_, caught_time = caught_date_time, bled_time = bled_date_time, 
                         sex = sex_genetic, testo, volume = NA, GnRH = NA, haema = hematocrit)]

#-------------------------------------------------------------------------------------------------------------------------
# merge species
#-------------------------------------------------------------------------------------------------------------------------

d = rbindlist(list(dR, dP, dS))
d[, year_ := year(date_)]

saveRDS(d, './DATA/REPH_PESA_SESA_testosterone.RDS')
