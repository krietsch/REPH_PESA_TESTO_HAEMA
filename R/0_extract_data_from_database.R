#==============================================================================================================
# Data and code from ""
# Contributor: Johannes Krietsch
# ❗This script is provided as reference only. It contains links to the internal database of the Max Planck 
# Institute for Ornithology, from which it pulls the data and exports all the collected data to ./DATA
#==============================================================================================================

sapply( c('data.table', 'sdb', 'magrittr', 'ggplot2', 'DBI'),
        require, character.only = TRUE)

#--------------------------------------------------------------------------------------------------------------
# REPH data
#--------------------------------------------------------------------------------------------------------------

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
dc[, caught_time := as.POSIXct(caught_time)]
dc[, bled_time := as.POSIXct(bled_time)]

dc = merge(dc[, !c('GnRH'), with = FALSE], dt[, .(ID, date_, GnRH, volume, T)], by = c('ID', 'date_'), 
           all.x = TRUE)

# exclude dead or injured birds
dc[is.na(dead), dead := 0]
dc = dc[dead != 1]

dR = dc[!is.na(T), .(species = 'REPH', ID, date_, caught_time, bled_time, sex = sex, tarsus, wing, 
                     weight, testo = T, volume, GnRH, haema)]

#--------------------------------------------------------------------------------------------------------------
# PESA data
#--------------------------------------------------------------------------------------------------------------

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

dc[, sex_observed := as.character(sex_observed)]
dc[sex_observed == 1, sex_observed := 'M']
dc[sex_observed == 2, sex_observed := 'F']

# add date
dc[is.na(start_capture_date_time), start_capture_date_time := caught_date_time]
dt[, date_ := as.Date(date_)]
dc[, date_ := as.Date(start_capture_date_time)]


dc = merge(dc, dt[, .(ID, date_, GnRH, volume, T)], by = c('ID', 'date_'), all.x = TRUE)

# exclude dead or injured birds
dc[is.na(dead), dead := 0]
dc = dc[dead != 1]

# NA if caught equals bleed time & > 40 min
dc[caught_date_time == bled_date_time, bled_date_time := NA]

# bleeding time
dc[, caught_date_time := as.POSIXct(caught_date_time)]
dc[bled_date_time == '0000-00-00 00:00:00', bled_date_time := NA]
dc[, bled_date_time := as.POSIXct(bled_date_time)]
dc[, diff_caught_bled := difftime(bled_date_time, caught_date_time, units = 'mins') %>% as.numeric]
dc[diff_caught_bled > 30, bled_date_time := NA] # excluded mistakes in the field

# remove unrealistic measure 
dc[ID == 250103853, wing := NA]

dP = dc[!is.na(T), .(species = 'PESA', ID, date_, caught_time = caught_date_time, bled_time = bled_date_time, 
                     sex = sex_genetic, tarsus, wing, weight, testo = T, volume, GnRH, haema = hematocrit)]


#--------------------------------------------------------------------------------------------------------------
# merge species
#--------------------------------------------------------------------------------------------------------------

d = rbindlist(list(dR, dP))
d[, year_ := year(date_)]

# check outliers

# tarsus
ggplot(data = d) +
  geom_histogram(aes(tarsus, fill = species))

ggplot(data = d[species == 'PESA']) +
  geom_histogram(aes(tarsus, fill = sex))

ggplot(data = d[species == 'REPH']) +
  geom_histogram(aes(tarsus, fill = sex))

# wing
ggplot(data = d) +
  geom_histogram(aes(wing, fill = species))

ggplot(data = d[species == 'PESA']) +
  geom_histogram(aes(wing, fill = sex))

ggplot(data = d[species == 'REPH']) +
  geom_histogram(aes(wing, fill = sex))

# weight
ggplot(data = d) +
  geom_histogram(aes(weight, fill = species))

ggplot(data = d[species == 'PESA']) +
  geom_histogram(aes(weight, fill = sex))

ggplot(data = d[species == 'REPH']) +
  geom_histogram(aes(weight, fill = sex))


saveRDS(d, './DATA/REPH_PESA_testosterone.RDS')
