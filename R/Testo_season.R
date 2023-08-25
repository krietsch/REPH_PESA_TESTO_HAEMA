#========================================================================================================================
# TESTO variation within season
#========================================================================================================================

sapply( c('data.table', 'sdb','foreach', 'wadeR', 'sdbvis', 'ggplot2'),
        require, character.only = TRUE)


#-------------------------------------------------------------------------------------------------------------------------
# REPH within season data
#-------------------------------------------------------------------------------------------------------------------------

con = dbcon('jkrietsch', db = 'REPHatBARROW')  

# data
dc = dbq(con, 'select * FROM CAPTURES')
dt = dbq(con, 'select * FROM TESTO')
dbDisconnect(con)

# add date
dt[, date_ := as.Date(date_)]
dc[, date_ := as.Date(caught_time)]

dc = merge(dc[, !c('GnRH'), with = FALSE], dt[, .(ID, date_, GnRH, volume, T)], by = c('ID', 'date_'), all.x = TRUE)


# exclude samples with to small volume
d = dc[is.na(GnRH) & !is.na(T)] # excludes a lot of samples!
d[, v20 := ifelse(min(volume) > 20, TRUE, FALSE), by = ID]
# d = d[log(T) > 2.5]

# create date year
d[, date_y := as.POSIXct(paste0('2020-', format(date_, format = '%m-%d')))]

# plot data

# outlier with big volume - dead birds?
ggplot(d) +
  geom_point(aes(date_, log(T), color = volume)) +
  theme_classic(base_size = 18)

ggplot(d) +
  geom_boxplot(aes(sex_observed, log(T))) +
  theme_classic(base_size = 18)

ggplot(d) +
  geom_boxplot(aes(paste0(sex_observed, year_), log(T))) +
  theme_classic(base_size = 18)

ggplot(d) +
  geom_point(aes(date_y, log(T), color = sex_observed)) +
  theme_classic(base_size = 18)

ggplot() +
  geom_point(data = d[date_y < as.Date('2020-07-01')], aes(date_y, log(T), color = sex_observed)) +
  geom_smooth(data = d[sex_observed == 'M' & date_y < as.Date('2020-07-01')], aes(date_y, log(T)), method = 'lm', colour = 'black') +
  geom_smooth(data = d[sex_observed == 'F' & date_y < as.Date('2020-07-01')], aes(date_y, log(T)), method = 'lm', colour = 'black') +
  scale_color_manual(name = 'Sex', values = c('F' = 'firebrick3', 'M' = 'dodgerblue3')) +
  labs(x = 'Date', y = 'Log testosteron (ng/ml)') +
  theme_classic()

dR = d[, .(species = 'REPH', year_, date_, date_y, v20, sex_observed, T)]

#-------------------------------------------------------------------------------------------------------------------------
# PESA within season data
#-------------------------------------------------------------------------------------------------------------------------

con = dbcon('jkrietsch', db = 'PESAatBARROW')  

# data
dc = dbq(con, 'select * FROM CAPTURES')
dt = dbq(con, 'select * FROM TESTO')
dbDisconnect(con)

setnames(dc, c('start_capture_date_time'), c('caught_time'))
dc[, sex_observed := as.character(sex_observed)]
dc[sex_observed == 1, sex_observed := 'M']
dc[sex_observed == 2, sex_observed := 'F']

# add date
dt[, date_ := as.Date(date_)]
dc[, date_ := as.Date(caught_time)]

dc = merge(dc, dt[, .(ID, date_, GnRH, volume, T)], by = c('ID', 'date_'), all.x = TRUE)

# exclude samples with to small volume
d = dc[is.na(GnRH) & !is.na(T)] 
d[is.na(volume), v20 := TRUE] # not known
d[!is.na(volume), v20 := ifelse(min(volume) > 20, TRUE, FALSE), by = ID]
# d = d[log(T) > 2.5]

# create date year
d[, date_y := as.POSIXct(paste0('2020-', format(date_, format = '%m-%d')))]

# plot data

# outlier with big volume - dead birds?
ggplot(d) +
  geom_point(aes(date_, log(T), color = volume)) +
  theme_classic(base_size = 18)

ggplot(d) +
  geom_boxplot(aes(sex_observed, log(T))) +
  theme_classic(base_size = 18)

ggplot(d) +
  geom_boxplot(aes(paste0(sex_observed, year_), log(T))) +
  theme_classic(base_size = 18)

ggplot(d) +
  geom_point(aes(date_y, log(T), color = sex_observed)) +
  theme_classic(base_size = 18)

ggplot() +
  geom_point(data = d[date_y < as.Date('2020-07-01')], aes(date_y, log(T), color = sex_observed)) +
  geom_smooth(data = d[sex_observed == 'M' & date_y < as.Date('2020-07-01')], aes(date_y, log(T)), method = 'lm', colour = 'black') +
  geom_smooth(data = d[sex_observed == 'F' & date_y < as.Date('2020-07-01')], aes(date_y, log(T)), method = 'lm', colour = 'black') +
  scale_color_manual(name = 'Sex', values = c('F' = 'firebrick3', 'M' = 'dodgerblue3')) +
  labs(x = 'Date', y = 'Log testosteron (ng/ml)') +
  theme_classic()

dP = d[, .(species = 'PESA', year_, date_, date_y, v20, sex_observed, T)]

#-------------------------------------------------------------------------------------------------------------------------
# SESA within season data
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

# unique sex
dc = unique(dc, by = 'ID')
dc = dc[!is.na(ID)]

dc = merge(dc, ds[, .(ID = as.integer(ID), sex)], by = 'ID', all.x = TRUE)

dc[sex == 1, sex_genetic := 'M']
dc[sex == 2, sex_genetic := 'F']

dc = merge(dc, dt[, .(ID, date_time, testo)], by = c('ID'), all.x = TRUE)

ggplot(dc) +
  geom_boxplot(aes(sex_genetic, log(testo))) +
  theme_classic(base_size = 18)

dc[!is.na(testo), .N, by = sex_genetic]

#-------------------------------------------------------------------------------------------------------------------------
# REPH & PESA within season 
#-------------------------------------------------------------------------------------------------------------------------

d = rbind(dR, dP)

p = 
ggplot() +
  geom_point(data = d[date_y < as.Date('2020-07-01')], aes(date_y, log(T), color = sex_observed)) +
  geom_smooth(data = d[sex_observed == 'M' & date_y < as.Date('2020-07-01')], aes(date_y, log(T)), method = 'lm', colour = 'black') +
  geom_smooth(data = d[sex_observed == 'F' & date_y < as.Date('2020-07-01')], aes(date_y, log(T)), method = 'lm', colour = 'black') +
  scale_color_manual(name = 'Sex', values = c('F' = 'firebrick3', 'M' = 'dodgerblue3')) +
  labs(x = 'Date', y = 'Log testosteron (ng/ml)') +
  theme_bw(base_size = 24) +
  facet_grid( ~ species)

p

# png('./REPORTS/Testosterone_season_PESA_REPH.png', width = 1400, height = 1000)
# p
# dev.off()

# excluding samples with less than 20 microliter
d = d[v20 == TRUE]

p = 
  ggplot() +
  geom_point(data = d[date_y < as.Date('2020-07-01')], aes(date_y, log(T), color = sex_observed)) +
  geom_smooth(data = d[sex_observed == 'M' & date_y < as.Date('2020-07-01')], aes(date_y, log(T)), method = 'lm', colour = 'black') +
  geom_smooth(data = d[sex_observed == 'F' & date_y < as.Date('2020-07-01')], aes(date_y, log(T)), method = 'lm', colour = 'black') +
  scale_color_manual(name = 'Sex', values = c('F' = 'firebrick3', 'M' = 'dodgerblue3')) +
  labs(x = 'Date', y = 'Log testosteron (ng/ml)') +
  theme_bw(base_size = 24) +
  facet_grid( ~ species)

p

# png('./REPORTS/Testosterone_season_PESA_REPH_volume20.png', width = 1400, height = 1000)
# p
# dev.off()
