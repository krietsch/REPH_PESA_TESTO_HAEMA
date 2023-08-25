#========================================================================================================================
# TESTO GnRH injections
#========================================================================================================================

sapply( c('data.table', 'sdb','foreach', 'wadeR', 'sdbvis'),
        require, character.only = TRUE)

#-------------------------------------------------------------------------------------------------------------------------
# REPH GnRH experiment
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

IDe = dc[!is.na(GnRH)]$ID

d = dc[!is.na(T) & ID %in% IDe]
dn = dc[!is.na(T) & !(ID %in% IDe)] # non experimental birds
dn[, v20 := ifelse(volume > 20, TRUE, FALSE), by = 1:nrow(dn)]

# exclude third testo sample
d = d[!(ID == 270170318	& date_ == '2017-06-01')]

# sample type
d[, GnRH_sample := ifelse(is.na(GnRH), 'start', 'after_30min')]
d[, v20 := ifelse(min(volume) > 20, TRUE, FALSE), by = ID]

ID_l = d[GnRH == 'low']$ID
d[ID %in% ID_l, GnRH := 'low']

ID_h = d[GnRH == 'high']$ID
d[ID %in% ID_h, GnRH := 'high']

dn[, GnRH_sample := 'none']
dn[, GnRH := 'none']

d = rbind(d, dn)

# order treatment
d$GnRH_sample = factor(d$GnRH_sample, levels = c('none', 'start', 'after_30min'))

d[, GnRH_sex := paste0(sex_observed, '_', GnRH)]
d$GnRH_sex = factor(d$GnRH_sex, levels = c('F_none', 'F_low', 'F_high', 'M_none', 'M_low', 'M_high'))

unique(d$GnRH_sex)

dS = d[, .(N = .N / 2), by = .(GnRH_sex)]
dS[GnRH_sex == 'F_none', N := N * 2]
dS[GnRH_sex == 'M_none', N := N * 2]

p = 
  ggplot(d) + 
  geom_boxplot(aes(y = log(T), x = GnRH_sex, fill = GnRH_sample)) +
  scale_fill_manual(name = 'Treatment', values = c('none' = 'white', 'after_30min' = 'grey40', 'start' = 'grey80')) +
  labs(x = 'GnRH treatment', y = 'Log testosteron (ng/ml)') +
  scale_x_discrete(labels = c('Females', 'Females_low', 'Females_high', 'Males', 'Males_low', 'Males_high')) + 
  geom_text(data = dS, aes(x = GnRH_sex, y = -0.3, label = N), size = 8) +
  theme_bw(base_size = 24)

p

# png('./REPORTS/GnRH_treatment_REPH.png', width = 1000, height = 800)
# p
# dev.off()


# # Correlation between Testo before and after GnRH
# 
# d[, con30 := con[GnRH_sample == 'after_30min']]
# 
# plot(con30 ~ con, d[GnRH_sample == 'start'])
# 
# p = 
#   ggplot(d[GnRH_sample == 'start']) +
#   geom_point(aes(y = con30, x = con, color = GnRH_sex), size = 5) +
#   scale_color_manual(name = 'GnRH_sex', values = c('F_l' = 'firebrick', 'F_h' = 'firebrick4', 
#                                                    'M_l' = 'dodgerblue', 'M_h' = 'dodgerblue4')) +
#   labs(x = 'Testo at capture (ng/ml)', y = 'Testo after 30 min GnRH (ng/ml)') +
#   theme_bw(base_size = 24)
# 
# p
# 
# # png('GnRH_Testo_before_after_GnRH.png', width = 1000, height = 800)
# # p
# # dev.off()


dR = d[, .(species = 'REPH', ID, year_, date_, sex = sex_observed, GnRH, volume, v20, GnRH_sample, GnRH_sex, T)]


#-------------------------------------------------------------------------------------------------------------------------
# PESA GnRH experiment
#-------------------------------------------------------------------------------------------------------------------------

con = dbcon('jkrietsch', db = 'PESAatBARROW')  

# data
dc = dbq(con, 'select * FROM CAPTURES')
dt = dbq(con, 'select * FROM TESTO')
dbDisconnect(con)

setnames(dc, c('start_capture_date_time'), c('caught_time'))

# add date
dt[, date_ := as.Date(date_)]
dc[, date_ := as.Date(caught_time)]

dc = merge(dc, dt[, .(ID, date_, GnRH, volume, T)], by = c('ID', 'date_'), all.x = TRUE)

IDe = dc[!is.na(GnRH)]$ID

d = dc[!is.na(T) & ID %in% IDe]
dn = dc[!is.na(T) & !(ID %in% IDe)] # non experimental birds
dn[, v20 := TRUE] # not known

# sample type
d[, GnRH_sample := ifelse(is.na(GnRH), 'start', 'after_30min')]
d[, v20 := ifelse(min(volume) > 20, TRUE, FALSE), by = ID]


ID_l = d[GnRH == 'low']$ID
d[ID %in% ID_l, GnRH := 'low']

ID_h = d[GnRH == 'high']$ID
d[ID %in% ID_h, GnRH := 'high']

dn[, GnRH_sample := 'none']
dn[, GnRH := 'none']

d = rbind(d, dn)

# order treatment
d$GnRH_sample = factor(d$GnRH_sample, levels = c('none', 'start', 'after_30min'))
d[, sex_observed := as.character(sex_observed)]
d[sex_observed == 1, sex_observed := 'M']
d[sex_observed == 2, sex_observed := 'F']

d[, GnRH_sex := paste0(sex_observed, '_', GnRH)]
d$GnRH_sex = factor(d$GnRH_sex, levels = c('F_none', 'F_low', 'F_high', 'M_none', 'M_low', 'M_high'))

unique(d$GnRH_sex)

dS = d[, .(N = .N / 2), by = .(GnRH_sex)]
dS[GnRH_sex == 'F_none', N := N * 2]
dS[GnRH_sex == 'M_none', N := N * 2]


p = 
  ggplot(d) + 
  geom_boxplot(aes(y = log(T), x = GnRH_sex, fill = GnRH_sample)) +
  scale_fill_manual(name = 'Treatment', values = c('none' = 'white', 'after_30min' = 'grey40', 'start' = 'grey80')) +
  labs(x = 'GnRH treatment', y = 'Log testosteron (ng/ml)') +
  scale_x_discrete(labels = c('Females', 'Females_low', 'Females_high', 'Males', 'Males_low', 'Males_high')) + 
  geom_text(data = dS, aes(x = GnRH_sex, y = -0.3, label = N), size = 8) +
  theme_bw(base_size = 24)

p

# png('./REPORTS/GnRH_treatment_PESA.png', width = 1000, height = 800)
# p
# dev.off()

dP = d[, .(species = 'PESA', ID, year_, date_, sex = sex_observed, GnRH, volume, v20, GnRH_sample, GnRH_sex, T)]

#-------------------------------------------------------------------------------------------------------------------------
# REPH & PESA GnRH experiment
#-------------------------------------------------------------------------------------------------------------------------

d = rbind(dR, dP)

# exclude no treatmeant data
d = d[GnRH_sample != 'none']

# time of injections
d[, date_y := as.POSIXct(paste0('2020-', format(date_, format = '%m-%d')))]
d[, min(date_y), by = .(species, GnRH_sample)]
d[, max(date_y), by = .(species, GnRH_sample)]

# all samples
dS = d[, .(N = .N / 2), by = .(species, GnRH_sex)]

p = 
  ggplot(d) + 
  geom_boxplot(aes(y = log(T), x = GnRH_sex, fill = GnRH_sample, alpha = 0.5), show.legend = FALSE) +
  geom_point(aes(y = log(T), x = GnRH_sex, fill = GnRH_sample), position = position_dodge(width = 0.75)) +
  geom_line(aes(y = log(T), x = GnRH_sex, group = ID), colour = 'grey') +
  scale_fill_manual(name = 'Treatment', values = c('after_30min' = 'grey40', 'start' = 'grey80')) +
  labs(x = 'GnRH treatment', y = 'Log testosteron (ng/ml)') +
  scale_x_discrete(labels = c('F_low', 'F_high', 'M_low', 'M_high')) + 
  geom_text(data = dS, aes(x = GnRH_sex, y = -0.3, label = N), size = 8) +
  theme_bw(base_size = 24) +
  facet_grid( ~ species)

p




# png('./REPORTS/GnRH_treatment_PESA_REPH.png', width = 1800, height = 1000)
# p
# dev.off()


# excluding samples with less than 20 microliter
d = d[v20 == TRUE]

dS = d[, .(N = .N / 2), by = .(species, GnRH_sex)]
dS[like(GnRH_sex, 'none') , N := N * 2]

p = 
  ggplot(d) + 
  geom_boxplot(aes(y = log(T), x = GnRH_sex, fill = GnRH_sample)) +
  scale_fill_manual(name = 'Treatment', values = c('none' = 'white', 'after_30min' = 'grey40', 'start' = 'grey80')) +
  labs(x = 'GnRH treatment', y = 'Log testosteron (ng/ml)') +
  scale_x_discrete(labels = c('Females', 'F_low', 'F_high', 'Males', 'M_low', 'M_high')) + 
  geom_text(data = dS, aes(x = GnRH_sex, y = -0.3, label = N), size = 8) +
  theme_bw(base_size = 24) +
  facet_grid( ~ species)

p


# png('./REPORTS/GnRH_treatment_PESA_REPH_volume20.png', width = 1800, height = 1000)
# p
# dev.off()





