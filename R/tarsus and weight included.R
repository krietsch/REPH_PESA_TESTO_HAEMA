# influence of weight and tarsus

d[mean(tarsus), by = ID]

d[, tarus_mean_ID := mean(tarsus, na.rm = TRUE), by = ID]
d[, tarus_mean_sp := mean(tarus_mean_ID, na.rm = TRUE), by = .(species, sex)]
d[, tarsus_rel := tarsus - tarus_mean_sp]


d[, weight_mean_sp := mean(weight, na.rm = TRUE), by = .(species, sex)]
d[, weight_rel := weight - weight_mean_sp]






# exclude GnRH induced samples
ds = d[is.na(GnRH)]

##### subset males
dm = ds[sex == 'M']

# sample size
dms = dm[, .N, by = species]
du = unique(dm, by = 'ID')
du = du[, .(N_ind = .N), by = species]
dms = merge(dms, du, by = 'species')
dms[, sample_size := paste0('N = ', N, ' | ', N_ind)]


# model 
m <- glmmTMB(testo_log ~ species + poly(date_doy,2) + tarsus_rel + weight_rel + (1 | year_) + (1 | ID),
             family = gaussian(link = "identity"), 
             data = dm,
             control = glmmTMBControl(parallel = 15)
)

plot(allEffects(m))
summary(m)









###### subset females
df = ds[sex == 'F']

# sample size
dfs = df[, .N, by = species]
du = unique(df, by = 'ID')
du = du[, .(N_ind = .N), by = species]
dfs = merge(dfs, du, by = 'species')
dfs[, sample_size := paste0('N = ', N, ' | ', N_ind)]

# model 
m <- glmmTMB(testo_log ~ species + date_doy + tarsus_rel + weight_rel + (1 | year_) + (1 | ID),
             family = gaussian(link = "identity"), 
             data = df,
             control = glmmTMBControl(parallel = 15)
)

plot(allEffects(m))
summary(m)











#--------------------------------------------------------------------------------------------------------------
# Testosterone influence on haematocrit
#--------------------------------------------------------------------------------------------------------------




# exclude GnRH induced samples
ds = d[is.na(GnRH)]

# exclude NA
ds = ds[!is.na(haema)]
ds[haema == 85, haema := 58]


# PESA
dss = ds[species == 'PESA']

# sample size
dsss = dss[, .N, by = sex]
du = unique(dss, by = 'ID')
du = du[, .(N_ind = .N), by = sex]
dsss = merge(dsss, du, by = 'sex')
dsss[, sample_size := paste0('N = ', N, ' | ', N_ind)]

# model
m <- glmmTMB(haema ~ sex * date_doy + sex * testo_log  + tarsus_rel + weight_rel  + (1 | year_) + (1 | ID),
             family = gaussian(link = "identity"), 
             data = dss,
             control = glmmTMBControl(parallel = 15)
)

plot(allEffects(m))
summary(m)










# REPH
dss = ds[species == 'REPH']

# sample size
dsss = dss[, .N, by = sex]
du = unique(dss, by = 'ID')
du = du[, .(N_ind = .N), by = sex]
dsss = merge(dsss, du, by = 'sex')
dsss[, sample_size := paste0('N = ', N, ' | ', N_ind)]

# model
m <- glmmTMB(haema ~ sex + date_doy + testo_log + tarsus_rel + weight_rel + (1 | year_) + (1 | ID),
             family = gaussian(link = "identity"), 
             data = dss,
             control = glmmTMBControl(parallel = 15)
)

plot(allEffects(m))
summary(m)






