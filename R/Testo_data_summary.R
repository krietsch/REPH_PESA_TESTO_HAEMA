#========================================================================================================================
# Summary of testo data from REPH, PESA and SESA
#========================================================================================================================

sapply( c('data.table', 'magrittr', 'ggplot2', 'knitr'),
        require, character.only = TRUE)

# load data
# source('./R/Testo_data_merge.R')
d = readRDS('./DATA/REPH_PESA_SESA_testosterone.RDS')

# Lines to run to create html output
opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())
# rmarkdown::render('./R/Testo_data_summary.R', output_dir = './OUTPUTS/R_COMPILED')


#-------------------------------------------------------------------------------------------------------------------------
# Check data
#-------------------------------------------------------------------------------------------------------------------------

# create date year
d[, date_y := as.POSIXct(paste0('2020-', format(date_, format = '%m-%d')))]

# bleeding time
d[, caught_time := as.POSIXct(caught_time)]
d[, bled_time := as.POSIXct(bled_time)]
d[, diff_caught_bled := difftime(bled_time, caught_time, units = 'mins') %>% as.numeric]

ggplot() +
  geom_histogram(data = d, aes(diff_caught_bled)) +
  theme_classic(base_size = 18)

setorder(d, diff_caught_bled)
d

# testo data
d[is.na(GnRH), .N, .(species, sex_genetic)]
d[is.na(GnRH), .N, .(year_, species, sex_genetic)]

# GnRH data
d[!is.na(GnRH), .N, .(species, sex_genetic)]

# volume
d[is.na(volume), .N, species]
d[!is.na(volume), .N, species]

ggplot() +
  geom_histogram(data = d, aes(volume)) +
  theme_classic(base_size = 18)

#-------------------------------------------------------------------------------------------------------------------------
# Between species data
#-------------------------------------------------------------------------------------------------------------------------

# exclude GnRH induced samples
ds = d[is.na(GnRH)]

ds[, species_sex := paste0(species, '_', sex_genetic)]
dS = ds[, .(N = .N), by = .(species_sex)]

# factor order
ds$species_sex %>% unique
ds$species_sex = factor(ds$species_sex, levels = c('PESA_F', 'SESA_F', 'REPH_F', 'PESA_M', 'SESA_M', 'REPH_M', 'SESA_NA'))

ggplot(ds) +
  geom_boxplot(aes(species_sex, log(testo)), notch = TRUE) +
  geom_text(data = dS, aes(x = species_sex, y = -0.3, label = N), size = 8) +
  theme_classic(base_size = 18)

#-------------------------------------------------------------------------------------------------------------------------
# Within season data
#-------------------------------------------------------------------------------------------------------------------------

# PESA
ggplot() +
  geom_point(data = ds[species == 'PESA'], aes(date_y, log(testo), color = sex_genetic)) +
  geom_smooth(data = ds[species == 'PESA' & sex_genetic == 'M'], aes(date_y, log(testo)), method = 'lm', colour = 'dodgerblue3') +
  geom_smooth(data = ds[species == 'PESA' & sex_genetic == 'F'], aes(date_y, log(testo)), method = 'lm', colour = 'firebrick3') +
  scale_color_manual(name = 'Sex', values = c('F' = 'firebrick3', 'M' = 'dodgerblue3')) +
  labs(x = 'Date', y = 'Log testosteron (ng/ml)') +
  theme_classic(base_size = 18)

# REPH
ggplot() +
  geom_point(data = ds[species == 'REPH'], aes(date_y, log(testo), color = sex_genetic)) +
  geom_smooth(data = ds[species == 'REPH' & sex_genetic == 'M'], aes(date_y, log(testo)), method = 'lm', colour = 'dodgerblue3') +
  geom_smooth(data = ds[species == 'REPH' & sex_genetic == 'F'], aes(date_y, log(testo)), method = 'lm', colour = 'firebrick3') +
  scale_color_manual(name = 'Sex', values = c('F' = 'firebrick3', 'M' = 'dodgerblue3')) +
  labs(x = 'Date', y = 'Log testosteron (ng/ml)') +
  theme_classic(base_size = 18)

# SESA
ggplot() +
  geom_point(data = ds[species == 'SESA'], aes(date_y, log(testo), color = sex_genetic)) +
  geom_smooth(data = ds[species == 'SESA' & sex_genetic == 'M'], aes(date_y, log(testo)), method = 'lm', colour = 'dodgerblue3') +
  geom_smooth(data = ds[species == 'SESA' & sex_genetic == 'F'], aes(date_y, log(testo)), method = 'lm', colour = 'firebrick3') +
  scale_color_manual(name = 'Sex', values = c('F' = 'firebrick3', 'M' = 'dodgerblue3')) +
  labs(x = 'Date', y = 'Log testosteron (ng/ml)') +
  theme_classic(base_size = 18)

# by species
ggplot() +
  geom_point(data = ds, aes(date_y, log(testo), color = sex_genetic)) +
  geom_smooth(data = ds[sex_genetic == 'M'], aes(date_y, log(testo)), method = 'lm', colour = 'dodgerblue3') +
  geom_smooth(data = ds[sex_genetic == 'F'], aes(date_y, log(testo)), method = 'lm', colour = 'firebrick3') +
  scale_color_manual(name = 'Sex', values = c('F' = 'firebrick3', 'M' = 'dodgerblue3')) +
  labs(x = 'Date', y = 'Log testosteron (ng/ml)') +
  theme_classic(base_size = 18) +
  facet_grid(rows = vars(species))

# by sex
ggplot() +
  geom_point(data = ds, aes(date_y, log(testo), color = species)) +
  geom_smooth(data = ds[species == 'PESA'], aes(date_y, log(testo)), method = 'lm', colour = 'dodgerblue3') +
  geom_smooth(data = ds[species == 'REPH'], aes(date_y, log(testo)), method = 'lm', colour = 'firebrick3') +
  geom_smooth(data = ds[species == 'SESA'], aes(date_y, log(testo)), method = 'lm', colour = 'darkorange') +
  scale_color_manual(name = 'Species', values = c('REPH' = 'firebrick3', 'PESA' = 'dodgerblue3', 'SESA' = 'darkorange')) +
  labs(x = 'Date', y = 'Log testosteron (ng/ml)') +
  theme_classic(base_size = 18) +
  facet_grid(rows = vars(sex_genetic))


#-------------------------------------------------------------------------------------------------------------------------
# GnRH experiments
#-------------------------------------------------------------------------------------------------------------------------

IDe = d[!is.na(GnRH)]$ID

ds = d[ID %in% IDe]
dn = d[!(ID %in% IDe)] # non experimental birds
dn[, v20 := ifelse(volume > 20, TRUE, FALSE), by = 1:nrow(dn)]

# exclude third testo sample
ds = ds[!(ID == 270170318	& date_ == '2017-06-01')]

# sample type
ds[, GnRH_sample := ifelse(is.na(GnRH), 'start', 'after_30min')]
ds[, v20 := ifelse(min(volume) > 20, TRUE, FALSE), by = ID]

ID_l = ds[GnRH == 'low']$ID
ds[ID %in% ID_l, GnRH := 'low']

ID_h = ds[GnRH == 'high']$ID
ds[ID %in% ID_h, GnRH := 'high']

dn[, GnRH_sample := 'none']
dn[, GnRH := 'none']

ds = rbind(ds, dn)

# order treatment
ds$GnRH_sample = factor(ds$GnRH_sample, levels = c('none', 'start', 'after_30min'))

ds[, GnRH_sex := paste0(sex_observed, '_', GnRH)]
ds$GnRH_sex = factor(ds$GnRH_sex, levels = c('F_none', 'F_low', 'F_high', 'M_none', 'M_low', 'M_high'))

unique(ds$GnRH_sex)

ds = ds[species != 'SESA']
dS = ds[, .(N = .N / 2), by = .(species, GnRH_sex)]
dS[like(GnRH_sex, 'none') , N := N * 2]

p = 
  ggplot(ds) + 
  geom_boxplot(aes(y = log(testo), x = GnRH_sex, fill = GnRH_sample)) +
  scale_fill_manual(name = 'Treatment', values = c('none' = 'white', 'after_30min' = 'grey40', 'start' = 'grey80')) +
  labs(x = 'GnRH treatment', y = 'Log testosteron (ng/ml)') +
  scale_x_discrete(labels = c('Females', 'F_low', 'F_high', 'Males', 'M_low', 'M_high')) + 
  geom_text(data = dS, aes(x = GnRH_sex, y = -0.3, label = N), size = 3) +
  theme_bw(base_size = 10) +
  facet_grid( ~ species)

p


# ggsave('./OUTPUTS/FIGURES/GnRH_experiment.tiff', plot = p,  width = 177, height = 89, units = c('mm'), 
#        dpi = 'print')

