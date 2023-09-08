#' ---
#' title: Figures and Statistic
#' subtitle: 
#' author: Johannes Krietsch
#' output:
#'    html_document:
#'      toc: true
#'      highlight: tango
#' ---

sapply( c('data.table', 'magrittr', 'ggplot2', 'knitr', 'glmmTMB', 'emmeans', 'effects', 'broomExtra',
          'flextable', 'officer', 'DHARMa', 'ggparl', 'patchwork'),
        require, character.only = TRUE)

# load data
# source('./R/Testo_data_merge.R')
d = readRDS('./DATA/REPH_PESA_SESA_testosterone.RDS')

# Lines to run to create html output
opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())
# rmarkdown::render('./R/1_figures_and_statistics.R', output_dir = './OUTPUTS/R_COMPILED')

d[, testo := testo/1000] # pg/ml to ng/ml
d[, testo_log := log10(testo)]

# bleeding time
d[, caught_time := as.POSIXct(caught_time)]
d[, bled_time := as.POSIXct(bled_time)]
d[, diff_caught_bled := difftime(bled_time, caught_time, units = 'mins') %>% as.numeric]

# data as Julian
d[, date_doy := yday(date_)]

# year as character
d[, year_ := as.character(year_)]

d[, .N, by = .(species, year_)]

# factor order
d[, species := factor(species, levels = c('PESA', 'SESA', 'REPH'))]

# min max scale
d[, .(min(date_doy), max(date_doy))]
d[, .(min(testo), max(testo))]

#--------------------------------------------------------------------------------------------------------------
# Between species comparison
#--------------------------------------------------------------------------------------------------------------

# exclude GnRH induced samples
ds = d[is.na(GnRH)]

# subset males
dm = ds[sex == 'M']

# sample size
dms = dm[, .N, by = species]
du = unique(dm, by = 'ID')
du = du[, .(N_ind = .N), by = species]
dms = merge(dms, du, by = 'species')
dms[, sample_size := paste0('N = ', N, ' | ', N_ind)]

# model
m <- glmmTMB(testo_log ~ species * poly(date_doy,2) + (1 | year_) + (1 | ID),
             family = gaussian(link = "identity"), 
             data = dm,
             control = glmmTMBControl(parallel = 15)
)

# plot(allEffects(m))
summary(m)

emmeans(m, pairwise ~ species)

# res <-simulateResiduals(m, plot = T)
# testDispersion(res) 

# extract season effect from model for plot
es = effect("species:poly(date_doy, 2)", m, xlevels = 1000) |>
  data.frame() |>
  setDT()

# subset period with data
dr = dm[, .(first_data = min(date_doy), last_data = max(date_doy)), by = species]
es = merge(es, dr, by = c('species'), all.x = TRUE)
es[, in_range := date_doy %between% c(first_data, last_data), by = 1:nrow(es)]
es = es[in_range == TRUE]


e = effect("species", m, xlevels = 3) |>
  data.frame() |>
  setDT()



# plot for males
p1 =
ggplot() +
  ggtitle('Males') + 
  geom_boxjitter(data = dm, aes(species, testo, fill = species), outlier.color = NA, jitter.shape = 21, jitter.color = NA, 
                 jitter.height = 0.0, jitter.width = 0.1, errorbar.draw = TRUE, jitter.size = 0.7, width = .6) +
  scale_fill_manual(values = c("steelblue4", "#E69F00", 'indianred3')) +
  geom_text(data = dms, aes(species, Inf, label = sample_size), vjust = 1, size = 2.5) +
  scale_y_log10(limits = c(0.005, 350),
                breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks(sides = "l") +  
  theme_classic(base_size = 10) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 10, face = "bold")) +
  ylab('Testosteron (ng/ml)') +
  xlab('')


# effect of season
p3 =
  ggplot() +
  geom_point(data = dm, aes(date_doy, testo, color = species), size = 0.5) +
  geom_line(data = es, aes(y = exp(fit), x = date_doy, color = species), size = 0.8) +
  geom_ribbon(data = es, aes(y = exp(fit), x = date_doy, fill = species, ymin = exp(lower), ymax = exp(upper)), alpha = 0.2) +
  scale_color_manual(values = c("steelblue4", "#E69F00", 'indianred3')) +
  scale_fill_manual(values = c("steelblue4", "#E69F00", 'indianred3')) +
  scale_y_log10(limits = c(0.005, 350),
                breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks(sides = "l") +  
  scale_x_continuous(limits = c(140, 206), expand = expansion(add = c(0, 0))) +
  theme_classic(base_size = 10) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ylab('Testosteron (ng/ml)') +
  xlab('Day of the year')



# subset females
df = ds[sex == 'F']

# sample size
dfs = df[, .N, by = species]
du = unique(df, by = 'ID')
du = du[, .(N_ind = .N), by = species]
dfs = merge(dfs, du, by = 'species')
dfs[, sample_size := paste0('N = ', N, ' | ', N_ind)]

# model
m <- glmmTMB(testo_log ~ species * date_doy + (1 | year_) + (1 | ID),
             family = gaussian(link = "identity"), 
             data = df,
             control = glmmTMBControl(parallel = 15)
)


# plot(allEffects(m))
summary(m)

emmeans(m, pairwise ~ species)

# res <-simulateResiduals(m, plot = T)
# testDispersion(res) 

# extract season effect from model for plot
es = effect("species:date_doy", m, xlevels = 1000) |>
  data.frame() |>
  setDT()

# subset period with data
dr = df[, .(first_data = min(date_doy), last_data = max(date_doy)), by = species]
es = merge(es, dr, by = c('species'), all.x = TRUE)
es[, in_range := date_doy %between% c(first_data, last_data), by = 1:nrow(es)]
es = es[in_range == TRUE]


# plot for females
p2 = 
  ggplot() +
  ggtitle('Females') + 
  geom_boxjitter(data = df, aes(species, testo, fill = species), outlier.color = NA, jitter.shape = 21, jitter.color = NA, 
                 jitter.height = 0.0, jitter.width = 0.075, errorbar.draw = TRUE, jitter.size = 0.7, width = .6) +
  scale_fill_manual(values = c("steelblue4", "#E69F00", 'indianred3')) +
  geom_text(data = dfs, aes(species, Inf, label = sample_size), vjust = 1, size = 2.5) +
  scale_y_log10(limits = c(0.005, 350),
                breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks(sides = "l") +  
  theme_classic(base_size = 10) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 10, face = "bold")) +
  ylab('') +
  xlab('')


# effect of season

p4 =
ggplot() +
  geom_point(data = df, aes(date_doy, testo, color = species), size = 0.5) +
  geom_line(data = es, aes(y = exp(fit), x = date_doy, color = species), size = 0.8) +
  geom_ribbon(data = es, aes(y = exp(fit), x = date_doy, fill = species, ymin = exp(lower), ymax = exp(upper)), alpha = 0.2) +
  scale_color_manual(values = c("steelblue4", "#E69F00", 'indianred3')) +
  scale_fill_manual(values = c("steelblue4", "#E69F00", 'indianred3')) +
  scale_y_log10(limits = c(0.005, 350),
                breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks(sides = "l") +  
  scale_x_continuous(limits = c(140, 206), expand = expansion(add = c(0, 0))) +
  theme_classic(base_size = 10) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ylab('') +
  xlab('Day of the year')


# merge plots
p1 + p2 + p3 + p4 +
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = 'a')

ggsave('./OUTPUTS/FIGURES/testo_by_sex_and_species.tiff', plot = last_plot(),  width = 177, height = 177,
       units = c('mm'), dpi = 'print')


#--------------------------------------------------------------------------------------------------------------
# GnRH experiment
#--------------------------------------------------------------------------------------------------------------

# subset birds with GnRH 
IDe = d[!is.na(GnRH)]$ID
ds = d[ID %in% IDe]

# exclude third testo sample
ds = ds[!(ID == 270170318	& date_ == '2017-06-01')]

# sample type
ds[, GnRH_sample := ifelse(is.na(GnRH), 'Baseline', 'GnRH-induced')]
ds[, v20 := ifelse(min(volume) > 20, TRUE, FALSE), by = ID]

ID_l = ds[GnRH == 'low']$ID
ds[ID %in% ID_l, GnRH := 'low']

ID_h = ds[GnRH == 'high']$ID
ds[ID %in% ID_h, GnRH := 'high']

# check dates
ds[, .N, .(species, year_, date_doy)]
ds[, species_sample := paste0(species, '_', GnRH_sample)]

# min max scale
ds[, .(min(date_doy), max(date_doy))]
ds[, .(min(testo), max(testo))]

# subset males
dm = ds[sex == 'M']

# sample size
dms = dm[, .N, by = species]
dms[, sample_size := paste0('N = ', N/2)]
dms[, species_sample := paste0(species, '_Baseline')]


# model
m <- glmmTMB(testo_log ~ species + GnRH_sample +  GnRH,
             family = gaussian(link = "identity"), 
             data = dm,
             control = glmmTMBControl(parallel = 15)
)


# plot(allEffects(m))
summary(m)


p1 =
  ggplot(aes(species_sample, testo), data = dm) +
  ggtitle('Males') + 
  geom_text(data = dms, aes(species_sample, Inf, label = sample_size), vjust = 1, hjust = -0.5, size = 3) +
  geom_boxplot(aes(fill = species), outlier.colour = NA) +
  scale_fill_manual(values = c("steelblue4", 'indianred3')) +
  ggnewscale::new_scale_fill() +
  geom_line(aes(group = ID)) +
  geom_point(aes(fill = GnRH), shape = 21, size = 1) +
  scale_fill_manual(values=c("black", "white")) +
  scale_y_log10(limits = c(0.1, 50),
                breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks(sides = "l") + 
  scale_x_discrete(breaks = c('PESA_Baseline', 'PESA_GnRH-induced', 'REPH_Baseline', 'REPH_GnRH-induced'),
                   labels = c('Baseline', 'Induced', 'Baseline', 'Induced')) +
  theme_classic(base_size = 10) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 10, face = "bold")) +
  ylab('Testosteron (ng/ml)') +
  xlab('PESA                       REPH')


# subset females
df = ds[sex == 'F']

# sample size
dfs = df[, .N, by = species]
dfs[, sample_size := paste0('N = ', N/2)]
dfs[, species_sample := paste0(species, '_Baseline')]

# model
m <- glmmTMB(testo_log ~ species + GnRH_sample +  GnRH,
             family = gaussian(link = "identity"), 
             data = df,
             control = glmmTMBControl(parallel = 15)
)


# plot(allEffects(m))
summary(m)


p2 =
  ggplot(aes(species_sample, testo), data = df) +
  ggtitle('Females') + 
  geom_text(data = dfs, aes(species_sample, Inf, label = sample_size), vjust = 1, hjust = -0.5, size = 3) +
  geom_boxplot(aes(fill = species), outlier.colour = NA) +
  scale_fill_manual(values = c("steelblue4", 'indianred3')) +
  ggnewscale::new_scale_fill() +
  geom_line(aes(group = ID)) +
  geom_point(aes(fill = GnRH), shape = 21, size = 1) +
  scale_fill_manual(values=c("black", "white")) +
  scale_y_log10(limits = c(0.1, 50),
                breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks(sides = "l") +  
  scale_x_discrete(breaks = c('PESA_Baseline', 'PESA_GnRH-induced', 'REPH_Baseline', 'REPH_GnRH-induced'),
                   labels = c('Baseline', 'Induced', 'Baseline', 'Induced')) +
  theme_classic(base_size = 10) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 10, face = "bold")) +
  ylab('') + 
  xlab('PESA                       REPH')

# merge plots
p1 + p2 +
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = 'a')



ggsave('./OUTPUTS/FIGURES/testo_GnRH.tiff', plot = last_plot(),  width = 177, height = 88,
       units = c('mm'), dpi = 'print')


