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
          'flextable', 'officer', 'DHARMa', 'ggparl', 'patchwork', 'performance', 'dplyr'),
        require, character.only = TRUE)

# load data
d = readRDS('./DATA/REPH_PESA_testosterone.RDS')

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
d[, species := factor(species, levels = c('PESA', 'REPH'))]

# min max scale
d[, .(min(date_doy), max(date_doy))]
d[, .(min(testo), max(testo))]

# start word file for ESM
ESM = read_docx()

# parameter names 
pn = fread("parname;                                                          parameter
            (Intercept);                                                      Intercept 
            speciesREPH;                                                      Species (red phalarope)
            date_doy;                                                         Day of the year 
            poly(date_doy, 2)1;                                               Day of the year (linear)
            poly(date_doy, 2)2;                                               Day of the year (quadratic)
            sexF;                                                             Sex (female)
            sexF:date_doy;                                                    Sex (female):Day of the year 
            testo_log;                                                        Testosterone (logarithmic)
            GnRH_sampleGnRH-induced;                                          GnRH induced
            GnRHlow;                                                          Low GnRH concentration
            speciesREPH:GnRH_sampleGnRH-induced;                              Species (red phalarope):GnRH induced
            sd__(Intercept);                                                  Random intercept
            sd__(Intercept)_year_;                                            Random intercept (year)
            sd__(Intercept)_ID;                                               Random intercept (ID)
            r2marg;                                                           R² marginal
            r2cond;                                                           R² conditional
            
", sep = ';')


#--------------------------------------------------------------------------------------------------------------
# Between species comparison
#--------------------------------------------------------------------------------------------------------------

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
m <- glmmTMB(testo_log ~ species + poly(date_doy,2) + (1 | year_) + (1 | ID),
             family = gaussian(link = "identity"), 
             data = dm,
             control = glmmTMBControl(parallel = 15)
)

plot(allEffects(m))
summary(m)

# res <-simulateResiduals(m, plot = T)
# testDispersion(res)

e = effect("species", m, xlevels = 2) |>
  data.frame() |>
  setDT()


# create clean summary table
y = tidy(m) |> data.table()
x = r2(m) |> data.table() 

setnames(x, c('estimate'))
y[term == 'sd__(Intercept)', term := paste0(term, '_', group)]
x[, estimate := as.numeric(estimate)]
x[, term :=  c('r2cond', 'r2marg')]
y = rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
y[, row_order := rownames(y) |> as.numeric()]
y = merge(y, pn, by.x = 'term', by.y = 'parname')
setorder(y, row_order)
y = y[, .(Parameter = parameter, Estimate = estimate, SE = std.error, Statistic = statistic, p = p.value)] 
y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns 

# save table in word
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S1. LMM males testo')) |>  body_add_par('') |> 
  body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')


# model with interaction for plot
m <- glmmTMB(testo_log ~ species * poly(date_doy,2) + (1 | year_) + (1 | ID),
             family = gaussian(link = "identity"), 
             data = dm,
             control = glmmTMBControl(parallel = 15)
)

# plot(allEffects(m))
# summary(m)

# extract season effect from model for plot
es = effect("species:poly(date_doy, 2)", m, xlevels = 1000) |>
  data.frame() |>
  setDT()

# subset period with data
dr = dm[, .(first_data = min(date_doy), last_data = max(date_doy)), by = species]
es = merge(es, dr, by = c('species'), all.x = TRUE)
es[, in_range := date_doy %between% c(first_data, last_data), by = 1:nrow(es)]
es = es[in_range == TRUE]



# plot for males
p1 =
ggplot() +
  ggtitle('Males') + 
  geom_violin(data = dm, aes(species, testo, fill = species), alpha = 0.7) +
  geom_point(data = e, aes(species, 10^fit, color = species),
             position = position_dodge(0.5), size = 2) +
  geom_linerange(data = e, aes(x = species, ymin = 10^upper, ymax = 10^lower, color = species), size = 0.5,
                 position = position_dodge(width = 0.5)) +
  scale_fill_manual(values = c("steelblue4", 'indianred3')) +
  scale_color_manual(values = c("black", 'black')) +
  geom_text(data = dms, aes(species, Inf, label = sample_size), vjust = 1, size = 2.5) +
  scale_y_log10(limits = c(0.01, 350),
                breaks = c(0.01, 0.1, 1, 10, 100),
                labels = c(0.01, 0.1, 1, 10, 100)) +
  annotation_logticks(sides = "l") +  
  scale_x_discrete(labels = c("PESA" = "Pectoral sandpiper", "REPH" = "Red phalarope")) +
  theme_classic(base_size = 10) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 10, face = "bold")) +
  ylab('Testosteron (ng/ml)') +
  xlab('Species')

# effect of season
p3 =
  ggplot() +
  geom_point(data = dm, aes(date_doy, testo, color = species), size = 0.5, alpha = 0.5) +
  geom_line(data = es, aes(y = 10^fit, x = date_doy, color = species), size = 0.8) +
  geom_ribbon(data = es, aes(y = 10^fit, x = date_doy, fill = species, 
                             ymin = 10^lower, ymax = 10^upper), alpha = 0.2) +
  scale_color_manual(values = c("steelblue4", 'indianred3')) +
  scale_fill_manual(values = c("steelblue4", 'indianred3')) +
  scale_y_log10(limits = c(0.001, 350),
                breaks = c(0.001, 0.01, 0.1, 1, 10, 100),
                labels = c(0.001, 0.01, 0.1, 1, 10, 100)) +
  annotation_logticks(sides = "l") +  
  scale_x_continuous(limits = c(140, 206), expand = expansion(add = c(0, 0))) +
  theme_classic(base_size = 10) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ylab('Testosteron (ng/ml)') +
  xlab('Day of the year')



###### subset females
df = ds[sex == 'F']

# sample size
dfs = df[, .N, by = species]
du = unique(df, by = 'ID')
du = du[, .(N_ind = .N), by = species]
dfs = merge(dfs, du, by = 'species')
dfs[, sample_size := paste0('N = ', N, ' | ', N_ind)]

# model 
m <- glmmTMB(testo_log ~ species + date_doy + (1 | year_) + (1 | ID),
             family = gaussian(link = "identity"), 
             data = df,
             control = glmmTMBControl(parallel = 15)
)

plot(allEffects(m))
summary(m)

# res <-simulateResiduals(m, plot = T)
# testDispersion(res)

e = effect("species", m, xlevels = 2) |>
  data.frame() |>
  setDT()


# create clean summary table
y = tidy(m) |> data.table()
x = r2(m) |> data.table() 

setnames(x, c('estimate'))
y[term == 'sd__(Intercept)', term := paste0(term, '_', group)]
x[, estimate := as.numeric(estimate)]
x[, term :=  c('r2cond', 'r2marg')]
y = rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
y[, row_order := rownames(y) |> as.numeric()]
y = merge(y, pn, by.x = 'term', by.y = 'parname')
setorder(y, row_order)
y = y[, .(Parameter = parameter, Estimate = estimate, SE = std.error, Statistic = statistic, p = p.value)] 
y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns 

# save table in word
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S2. LMM females testo')) |>  body_add_par('') |> 
  body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')


# model with interaction for plot
m <- glmmTMB(testo_log ~ species * date_doy + (1 | year_) + (1 | ID),
             family = gaussian(link = "identity"), 
             data = df,
             control = glmmTMBControl(parallel = 15)
)

# plot(allEffects(m))
# summary(m)

# extract season effect from model for plot
es = effect("species:poly(date_doy, 2)", m, xlevels = 1000) |>
  data.frame() |>
  setDT()

# subset period with data
dr = dm[, .(first_data = min(date_doy), last_data = max(date_doy)), by = species]
es = merge(es, dr, by = c('species'), all.x = TRUE)
es[, in_range := date_doy %between% c(first_data, last_data), by = 1:nrow(es)]
es = es[in_range == TRUE]


# plot for females
p2 = 
  ggplot() +
  ggtitle('Females') + 
  geom_violin(data = df, aes(species, testo, fill = species), alpha = 0.7) +
  geom_point(data = e, aes(species, 10^fit, color = species),
             position = position_dodge(0.5), size = 2) +
  geom_linerange(data = e, aes(x = species, ymin = 10^upper, ymax = 10^lower, color = species), size = 0.5,
                 position = position_dodge(width = 0.5)) +
  scale_fill_manual(values = c("steelblue4", 'indianred3')) +
  scale_color_manual(values = c("black", 'black')) +
  geom_text(data = dfs, aes(species, Inf, label = sample_size), vjust = 1, size = 2.5) +
  scale_y_log10(limits = c(0.01, 350),
                breaks = c(0.01, 0.1, 1, 10, 100),
                labels = c(0.01, 0.1, 1, 10, 100)) +
  annotation_logticks(sides = "l") +  
  scale_x_discrete(labels = c("PESA" = "Pectoral sandpiper", "REPH" = "Red phalarope")) +
  theme_classic(base_size = 10) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 10, face = "bold")) +
  ylab('') +
  xlab('Species')


# effect of season

p4 =
ggplot() +
  geom_point(data = df, aes(date_doy, testo, color = species), size = 0.5, alpha = 0.5) +
  geom_line(data = es, aes(y = 10^fit, x = date_doy, color = species), size = 0.8) +
  geom_ribbon(data = es, aes(y = 10^fit, x = date_doy, 
                             fill = species, ymin = 10^lower, ymax = 10^upper), alpha = 0.2) +
  scale_color_manual(values = c("steelblue4", 'indianred3')) +
  scale_fill_manual(values = c("steelblue4", 'indianred3')) +
  scale_y_log10(limits = c(0.001, 350),
                breaks = c(0.001, 0.01, 0.1, 1, 10, 100),
                labels = c(0.001, 0.01, 0.1, 1, 10, 100)) +
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
m <- glmmTMB(testo_log ~ species * GnRH_sample +  GnRH + (1 | ID),
             family = gaussian(link = "identity"), 
             data = dm,
             control = glmmTMBControl(parallel = 15)
)


# plot(allEffects(m))
summary(m)

e = effect("species:GnRH_sample", m, xlevels = 2) |>
  data.frame() |>
  setDT()

e[, species_sample := paste0(species, '_', GnRH_sample)]

# create clean summary table
y = tidy(m) |> data.table()
x = r2(m) |> data.table() 

setnames(x, c('estimate'))
y[term == 'sd__(Intercept)', term := paste0(term, '_', group)]
x[, estimate := as.numeric(estimate)]
x[, term :=  c('r2cond', 'r2marg')]
y = rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
y[, row_order := rownames(y) |> as.numeric()]
y = merge(y, pn, by.x = 'term', by.y = 'parname')
setorder(y, row_order)
y = y[, .(Parameter = parameter, Estimate = estimate, SE = std.error, Statistic = statistic, p = p.value)] 
y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns 

# save table in word
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S3. LMM males GnRH')) |>  body_add_par('') |> 
  body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')


p1 =
  ggplot() +
  ggtitle('Males') + 
  geom_text(data = dms, aes(species_sample, Inf, label = sample_size), vjust = 1, hjust = -0.5, size = 3) +
  geom_point(data = e[GnRH_sample == 'Baseline'], aes(species_sample, 10^fit, color = species),
               position = position_nudge(x = -0.2), size = 2) +
  geom_point(data = e[GnRH_sample == 'GnRH-induced'], aes(species_sample, 10^fit, color = species),
             position = position_nudge(x = 0.2), size = 2) +
  geom_linerange(data = e[GnRH_sample == 'Baseline'], 
                 aes(x = species_sample, ymin = 10^upper, ymax = 10^lower, color = species), size = 0.5,
                 position = position_nudge(x = -0.2)) +
  geom_linerange(data =  e[GnRH_sample == 'GnRH-induced'], 
                 aes(x = species_sample, ymin = 10^upper, ymax = 10^lower, color = species), size = 0.5,
                 position = position_nudge(x = 0.2)) +
  scale_color_manual(values = c("steelblue4", 'indianred3')) +
  ggnewscale::new_scale_fill() +
  geom_line(data = dm, aes(species_sample, testo, group = ID), size = 0.3) +
  geom_point(data = dm, aes(species_sample, testo, fill = GnRH), shape = 21, size = 1) +
  scale_fill_manual(values=c("black", "white")) +
  scale_y_log10(limits = c(0.1, 50),
                breaks = c(0.01, 0.1, 1, 10),
                labels = c(0.01, 0.1, 1, 10)) +
  annotation_logticks(sides = "l") + 
  scale_x_discrete(breaks = c('PESA_Baseline', 'PESA_GnRH-induced', 
                              'REPH_Baseline', 'REPH_GnRH-induced'),
                   labels = c('Baseline', 'GnRH', 'Baseline', 'GnRH')) +
  theme_classic(base_size = 10) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 10, face = "bold")) +
  ylab('Testosteron (ng/ml)') +
  xlab('Pectoral Sandpiper       Red Phalarope    ')


# subset females
df = ds[sex == 'F']

# sample size
dfs = df[, .N, by = species]
dfs[, sample_size := paste0('N = ', N/2)]
dfs[, species_sample := paste0(species, '_Baseline')]

# model
m <- glmmTMB(testo_log ~ species * GnRH_sample +  GnRH + (1 | ID),
             family = gaussian(link = "identity"), 
             data = df,
             control = glmmTMBControl(parallel = 15)
)


# plot(allEffects(m))
summary(m)

e = effect("species:GnRH_sample", m, xlevels = 2) |>
  data.frame() |>
  setDT()

e[, species_sample := paste0(species, '_', GnRH_sample)]


# create clean summary table
y = tidy(m) |> data.table()
x = r2(m) |> data.table() 

setnames(x, c('estimate'))
y[term == 'sd__(Intercept)', term := paste0(term, '_', group)]
x[, estimate := as.numeric(estimate)]
x[, term :=  c('r2cond', 'r2marg')]
y = rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
y[, row_order := rownames(y) |> as.numeric()]
y = merge(y, pn, by.x = 'term', by.y = 'parname')
setorder(y, row_order)
y = y[, .(Parameter = parameter, Estimate = estimate, SE = std.error, Statistic = statistic, p = p.value)] 
y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns 

# save table in word
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S3. LMM females GnRH')) |>  body_add_par('') |> 
  body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')

p2 =
  ggplot() +
  ggtitle('Females') + 
  geom_text(data = dfs, aes(species_sample, Inf, label = sample_size), vjust = 1, hjust = -0.5, size = 3) +
  geom_point(data = e[GnRH_sample == 'Baseline'], aes(species_sample, 10^fit, color = species),
             position = position_nudge(x = -0.2), size = 2) +
  geom_point(data = e[GnRH_sample == 'GnRH-induced'], aes(species_sample, 10^fit, color = species),
             position = position_nudge(x = 0.2), size = 2) +
  geom_linerange(data = e[GnRH_sample == 'Baseline'], 
                 aes(x = species_sample, ymin = 10^upper, ymax = 10^lower, color = species), size = 0.5,
                 position = position_nudge(x = -0.2)) +
  geom_linerange(data =  e[GnRH_sample == 'GnRH-induced'], 
                 aes(x = species_sample, ymin = 10^upper, ymax = 10^lower, color = species), size = 0.5,
                 position = position_nudge(x = 0.2)) +
  scale_color_manual(values = c("steelblue4", 'indianred3')) +
  ggnewscale::new_scale_fill() +
  geom_line(data = df, aes(species_sample, testo, group = ID), size = 0.3) +
  geom_point(data = df, aes(species_sample, testo, fill = GnRH), shape = 21, size = 1) +
  scale_fill_manual(values=c("black", "white")) +
  scale_y_log10(limits = c(0.1, 50),
                breaks = c(0.01, 0.1, 1, 10),
                labels = c(0.01, 0.1, 1, 10)) +
  annotation_logticks(sides = "l") +  
  scale_x_discrete(breaks = c('PESA_Baseline', 'PESA_GnRH-induced', 
                              'REPH_Baseline', 'REPH_GnRH-induced'),
                   labels = c('Baseline', 'GnRH', 'Baseline', 'GnRH')) +
  theme_classic(base_size = 10) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 10, face = "bold")) +
  ylab('') + 
  xlab('Pectoral Sandpiper       Red Phalarope    ')

# merge plots
p1 + p2 +
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = 'a')


ggsave('./OUTPUTS/FIGURES/testo_GnRH.tiff', plot = last_plot(),  width = 177, height = 88,
       units = c('mm'), dpi = 'print')

#--------------------------------------------------------------------------------------------------------------
# Testosterone influence on haematocrit
#--------------------------------------------------------------------------------------------------------------

# as numeric
d[, haema := as.numeric(haema)]

# factor order
d[, sex := factor(sex, levels = c('M', 'F'))]

# exclude GnRH induced samples
ds = d[is.na(GnRH)]

# exclude NA
ds = ds[!is.na(haema)]
ds[haema == 85, haema := 58]


ggplot() +
  ggtitle('Males') + 
  geom_boxjitter(data = ds, aes(species, haema, fill = sex, group = interaction(sex, species)), 
                 outlier.color = NA, jitter.shape = 21, jitter.color = NA, 
                 jitter.height = 0.0, jitter.width = 0.1, errorbar.draw = TRUE, jitter.size = 0.7, width = .6) +
  scale_fill_manual(values = c("steelblue4", 'indianred3')) +
  theme_classic(base_size = 10) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 10, face = "bold")) +
  ylab('Haematocrit') +
  xlab('')



### by species

# PESA
dss = ds[species == 'PESA']

# sample size
dsss = dss[, .N, by = sex]
du = unique(dss, by = 'ID')
du = du[, .(N_ind = .N), by = sex]
dsss = merge(dsss, du, by = 'sex')
dsss[, sample_size := paste0('N = ', N, ' | ', N_ind)]

# model
m <- glmmTMB(haema ~ sex * date_doy + testo_log  + (1 | year_) + (1 | ID),
             family = gaussian(link = "identity"), 
             data = dss,
             control = glmmTMBControl(parallel = 15)
)

plot(allEffects(m))
summary(m)

# extract effect of haema
e = effect("sex", m, xlevels = 2) |>
  data.frame() |>
  setDT()

# factor order
e[, sex := factor(sex, levels = c('M', 'F'))]

# extract season effect from model for plot
es = effect("sex:date_doy", m, xlevels = 1000) |>
  data.frame() |>
  setDT()

# subset period with data
dr = ds[, .(first_data = min(date_doy), last_data = max(date_doy)), by = sex]
es = merge(es, dr, by = c('sex'), all.x = TRUE)
es[, in_range := date_doy %between% c(first_data, last_data), by = 1:nrow(es)]
es = es[in_range == TRUE]

# factor order
es[, sex := factor(sex, levels = c('M', 'F'))]

# extract effect of testo
et = effect("sex:testo_log", m, xlevels = 1000) |>
  data.frame() |>
  setDT()

# subset period with data
dr = ds[, .(first_data = min(testo_log), last_data = max(testo_log)), by = sex]
et = merge(et, dr, by = c('sex'), all.x = TRUE)
et[, in_range := testo_log %between% c(first_data, last_data), by = 1:nrow(et)]
et = et[in_range == TRUE]

# factor order
et[, sex := factor(sex, levels = c('M', 'F'))]

# create clean summary table
y = tidy(m) |> data.table()
x = r2(m) |> data.table() 

setnames(x, c('estimate'))
y[term == 'sd__(Intercept)', term := paste0(term, '_', group)]
x[, estimate := as.numeric(estimate)]
x[, term :=  c('r2cond', 'r2marg')]
y = rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
y[, row_order := rownames(y) |> as.numeric()]
y = merge(y, pn, by.x = 'term', by.y = 'parname')
setorder(y, row_order)
y = y[, .(Parameter = parameter, Estimate = estimate, SE = std.error, Statistic = statistic, p = p.value)] 
y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns 

# save table in word
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S4. LMM haematocrit males')) |>  body_add_par('') |> 
  body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')


# sex comparision
p1 =
  ggplot() +
  ggtitle('Pectoral sandpiper') + 
  geom_text(data = dsss, aes(sex, Inf, label = sample_size), vjust = 1, size = 2.5) +
  geom_violin(data = dss, aes(sex, haema, fill = sex), alpha = 0.7) +
  geom_point(data = e, aes(sex, fit), color = 'black',
             position = position_dodge(0.5), size = 2) +
  geom_linerange(data = e, aes(x = sex, ymin = upper, ymax = lower), color = 'black', size = 0.5,
                 position = position_dodge(width = 0.5)) +
  scale_fill_manual(values = c('#7aa048', '#E69F00')) +
  scale_y_continuous(limits = c(34, 73), expand = expansion(add = c(0, 0))) +
  scale_x_discrete(labels = c("M" = "Male", "F" = "Female")) +
  theme_classic(base_size = 10) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 10, face = "bold")) +
  ylab('Haematocrit (%)') +
  xlab('Sex')


# effect of season
p2 =
  ggplot() +
  geom_point(data = dss, aes(date_doy, haema, color = sex), size = 0.5, alpha = 0.5) +
  geom_line(data = es, aes(y = fit, x = date_doy, color = sex), size = 0.8) +
  geom_ribbon(data = es, aes(y = fit, x = date_doy, fill = sex, ymin = lower, ymax = upper), alpha = 0.2) +
  scale_color_manual(values = c('#7aa048', '#E69F00')) +
  scale_fill_manual(values = c('#7aa048', '#E69F00')) +
  scale_y_continuous(limits = c(34, 73), expand = expansion(add = c(0, 0))) +
  scale_x_continuous(limits = c(140, 206), expand = expansion(add = c(0, 0))) +
  theme_classic(base_size = 10) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ylab('Haematocrit (%)') +
  xlab('Day of the year')


# effect of testosterone
p3 =
  ggplot() +
  geom_point(data = dss, aes(10^testo_log, haema, color = sex), size = 0.5, alpha = 0.5) +
  geom_line(data = et, aes(y = fit, x = 10^testo_log, color = sex), size = 0.8) +
  geom_ribbon(data = et, aes(y = fit, x = 10^testo_log, fill = sex, ymin = lower, ymax = upper), alpha = 0.2) +
  scale_color_manual(values = c('#7aa048', '#E69F00')) +
  scale_fill_manual(values = c('#7aa048', '#E69F00')) +
  scale_y_continuous(limits = c(34, 73), expand = expansion(add = c(0, 0))) +
  scale_x_log10(limits = c(0.01, 350),
                breaks = c(0.01, 0.1, 1, 10, 100),
                labels = c(0.01, 0.1, 1, 10, 100)) +
  annotation_logticks(sides = "b") +
  theme_classic(base_size = 10) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ylab('Haematocrit (%)') +
  xlab('Testosteron (ng/ml)')

# REPH
dss = ds[species == 'REPH']

# sample size
dsss = dss[, .N, by = sex]
du = unique(dss, by = 'ID')
du = du[, .(N_ind = .N), by = sex]
dsss = merge(dsss, du, by = 'sex')
dsss[, sample_size := paste0('N = ', N, ' | ', N_ind)]

# model
m <- glmmTMB(haema ~ sex + date_doy + testo_log  + (1 | year_) + (1 | ID),
             family = gaussian(link = "identity"), 
             data = dss,
             control = glmmTMBControl(parallel = 15)
)

plot(allEffects(m))
summary(m)

# extract effect of haema
e = effect("sex", m, xlevels = 2) |>
  data.frame() |>
  setDT()

# factor order
e[, sex := factor(sex, levels = c('M', 'F'))]

# extract season effect from model for plot
es = effect("sex:date_doy", m, xlevels = 1000) |>
  data.frame() |>
  setDT()

# subset period with data
dr = ds[, .(first_data = min(date_doy), last_data = max(date_doy)), by = sex]
es = merge(es, dr, by = c('sex'), all.x = TRUE)
es[, in_range := date_doy %between% c(first_data, last_data), by = 1:nrow(es)]
es = es[in_range == TRUE]

# factor order
es[, sex := factor(sex, levels = c('M', 'F'))]

# extract effect of testo
et = effect("sex:testo_log", m, xlevels = 1000) |>
  data.frame() |>
  setDT()

# subset period with data
dr = ds[, .(first_data = min(testo_log), last_data = max(testo_log)), by = sex]
et = merge(et, dr, by = c('sex'), all.x = TRUE)
et[, in_range := testo_log %between% c(first_data, last_data), by = 1:nrow(et)]
et = et[in_range == TRUE]

# factor order
et[, sex := factor(sex, levels = c('M', 'F'))]


# create clean summary table
y = tidy(m) |> data.table()
x = r2(m) |> data.table() 

setnames(x, c('estimate'))
y[term == 'sd__(Intercept)', term := paste0(term, '_', group)]
x[, estimate := as.numeric(estimate)]
x[, term :=  c('r2cond', 'r2marg')]
y = rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
y[, row_order := rownames(y) |> as.numeric()]
y = merge(y, pn, by.x = 'term', by.y = 'parname')
setorder(y, row_order)
y = y[, .(Parameter = parameter, Estimate = estimate, SE = std.error, Statistic = statistic, p = p.value)] 
y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns 

# save table in word
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S5. LMM haematocrit males')) |>  body_add_par('') |> 
  body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')

# sex comparision
p4 = 
  ggplot() +
  ggtitle('Red Phalarope') + 
  geom_text(data = dsss, aes(sex, Inf, label = sample_size), vjust = 1, size = 2.5) +
  geom_violin(data = dss, aes(sex, haema, fill = sex), alpha = 0.7) +
  geom_point(data = e, aes(sex, fit), color = 'black',
             position = position_dodge(0.5), size = 2) +
  geom_linerange(data = e, aes(x = sex, ymin = upper, ymax = lower), color = 'black', size = 0.5,
                 position = position_dodge(width = 0.5)) +
  scale_fill_manual(values = c('#7aa048', '#E69F00')) +
  scale_y_continuous(limits = c(34, 73), expand = expansion(add = c(0, 0))) +
  scale_x_discrete(labels = c("M" = "Male", "F" = "Female")) +
  theme_classic(base_size = 10) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 10, face = "bold")) +
  ylab('') +
  xlab('Sex')


# effect of season
p5 =
  ggplot() +
  geom_point(data = dss, aes(date_doy, haema, color = sex), size = 0.5, alpha = 0.5) +
  geom_line(data = es, aes(y = fit, x = date_doy, color = sex), size = 0.8) +
  geom_ribbon(data = es, aes(y = fit, x = date_doy, fill = sex, ymin = lower, ymax = upper), alpha = 0.2) +
  scale_color_manual(values = c('#7aa048', '#E69F00')) +
  scale_fill_manual(values = c('#7aa048', '#E69F00')) +
  scale_y_continuous(limits = c(34, 73), expand = expansion(add = c(0, 0))) +
  scale_x_continuous(limits = c(140, 206), expand = expansion(add = c(0, 0))) +
  theme_classic(base_size = 10) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ylab('') +
  xlab('Day of the year')

# effect of testosterone
p6 =
  ggplot() +
  geom_point(data = dss, aes(10^testo_log, haema, color = sex), size = 0.5, alpha = 0.5) +
  geom_line(data = et, aes(y = fit, x = 10^testo_log, color = sex), size = 0.8) +
  geom_ribbon(data = et, aes(y = fit, x = 10^testo_log, fill = sex, ymin = lower, ymax = upper), alpha = 0.2) +
  scale_color_manual(values = c('#7aa048', '#E69F00')) +
  scale_fill_manual(values = c('#7aa048', '#E69F00')) +
  scale_y_continuous(limits = c(34, 73), expand = expansion(add = c(0, 0))) +
  scale_x_log10(limits = c(0.01, 350),
                breaks = c(0.01, 0.1, 1, 10, 100),
                labels = c(0.01, 0.1, 1, 10, 100)) +
  annotation_logticks(sides = "b") +
  theme_classic(base_size = 10) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ylab('') +
  xlab('Testosteron (ng/ml)')



# merge plots
p1 + p4 +
p2 + p5 +
p3 + p6 +
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = 'a')


ggsave('./OUTPUTS/FIGURES/haematocrit_species_split.tiff', plot = last_plot(),  width = 177, height = 264,
       units = c('mm'), dpi = 'print')




# save word file
print(ESM, target = "./OUTPUTS/ESM/ESM_testo_analysis.docx")

