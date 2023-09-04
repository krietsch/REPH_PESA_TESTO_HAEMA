#' ---
#' title: Figures and Statistic
#' subtitle: 
#' author: Johannes Krietsch
#' output:
#'    html_document:
#'      toc: true
#'      highlight: tango
#' ---

#==============================================================================================================
#' Data and code from ""
#' Contributor: Johannes Krietsch  
#' 📍 This script runs relative to the project's root directory and describes how I created all figures 
#' (stored in ./OUTPUTS/FIGURES) and performed all statistical analysis (summary tables stored in./OUTPUTS/ESM).
#==============================================================================================================

sapply( c('data.table', 'magrittr', 'ggplot2', 'knitr', 'glmmTMB', 'effects', 'broomExtra',
          'flextable', 'officer', 'DHARMa'),
        require, character.only = TRUE)

# load data
# source('./R/Testo_data_merge.R')
d = readRDS('./DATA/REPH_PESA_SESA_testosterone.RDS')

# Lines to run to create html output
opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())
# rmarkdown::render('./R/Testo_data_summary.R', output_dir = './OUTPUTS/R_COMPILED')

d[, testo_log := log(testo)]

# bleeding time
d[, caught_time := as.POSIXct(caught_time)]
d[, bled_time := as.POSIXct(bled_time)]
d[, diff_caught_bled := difftime(bled_time, caught_time, units = 'mins') %>% as.numeric]

# data as Julian
d[, date_doy := yday(caught_time)]

# year as character
d[, year_ := as.character(year_)]

d[, .N, by = year_]

#--------------------------------------------------------------------------------------------------------------
# Between species comparison
#--------------------------------------------------------------------------------------------------------------

# exclude GnRH induced samples
ds = d[is.na(GnRH)]

# model for males
dx = ds[sex_observed == 'M']

m <- glmmTMB(testo_log ~ species + volume + date_doy + year_,
             family = gaussian(link = "identity"), 
             data = dx,
             control = glmmTMBControl(parallel = 15)
)


plot(allEffects(m))
summary(m)

res <-simulateResiduals(m, plot = T)
testDispersion(res) 


m <- glmmTMB(testo_log ~ species + volume + date_doy + (1 | year_),
             family = gaussian(link = "identity"), 
             data = dx,
             control = glmmTMBControl(parallel = 15)
)


plot(allEffects(m))
summary(m)

res <-simulateResiduals(m, plot = T)
testDispersion(res) 

# model for females
dx = ds[sex_observed == 'F']

m <- glmmTMB(testo_log ~ species + volume + date_doy + year_,
             family = gaussian(link = "identity"), 
             data = dx,
             control = glmmTMBControl(parallel = 15)
)


plot(allEffects(m))
summary(m)

res <-simulateResiduals(m, plot = T)
testDispersion(res) 


m <- glmmTMB(testo_log ~ species + volume + date_doy + (1 | year_),
             family = gaussian(link = "identity"), 
             data = dx,
             control = glmmTMBControl(parallel = 15)
)


plot(allEffects(m))
summary(m)

res <-simulateResiduals(m, plot = T)
testDispersion(res) 


# To do:

# plot and statistic for comparison between species 

# plot and statistic for comparison with GnRH



