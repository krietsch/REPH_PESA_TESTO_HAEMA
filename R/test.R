#' ---
#' title: Figures and Statistic
#' subtitle: 
#' author: Johannes Krietsch
#' output:
#'    html_document:
#'      toc: true
#'      highlight: tango
#' ---

#-------------------------------------------------------------------------------
#' # Load packages and data
#-------------------------------------------------------------------------------

# packages
library(data.table)
library(magrittr)
library(dplyr)
library(foreach)
library(ggplot2)
library(ggparl)
library(patchwork)
library(knitr)
library(glmmTMB)
library(emmeans)
library(effects)
# library(broomExtra)
library(DHARMa)
library(performance)
library(flextable)
library(officer)

# load data
d <- fread("./DATA/REPH_PESA_testo_haema.csv", yaml = TRUE)

# Lines to run to create html output
opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())
# rmarkdown::render(
#   "./R/1_figures_and_statistics.R",
#   output_dir = "./OUTPUTS/R_COMPILED"
# )

# Function to deal with time
dt2hh <- function(x) {
  h <- as.POSIXlt(x)
  h$hour + h$min / 60 + h$sec / 3600
}

hh2rad <- function(x) {
  x * pi / 12
}

#-------------------------------------------------------------------------------
#' # Prepare data for analysis
#-------------------------------------------------------------------------------

# testo pg/ml to ng/ml
d[, testo := testo / 1000]
d[, testo_log := log10(testo)]

# bleeding time
d[, diff_caught_bled := difftime(bled_time, caught_time, units = 'mins') |>
    as.numeric()]

# data as Julian
d[, date_doy := yday(date_)]

# year as character
d[, year_ := as.character(year_)]
d[, .N, by = .(species, year_)]

# factor order
d[, species := factor(species, levels = c("PESA", "REPH"))]
d[, sex := factor(sex, levels = c("M", "F"))]

# min max scale
d[, .(min(date_doy), max(date_doy))]
d[, .(min(testo), max(testo))]

# start word file for ESM
ESM <- read_docx()

# parameter names
pn <- fread(
  "parname;                            parameter
  (Intercept);                         Intercept 
  speciesREPH;                         Species (red phalarope)
  date_doy;                            Day of the year 
  poly(date_doy, 2)1;                  Day of the year (linear)
  poly(date_doy, 2)2;                  Day of the year (quadratic)
  sexF;                                Sex (female)
  sexF:date_doy;                       Sex (female):Day of the year 
  testo_log;                           Testosterone (logarithmic)
  GnRH_sampleGnRH-induced;             GnRH induced
  GnRHlow;                             Low GnRH concentration
  speciesREPH:GnRH_sampleGnRH-induced; Species (red phalarope):GnRH induced
  smi_z;                               Scaled mass index
  speciesREPH:smi_z;                   Species (red phalarope):Scaled mass index
  sexF:smi_z;                          Sex (female):Scaled mass index
  sd__(Intercept);                     Random intercept
  sd__(Intercept)_year_;               Random intercept (year)
  sd__(Intercept)_ID;                  Random intercept (ID)
  r2marg;                              R² marginal
  r2cond;                              R² conditional
",
  sep = ";"
)

# plot settings
bs <- 12 # base size
ls <- 3 # labels

#-------------------------------------------------------------------------------
#' # Scaled mass index (Peig and Green, 2009)
#-------------------------------------------------------------------------------

# mean by ID
dID <- unique(d[, wing_mean_ID := mean(wing, na.rm = TRUE), by = ID], by = "ID")

# mean wing length by species and sex
dPop <- dID[, .(wing_mean_pop = mean(wing_mean_ID, na.rm = TRUE)),
            by = .(species, sex)
]

# slope for each category
foreach(i = 1:nrow(dPop)) %do% {
  ds <- d[species == dPop[i, ]$species & sex == dPop[i, ]$sex] # subset
  b_msa_ <- coef(smatr::sma(log(ds$weight) ~ log(ds$wing)))[2]
  dPop[species == dPop[i, ]$species & sex == dPop[i, ]$sex, b_msa := b_msa_]
}

# merge with all data
d <- merge(d, dPop, by = c("species", "sex"))

# scaled mass index for each observation
d[, smi := weight * (wing_mean_pop / wing_mean_ID)^b_msa]

# z transformed by species and sex
d[, smi_z := scale(smi), by = .(species, sex)]

ggplot(data = d) +
  geom_boxplot(aes(smi_z, interaction(species, sex))) +
  theme_classic(base_size = 12)

d[, .(min(smi_z, na.rm = TRUE), max(smi_z, na.rm = TRUE))]

#-------------------------------------------------------------------------------
#' # Testosterone between species comparison
#-------------------------------------------------------------------------------

# exclude GnRH induced samples
ds <- d[is.na(GnRH)]

# convert datetime to hour
ds[, HH := dt2hh(caught_time)]

# model
m1 <- glmmTMB(
  testo_log ~ species * sex + species * sex * poly(date_doy, 2) + species * sex * smi_z +
    (1 | year_) + (1 | ID),
  family = gaussian(link = "identity"),
  data = ds
)

# model summary
summary(m1)
plot(allEffects(m1))

# reduce model
m2 <- update(m1, . ~ . - species:sex:smi_z)
anova(m1, m2, test = "Chisq")

# model summary
summary(m2)
plot(allEffects(m2))

# reduce model
m3 <- update(m2, . ~ . - sex:smi_z)
anova(m2, m3, test = "Chisq")

# model summary
summary(m3)
plot(allEffects(m3))

# reduce model
m4 <- update(m3, . ~ . - species:smi_z)
anova(m3, m4, test = "Chisq")

# model summary
summary(m4)
plot(allEffects(m4))


# final model
m <- glmmTMB(
  testo_log ~ species * sex * poly(date_doy, 2) + smi_z +
    (1 | year_) + (1 | ID),
  family = gaussian(link = "identity"),
  data = ds
)

# model summary
summary(m)
plot(allEffects(m))


library(car)
Anova(m, type = 3)


emtrends(m, ~ sex, var = "date_doy", at = list(date_doy = median(ds$date_doy)))

emtrends(m, ~ sex | species, var = "date_doy",
         at = list(date_doy = median(ds$date_doy)))


# slopes of date_doy for each sex within species
tr <- emtrends(m, ~ sex | species, var = "date_doy")

# show tests of whether slope ≠ 0
summary(tr, infer = c(TRUE, TRUE)) 



# get slopes for date_doy by sex within species
tr <- emtrends(m, ~ sex | species, var = "date_doy")

# test difference in slopes between species for males
contrast(tr, method = "pairwise", by = "sex")



emm <- emmeans(m, ~ species * sex, type = "response")

# all pairwise comparisons (Tukey-adjusted)
pairs(emm, adjust = "tukey")


emm <- emmeans(m, ~ species | sex)  
pairs(emm, by = "sex")


emm <- emmeans(m, ~ sex | species)  
pairs(emm, by = "species")

dm <- ds[sex == "M"]

e <- effect("species:sex", m,
                      xlevels = list(species = "PESA", sex = "M")) |>
  data.frame() |>
  setDT()

e <- e[sex == "M"]

#-------------------------------------------------------------------------------
#' # Effect of time
#-------------------------------------------------------------------------------

# convert datetime to hour
ds[, HH := dt2hh(caught_time)]

# model
m <- glmmTMB(
  testo_log ~ species + sex * poly(date_doy, 2) + smi_z + sin(hh2rad(HH)) + cos(hh2rad(HH)) +
    (1 | year_) + (1 | ID),
  family = gaussian(link = "identity"),
  data = ds
)

plot(allEffects(m))
summary(m)



ds[, hour := round(HH, 0)]

# Now count observations by HH, species, sex, and hour
d_counts <- ds[!is.na(HH), .N, by = .(species, sex, hour)]

ggplot(d_counts, aes(x = hour, color = sex, fill = sex)) +
  geom_density(alpha = 0.3) +
  facet_wrap(~ species) +
  theme_minimal() +
  labs(title = "Density of Hourly Counts by Sex and Species",
       x = "Hourly Count",
       y = "Density")





# Summarize counts across households
hourly_counts <- d_counts[, .(totalN = sum(N)), by = .(species, sex, hour)]


ggplot(hourly_counts, aes(x = hour, y = totalN, color = sex, group = sex)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ species) +
  theme_minimal() +
  labs(title = "Hourly Sampling Distribution by Species and Sex",
       x = "Hour of Day",
       y = "Number of Observations")

hourly_props <- d_counts[, .(totalN = sum(N)), by = .(species, sex, hour)]
hourly_props[, prop := totalN / sum(totalN), by = .(species, sex)]

ggplot(hourly_props, aes(x = hour, y = prop, color = sex, group = sex)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ species) +
  theme_minimal() +
  labs(title = "Proportion of Observations by Hour of Day",
       x = "Hour of Day",
       y = "Proportion of Observations")


# Sum counts across species for each sex and hour
sex_hourly_counts <- d_counts[, .(totalN = sum(N)), by = .(sex, hour)]

# Convert to wide format: rows = hour, columns = sex
sex_wide <- dcast(sex_hourly_counts, hour ~ sex, value.var = "totalN", fill = 0)

# Remove hour column
chisq_matrix <- as.matrix(sex_wide[, -1, with = FALSE])

# Perform chi-square test
chisq.test(chisq_matrix)





# Sum counts across species for each sex and hour
sex_hourly_counts <- d_counts[, .(totalN = sum(N)), by = .(species, hour)]

# Convert to wide format: rows = hour, columns = sex
sex_wide <- dcast(sex_hourly_counts, hour ~ species, value.var = "totalN", fill = 0)

# Remove hour column
chisq_matrix <- as.matrix(sex_wide[, -1, with = FALSE])

# Perform chi-square test
chisq.test(chisq_matrix)





#-------------------------------------------------------------------------------
#' # Haematocrit between species comparison
#-------------------------------------------------------------------------------

# exclude GnRH induced samples
ds <- d[is.na(GnRH)]

# exclude NA
ds <- ds[!is.na(haema)]


m1 <- glmmTMB(
  haema ~ sex * species * poly(date_doy, 2) + sex * species * testo_log + sex * species * smi_z +
    (1 | year_) + (1 | ID),
  family = gaussian(link = "identity"),
  data = ds
)

# model summary
summary(m1)
plot(allEffects(m1))

# reduce model
m2 <- update(m1, . ~ . - sex:species:testo_log)
anova(m1, m2, test = "Chisq")

# model summary
summary(m2)
plot(allEffects(m2))

# reduce model
m3 <- update(m2, . ~ . - sex:species:smi_z)
anova(m2, m3, test = "Chisq")

# model summary
summary(m3)
plot(allEffects(m3))

# reduce model
m4 <- update(m3, . ~ . - species:smi_z)
anova(m3, m4, test = "Chisq")

# model summary
summary(m4)
plot(allEffects(m4))

# reduce model
m5 <- update(m4, . ~ . - sex:smi_z)
anova(m4, m5, test = "Chisq")

# model summary
summary(m5)
plot(allEffects(m5))

# reduce model
m6 <- update(m5, . ~ . - species:testo_log)
anova(m5, m6, test = "Chisq")

# model summary
summary(m6)
plot(allEffects(m6))

# reduce model
m7 <- update(m6, . ~ . - sex:testo_log)
anova(m6, m7, test = "Chisq")

# model summary
summary(m7)
plot(allEffects(m7))

# reduce model
m8 <- glmmTMB(
  haema ~ sex * species * date_doy + testo_log + smi_z +
    (1 | year_) + (1 | ID),
  family = gaussian(link = "identity"),
  data = ds
)

anova(m7, m8, test = "Chisq")

# model summary
summary(m8)
plot(allEffects(m8))





# final reduced model
m <- glmmTMB(
  haema ~ sex * species * poly(date_doy, 2) + testo_log + smi_z +
    (1 | year_) + (1 | ID),
  family = gaussian(link = "identity"),
  data = ds
)

# model summary
summary(m)
plot(allEffects(m))



