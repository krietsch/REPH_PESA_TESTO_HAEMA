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


# model
m <- glmmTMB(
  testo_log ~ species * sex * poly(date_doy, 2) + smi_z +
    (1 | year_) + (1 | ID),
  family = gaussian(link = "identity"),
  data = ds
)

# model summary
summary(m)
plot(allEffects(m))


emm <- emmeans(m, ~ species | sex)  
pairs(emm, by = "sex")

dm <- ds[sex == "M"]

e <- effect("species:sex", m,
                      xlevels = list(species = "PESA", sex = "M")) |>
  data.frame() |>
  setDT()

e <- e[sex == "M"]


# subset period with data
dr <- dm[, .(first_data = min(date_doy), last_data = max(date_doy)),
         by = species
]

# plot for males

  ggplot() +
  ggtitle("Males") +
  geom_violin(data = dm, aes(species, testo, fill = species), alpha = 0.7) +
  geom_point(
    data = e, aes(species, 10^fit, color = species),
    position = position_dodge(0.5), size = 2
  ) +
  geom_linerange(
    data = e, aes(
      x = species, ymin = 10^upper, ymax = 10^lower,
      color = species
    ), linewidth = 0.5,
    position = position_dodge(width = 0.5)
  ) +
  scale_fill_manual(values = c("steelblue4", "indianred3")) +
  scale_color_manual(values = c("black", "black")) +

  scale_y_log10(
    limits = c(0.01, 350),
    breaks = c(0.01, 0.1, 1, 10, 100),
    labels = c(0.01, 0.1, 1, 10, 100)
  ) +
  annotation_logticks(sides = "l") +
  scale_x_discrete(
    labels = c("PESA" = "Pectoral sandpiper", "REPH" = "Red phalarope")
  ) +
  theme_classic(base_size = bs) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = bs, face = "bold")
  ) +
  ylab("Testosterone (ng/ml)") +
  xlab("Species")










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







# model
m2 <- glmmTMB(
  testo_log ~ species * sex + sex * poly(date_doy, 2) + species * sex * smi_z +
    (1 | year_) + (1 | ID),
  family = gaussian(link = "identity"),
  data = ds
)

# model
m3 <- glmmTMB(
  testo_log ~ species * sex + species * poly(date_doy, 2) + species * sex * smi_z +
    (1 | year_) + (1 | ID),
  family = gaussian(link = "identity"),
  data = ds
)

# model
m4 <- glmmTMB(
  testo_log ~ species * sex + poly(date_doy, 2) + species * sex * smi_z +
    (1 | year_) + (1 | ID),
  family = gaussian(link = "identity"),
  data = ds
)

# model
m5 <- glmmTMB(
  testo_log ~ species * sex + poly(date_doy, 2) + species * sex * smi_z +
    (1 | year_) + (1 | ID),
  family = gaussian(link = "identity"),
  data = ds
)

# model
m5 <- glmmTMB(
  testo_log ~ species * sex + poly(date_doy, 2) + species * sex * smi_z +
    (1 | year_) + (1 | ID),
  family = gaussian(link = "identity"),
  data = ds
)


AIC(m1, m2, m3, m4)











ds[is.na(testo_log)]
ds[is.na(date_doy)]
ds[is.na(smi_z)]


ds <- ds[!is.na(HH)]
    


m_reduced <- update(m, . ~ . - species:sex:smi_z)
m_reduced <- update(m_reduced, . ~ . - species:sex:poly(date_doy, 2))
m_reduced <- update(m_reduced, . ~ . - sex:smi_z)
m_reduced <- update(m_reduced, . ~ . - species:smi_z)
m_reduced <- update(m_reduced, . ~ . - sex:poly(date_doy, 2))


anova(m, m_reduced, test = "Chisq")  # LRT



plot(allEffects(m_reduced))
summary(m_reduced)




m1 <- glmmTMB(
  testo_log ~ sin(hh2rad(HH)) + cos(hh2rad(HH)) + species * sex + species * poly(date_doy, 2) + smi_z + (1 | year_) + (1 | ID),
  family = gaussian(link = "identity"),
  data = ds
)


ds[is.na(HH)]

ds[, .N, .(species, sex)]


m1 <- glmmTMB(
  testo_log ~ sin(hh2rad(HH)) + cos(hh2rad(HH)) + species * sex + sex * poly(date_doy, 2) + smi_z + (1 | year_) + (1 | ID),
  family = gaussian(link = "identity"),
  data = ds
)


m2 <- glmmTMB(
  testo_log ~ species * sex + sex * poly(date_doy, 2) + smi_z + (1 | year_) + (1 | ID),
  family = gaussian(link = "identity"),
  data = ds
)

anova(m1, m2, test = "Chisq")  # LRT

summary(m1)
summary(m2)


plot(allEffects(m2))


library(emmeans)

emm <- emmeans(m2, ~ species * sex)



pairs(emm, by = "sex")  






library(MuMIn)

options(na.action = "na.fail")  # required for dredge

m_full <- glmmTMB(
  testo_log ~ 
    species * sex * poly(date_doy, 2) +
    species * sex * smi_z +
    (1 | year_) + (1 | ID),
  family = gaussian(link = "identity"),
  data = ds
)

dd <- dredge(m_full)  # all possible subsets of fixed effects
dd       





m <- glmmTMB(
  testo_log ~ sin(hh2rad(HH)) + species + poly(date_doy, 2) * species + smi_z + (1 | year_) + (1 | ID),
  family = gaussian(link = "identity"),
  data = ds
)

plot(allEffects(m))
summary(m)





# exclude GnRH induced samples
ds <- d[is.na(GnRH)]

# exclude NA
ds <- ds[!is.na(haema)]




m <- glmmTMB(
  haema ~ sex * species * poly(date_doy, 2) + species * sex * testo_log + smi_z +
    (1 | year_) + (1 | ID),
  family = gaussian(link = "identity"),
  data = ds
)

m <- glmmTMB(
  haema ~ sex * species * poly(date_doy, 2) + testo_log + smi_z +
    (1 | year_) + (1 | ID),
  family = gaussian(link = "identity"),
  data = ds
)

# model summary
summary(m)
plot(allEffects(m))

# Create polynomial columns first
ds$pd1 <- poly(ds$date_doy, 2)[,1]
ds$pd2 <- poly(ds$date_doy, 2)[,2]

m2 <- glmmTMB(
  haema ~ sex * species * pd1 + sex * pd2 + testo_log + smi_z +
    (1 | year_) + (1 | ID),
  family = gaussian(link = "identity"),
  data = ds
)


# model summary
summary(m2)
plot(allEffects(m2))

anova(m, m2, test = "Chisq")




m3 <- glmmTMB(
  haema ~ species * pd1 + sex * pd2 + testo_log + smi_z +
    (1 | year_) + (1 | ID),
  family = gaussian(link = "identity"),
  data = ds
)


# model summary
summary(m3)
plot(allEffects(m3))

anova(m2, m3, test = "Chisq")






m1 <- glmmTMB(
  haema ~ sex * species * poly(date_doy, 2) + species * sex * testo_log + species * sex * smi_z +
    (1 | year_) + (1 | ID),
  family = gaussian(link = "identity"),
  data = ds
)


# model summary
summary(m1)
plot(allEffects(m1))

# reduce model
m2 <- update(m1, . ~ . - sex:species:smi_z)
anova(m1, m2, test = "Chisq")

# model summary
summary(m2)
plot(allEffects(m2))

# reduce model
m3 <- update(m2, . ~ . - sex:species:testo_log)
anova(m2, m3, test = "Chisq")

# model summary
summary(m3)
plot(allEffects(m3))

# reduce model
m4 <- glmmTMB(
  haema ~ sex * species * date_doy + species * testo_log + sex * testo_log + sex * smi_z + sex * poly(date_doy, 2) +
    (1 | year_) + (1 | ID),
  family = gaussian(link = "identity"),
  data = ds
)
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
m6 <- update(m5, . ~ . - sex:species:poly(date_doy, 2))
anova(m5, m6, test = "Chisq")

# model summary
summary(m6)
plot(allEffects(m6))












