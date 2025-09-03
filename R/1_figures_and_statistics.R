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
library(ggnewscale)
library(cowplot)
library(knitr)
library(glmmTMB)
library(emmeans)
library(effects)
library(broomExtra)
library(DHARMa)
library(performance)
library(flextable)
library(officer)

# load data
d <- fread("./DATA/REPH_PESA_testo_haema.csv", yaml = TRUE)
dn <- fread("./DATA/REPH_PESA_nests.csv", yaml = TRUE)

# Lines to run to create html output
opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())
# rmarkdown::render(
#   "./R/1_figures_and_statistics.R",
#   output_dir = "./OUTPUTS/R_COMPILED"
# )

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

# capture date independent of year
d[, date_y := as.Date(format(date_, "2100-%m-%d"))]

# year as character
d[, year_ := as.character(year_)]
d[, .N, by = .(species, year_)]

# factor order
d[, species := factor(species, levels = c("PESA", "REPH"))]
d[, sex := factor(sex, levels = c("M", "F"))]

# min max scale
d[, .(min(date_doy), max(date_doy))]
d[, .(min(testo), max(testo))]

# clutch initiation date independent of year
dn[, initiation_y := as.Date(format(initiation, "2100-%m-%d"))]


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
  speciesREPH:sexF;                    Species x Sex
  sexF:date_doy;                       Sex:Day of the year
  speciesREPH:poly(date_doy, 2)1;      Species x Day of the year (linear)
  speciesREPH:poly(date_doy, 2)2;      Species x Day of the year (quadratic)
  sexF:poly(date_doy, 2)1;             Sex x Day of the year (linear)
  sexF:poly(date_doy, 2)2;             Sex x Day of the year (quadratic)
  speciesREPH:sexF:poly(date_doy, 2)1; Species x Sex x Day of the year (linear)
  speciesREPH:sexF:poly(date_doy, 2)2; Species x Sex x Day of the year (quadratic)
  testo_log;                           Testosterone concentration
  GnRH_sampleGnRH-induced;             GnRH (induced)
  GnRHlow;                             GnRH concentration (low)
  speciesREPH:GnRH_sampleGnRH-induced; Species (red phalarope) x GnRH (induced)
  smi_z;                               Scaled mass index
  speciesREPH:smi_z;                   Species (red phalarope) x Scaled mass index
  sexF:smi_z;                          Sex (female) x Scaled mass index
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
#' # Sampling period in relation to breeding
#-------------------------------------------------------------------------------

# years with data by species
dn[, .N, .(year_, species)]

# sample size
dns <- dn[, .N, species]

# exclude GnRH induced samples
ds <- d[is.na(GnRH)]
ds_gnrh <- d[!is.na(GnRH)]  # keep only GnRH samples

# clutch initiation periods for years with data
p1 <-
  ggplot(
    dn, aes(x = initiation_y, y = factor(species), fill = factor(species))
  ) +
  geom_boxplot(alpha = 0.7) +
  # add GnRH sample points
  geom_point(
    data = ds_gnrh,
    aes(x = date_y, y = factor(species)),
    shape = 23, size = 1, stroke = 1.2, fill = "black",
    position = position_dodge(width = 0.75)
  ) +
  scale_fill_manual(values = c("steelblue4", "indianred3")) +
  # add sample size
  geom_text(
    data = dns,
    aes(
      x = as.Date("2100-07-20"),
      y = factor(species), label = paste0("N = ", N)
    ),
    inherit.aes = FALSE,
    size = ls
  ) +
  scale_x_date(
    limits = as.Date(c("2100-05-20", "2100-07-25")),
    expand = expansion(add = c(0, 0)),
    date_labels = "%b %d",
    date_breaks = "7 days"
  ) +
  theme_classic(base_size = bs) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = bs, face = "bold"),
    axis.title.x = element_text(size = 11)
  ) +
  scale_y_discrete(labels = c(
    "REPH" = "Red\nphalarope", "PESA" = "Pectoral\nsandpiper"
  )) +
  ggtitle("Timing of clutch initiation") +
  ylab("Species") +
  xlab("")

# sample size
dss <- ds[, .N, .(species, sex)]
du <- unique(ds, by = "ID")
du <- du[, .(N_ind = .N), .(species, sex)]
dss <- merge(dss, du, by = c("species", "sex"))
dss[, sample_size := paste0("N = ", N, " | ", N_ind)]

# plot timing of sampling
p2 <-
  ggplot(ds, aes(x = date_y, y = factor(species), fill = sex, color = species)) +
  geom_boxplot(alpha = 0.7, show.legend = TRUE) +
  scale_color_manual(values = c("steelblue4", "indianred3")) +
  scale_fill_manual(values = c("#7aa048", "#E69F00")) +
  new_scale_fill() +
  # add GnRH sample points
  geom_point(
    data = ds_gnrh,
    aes(x = date_y, y = factor(species), fill = species),
    shape = 23, size = 1, stroke = 1.2,
    position = position_dodge(width = 0.75)
  ) +
  # add sample size
  geom_text(
    data = dss,
    aes(
      x = as.Date("2100-07-20"),
      y = factor(species),
      group = sex,
      label = sample_size
    ),
    position = position_dodge(width = 0.75),
    inherit.aes = FALSE,
    size = ls
  ) +
  scale_fill_manual(values = c("steelblue4", "indianred3")) +
  scale_x_date(
    limits = as.Date(c("2100-05-20", "2100-07-25")),
    expand = expansion(add = c(0, 0)),
    date_labels = "%b %d",
    date_breaks = "7 days"
  ) +
  theme_classic(base_size = bs) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = bs, face = "bold"),
    axis.title.x = element_text(size = 11)
  ) +
  scale_y_discrete(labels = c(
    "REPH" = "Red\nphalarope", "PESA" = "Pectoral\nsandpiper"
  )) +
  ggtitle("Timing of captures") +
  ylab("Species") +
  xlab("Date")


### make a combined legend

# dummy data
dt <- data.table(
  x = 1:8,
  y = 1:8,
  species = rep(c("Pectoral Sandpiper", "Red Phalarope"), 4),
  sex = rep(c("Male", "Female"), each = 4)
)

# order factor
dt[, species := factor(species, levels = c(
  "Red Phalarope", "Pectoral Sandpiper"
))]

# legend for species
p_species <- ggplot(dt, aes(x, y, color = species)) +
  geom_point(size = 3) +
  scale_color_manual(
    name = "Species",
    values = c(
      "Pectoral Sandpiper" = "steelblue4", "Red Phalarope" = "indianred3"
    )
  ) +
  theme_classic(base_size = bs) +
  theme(legend.position = "right") +
  guides(color = guide_legend(title.position = "left", title.hjust = 1))

# legend for sex
p_sex <- ggplot(dt, aes(x, y, color = sex)) +
  geom_point(size = 3) +
  scale_color_manual(
    name = "Sex",
    values = c("Male" = "#7aa048", "Female" = "#E69F00")
  ) +
  theme_classic(base_size = bs) +
  theme(legend.position = "right") +
  guides(color = guide_legend(title.position = "left", title.hjust = 1))

# legend for GnRH treatment days
dt <- data.table(x = 1, y = 1, treatment = "GnRH treatment days")

p_gnrh <- ggplot(dt, aes(x, y, shape = treatment)) +
  geom_point(size = 2, stroke = 1.2, color = "black", fill = "black") +
  scale_shape_manual(values = c("GnRH treatment days" = 23), name = "") +
  theme_classic(base_size = bs) +
  theme(legend.position = "right")

# extract each legend
leg_species <- get_legend(p_species)
leg_sex <- get_legend(p_sex)
leg_gnrh <- get_legend(p_gnrh)

# combine legends
legend <- plot_grid(leg_species, leg_sex, leg_gnrh,
                    nrow = 1, align = "h"
)

# combine plots
p1 / p2 + legend +
  plot_layout(heights = c(1, 2, 1)) +
  plot_annotation(tag_levels = list(c("a", "b"), ""))

# save plot
ggsave(
  "./OUTPUTS/FIGURES/timing_of_sampling.tiff",
  plot = last_plot(), width = 177, height = 120,
  units = c("mm"), dpi = "print"
)

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
#' # Testosterone between species and sexes comparison
#-------------------------------------------------------------------------------

# exclude GnRH induced samples
ds <- d[is.na(GnRH)]

# sample size
dss <- ds[, .N, by = .(species, sex)]
du <- unique(ds, by = "ID")
du <- du[, .(N_ind = .N), by = .(species, sex)]
dss <- merge(dss, du, by = c("species", "sex"))
dss[, sample_size := paste0("N = ", N, " | ", N_ind)]

# start with full model to check interactions with sex and species

# model
m1 <- glmmTMB(
  testo_log ~ species * sex * poly(date_doy, 2) + species * sex * smi_z +
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

# check mpdel assumptions
res <-simulateResiduals(m, plot = T)
testDispersion(res)


### create clean summary table
y <- tidy(m) |> data.table()
x <- r2(m, tolerance = 1e-10) |> data.table()

setnames(x, c("estimate"))
y[term == "sd__(Intercept)", term := paste0(term, "_", group)]
x[, estimate := as.numeric(estimate)]
x[, term := c("r2cond", "r2marg")]
y <- rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
y[, row_order := rownames(y) |> as.numeric()]
y <- merge(y, pn, by.x = "term", by.y = "parname")
setorder(y, row_order)
y <- y[, .(
  Parameter = parameter, Estimate = estimate, SE = std.error,
  Statistic = statistic, p = p.value
)]
y <- y %>% mutate_if(is.numeric, ~ round(., 3)) # round all numeric columns

# save table in word
ft <- flextable(y) |> autofit()
ft <- bold(ft, bold = TRUE, part = "header")
ESM <- ESM |>
  body_add_par(paste0("Table S1. LMM testo")) |>
  body_add_par("") |>
  body_add_flextable(ft)
ESM <- ESM |> body_add_break(pos = "after")

# post-hoc tests
emm <- emmeans(m, ~ sex * species)
y <- pairs(emm) |> tidy() |> data.table()
y <- y[, .(
  Contrast = contrast, Estimate = estimate, SE = std.error,
  Statistic = statistic, p = adj.p.value
)]
y <- y %>% mutate_if(is.numeric, ~ round(., 3)) # round all numeric columns

# save table in word
ft <- flextable(y) |> autofit()
ft <- bold(ft, bold = TRUE, part = "header")
ESM <- ESM |>
  body_add_par(paste0("Table S2. LMM testo post-hoc")) |>
  body_add_par("") |>
  body_add_flextable(ft)
ESM <- ESM |> body_add_break(pos = "after")

# tr <- emtrends(m, ~ species | sex, var = "date_doy")
# pairs(tr)  
# 
# tr <- emtrends(m, ~ sex | species, var = "date_doy")
# pairs(tr)  
# 
# emm <- emmeans(m, ~ species | sex)  
# pairs(emm, by = "sex")
# 
# emm <- emmeans(m, ~ sex | species)  
# pairs(emm, by = "species")

### extract mean effect of sex and species
e <- effect("species:sex", m) |>
  data.frame() |>
  setDT()

# back transform
e[, `:=`(
  fit_back = 10^fit,
  se_back = 10^se,
  lower_back = 10^lower,
  upper_back = 10^upper
)]

# plot for males
p1 <-
  ggplot() +
  ggtitle("Males") +
  geom_violin(data = ds[sex == "M"], aes(species, testo, fill = species),
              alpha = 0.7, draw_quantiles = c(0.5)) +
  geom_point(
    data = e[sex == "M"], aes(species, 10^fit, color = species),
    position = position_dodge(0.5), size = 2
  ) +
  geom_linerange(
    data = e[sex == "M"], aes(
      x = species, ymin = 10^upper, ymax = 10^lower,
      color = species
    ), linewidth = 0.5,
    position = position_dodge(width = 0.5)
  ) +
  scale_fill_manual(values = c("steelblue4", "indianred3")) +
  scale_color_manual(values = c("black", "black")) +
  geom_text(
    data = dss[sex == "M"], aes(species, Inf, label = sample_size),
    vjust = 1, size = ls
  ) +
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

# plot for females
p2 <-
  ggplot() +
  ggtitle("Females") +
  geom_violin(data = ds[sex == "F"], aes(species, testo, fill = species),
              alpha = 0.7, draw_quantiles = c(0.5)) +
  geom_point(
    data = e[sex == "F"], aes(species, 10^fit, color = species),
    position = position_dodge(0.5), size = 2
  ) +
  geom_linerange(
    data = e[sex == "F"], aes(
      x = species, ymin = 10^upper, ymax = 10^lower,
      color = species
    ), linewidth = 0.5,
    position = position_dodge(width = 0.5)
  ) +
  scale_fill_manual(values = c("steelblue4", "indianred3")) +
  scale_color_manual(values = c("black", "black")) +
  geom_text(
    data = dss[sex == "F"], aes(species, Inf, label = sample_size),
    vjust = 1, size = ls
  ) +
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
  ylab("") +
  xlab("Species")


### effect of season on testo
es <- effect("species:sex:poly(date_doy, 2)", m, 
             xlevels = list(date_doy = 1000)) |> 
  data.frame() |> 
  setDT()

# subset period with data
dr <- ds[, .(first_data = min(date_doy), last_data = max(date_doy)),
         by = .(species, sex)
]
es <- merge(es, dr, by = c("species", "sex"), all.x = TRUE)
es[, in_range := date_doy %between% c(first_data, last_data), by = 1:nrow(es)]
es <- es[in_range == TRUE]

# transform into date
es[, date_y := as.Date(date_doy - 1, origin = "2100-01-01")]
ds[, date_y := as.Date(format(date_, "2100-%m-%d"))]

# plot for season effect on males
p3 <-
  ggplot() +
  geom_point(
    data = ds[sex == "M"], aes(date_y, testo, color = species),
    size = 0.5, alpha = 0.5
  ) +
  geom_line(
    data = es[sex == "M"], aes(y = 10^fit, x = date_y, color = species),
    linewidth = 0.8
  ) +
  geom_ribbon(
    data = es[sex == "M"], aes(
      y = 10^fit, x = date_y, fill = species,
      ymin = 10^lower, ymax = 10^upper
    ), alpha = 0.2
  ) +
  scale_color_manual(values = c("steelblue4", "indianred3")) +
  scale_fill_manual(values = c("steelblue4", "indianred3")) +
  scale_y_log10(
    limits = c(0.001, 350),
    breaks = c(0.001, 0.01, 0.1, 1, 10, 100),
    labels = c(0.001, 0.01, 0.1, 1, 10, 100)
  ) +
  annotation_logticks(sides = "l") +
  scale_x_date(
    limits = as.Date(c("2100-05-20", "2100-07-25")),
    expand = expansion(add = c(0, 0)),
    date_labels = "%b %d",
    breaks = seq(as.Date("2100-05-20"), as.Date("2100-07-25"), by = "14 days")
  ) +
  theme_classic(base_size = bs) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ylab("Testosterone (ng/ml)") +
  xlab("Date")

# plot for season effect on females
p4 <-
  ggplot() +
  geom_point(
    data = ds[sex == "F"], aes(date_y, testo, color = species), size = 0.5,
    alpha = 0.5
  ) +
  geom_line(
    data = es[sex == "F"], aes(y = 10^fit, x = date_y, color = species),
    linewidth = 0.8
  ) +
  geom_ribbon(
    data = es[sex == "F"], aes(
      y = 10^fit, x = date_y, fill = species,
      ymin = 10^lower, ymax = 10^upper
    ), alpha = 0.2
  ) +
  scale_color_manual(values = c("steelblue4", "indianred3")) +
  scale_fill_manual(values = c("steelblue4", "indianred3")) +
  scale_y_log10(
    limits = c(0.001, 350),
    breaks = c(0.001, 0.01, 0.1, 1, 10, 100),
    labels = c(0.001, 0.01, 0.1, 1, 10, 100)
  ) +
  annotation_logticks(sides = "l") +
  scale_x_date(
    limits = as.Date(c("2100-05-20", "2100-07-25")),
    expand = expansion(add = c(0, 0)),
    date_labels = "%b %d",
    breaks = seq(as.Date("2100-05-20"), as.Date("2100-07-25"), by = "14 days")
  ) +
  theme_classic(base_size = bs) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ylab("") +
  xlab("Date")


### effect of smi_z on testo

# define the range of smi_z
smi_range <- seq(
  min(ds$smi_z, na.rm = TRUE),
  max(ds$smi_z, na.rm = TRUE),
  length.out = 100
)

# get estimated marginal means along smi_z, separately by sex and species
es <- emmeans(m, ~ sex * species | smi_z, at = list(smi_z = smi_range)) |>
  as.data.frame() |>
  setDT()

# subset period with data
dr <- ds[, .(
  first_data = min(smi_z, na.rm = TRUE),
  last_data = max(smi_z, na.rm = TRUE)
),
by = .(species, sex)
]
es <- merge(es, dr, by = c("species", "sex"), all.x = TRUE)
es[, in_range := smi_z %between% c(first_data, last_data), by = 1:nrow(es)]
es <- es[in_range == TRUE]

# plot of effect for scaled mass index on males
p5 <-
  ggplot() +
  geom_point(
    data = ds[!is.na(smi_z) & sex == "M"], 
    aes(smi_z, testo, color = species), size = 0.5, alpha = 0.5
  ) +
  geom_line(
    data = es[sex == "M"], aes(y = 10^emmean, x = smi_z, color = species), 
    linewidth = 0.8
  ) +
  geom_ribbon(data = es[sex == "M"], aes(
    y = 10^emmean, x = smi_z, fill = species,
    ymin = 10^lower.CL, ymax = 10^upper.CL
  ), alpha = 0.2) +
  scale_color_manual(values = c("steelblue4", "indianred3")) +
  scale_fill_manual(values = c("steelblue4", "indianred3")) +
  scale_y_log10(
    limits = c(0.001, 350),
    breaks = c(0.001, 0.01, 0.1, 1, 10, 100),
    labels = c(0.001, 0.01, 0.1, 1, 10, 100)
  ) +
  annotation_logticks(sides = "l") +
  scale_x_continuous(
    limits = c(-4.5, 4.5), expand = expansion(add = c(0, 0)),
    breaks = c(-4, -2, 0, 2, 4)
  ) +
  theme_classic(base_size = bs) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ylab("Testosterone (ng/ml)") +
  xlab("Scaled mass index")

# plot of effect for scaled mass index on females
p6 <-
  ggplot() +
  geom_point(
    data = ds[!is.na(smi_z) & sex == "F"], 
    aes(smi_z, testo, color = species), size = 0.5, alpha = 0.5
  ) +
  geom_line(
    data = es[sex == "F"], aes(y = 10^emmean, x = smi_z, color = species), 
    linewidth = 0.8
  ) +
  geom_ribbon(data = es[sex == "F"], aes(
    y = 10^emmean, x = smi_z, fill = species,
    ymin = 10^lower.CL, ymax = 10^upper.CL
  ), alpha = 0.2) +
  scale_color_manual(values = c("steelblue4", "indianred3")) +
  scale_fill_manual(values = c("steelblue4", "indianred3")) +
  scale_y_log10(
    limits = c(0.001, 350),
    breaks = c(0.001, 0.01, 0.1, 1, 10, 100),
    labels = c(0.001, 0.01, 0.1, 1, 10, 100)
  ) +
  annotation_logticks(sides = "l") +
  scale_x_continuous(
    limits = c(-4.5, 4.5), expand = expansion(add = c(0, 0)),
    breaks = c(-4, -2, 0, 2, 4)
  ) +
  theme_classic(base_size = bs) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ylab("") +
  xlab("Scaled mass index")


# merge plots
p1 + p2 + p3 + p4 + p5 + p6 +
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = "a")

ggsave(
  "./OUTPUTS/FIGURES/testo_by_sex_and_species_new.tiff",
  plot = last_plot(), width = 177, height = 264,
  units = c("mm"), dpi = "print"
)

#-------------------------------------------------------------------------------
#' # GnRH experiment
#-------------------------------------------------------------------------------

# subset birds with GnRH
IDe <- d[!is.na(GnRH)]$ID
ds <- d[ID %in% IDe]

# exclude third testo sample
ds <- ds[!(ID == 270170318 & date_ == "2017-06-01")]

# sample type
ds[, GnRH_sample := ifelse(is.na(GnRH), "Baseline", "GnRH-induced")]
ds[, v20 := ifelse(min(volume) > 20, TRUE, FALSE), by = ID]

ID_l <- ds[GnRH == "low"]$ID
ds[ID %in% ID_l, GnRH := "low"]

ID_h <- ds[GnRH == "high"]$ID
ds[ID %in% ID_h, GnRH := "high"]

# check dates
ds[, .N, .(species, year_, date_doy, date_y)]
ds[, species_sample := paste0(species, "_", GnRH_sample)]

# min max scale
ds[, .(min(date_doy), max(date_doy))]
ds[, .(min(testo), max(testo))]

# subset males
dm <- ds[sex == "M"]

# sample size
dms <- dm[, .N, by = species]
dms[, sample_size := paste0("N = ", N / 2)]
dms[, species_sample := paste0(species, "_Baseline")]

# model
m <- glmmTMB(
  testo_log ~ species * GnRH_sample + GnRH + (1 | ID),
  family = gaussian(link = "identity"),
  data = dm
)

# plot(allEffects(m))
summary(m)

e <- effect("species:GnRH_sample", m, xlevels = 2) |>
  data.frame() |>
  setDT()

e[, species_sample := paste0(species, "_", GnRH_sample)]

# create clean summary table
y <- tidy(m) |> data.table()
x <- r2(m) |> data.table()

setnames(x, c("estimate"))
y[term == "sd__(Intercept)", term := paste0(term, "_", group)]
x[, estimate := as.numeric(estimate)]
x[, term := c("r2cond", "r2marg")]
y <- rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
y[, row_order := rownames(y) |> as.numeric()]
y <- merge(y, pn, by.x = "term", by.y = "parname")
setorder(y, row_order)
y <- y[, .(Parameter = parameter, Estimate = estimate, SE = std.error,
           Statistic = statistic, p = p.value)]
y <- y %>% mutate_if(is.numeric, ~ round(., 3)) # round all numeric columns

# save table in word
ft <- flextable(y) |> autofit()
ft <- bold(ft, bold = TRUE, part = "header")
ESM <- ESM |>
  body_add_par(paste0("Table S3. LMM males GnRH")) |>
  body_add_par("") |>
  body_add_flextable(ft)
ESM <- ESM |> body_add_break(pos = "after")

### percentage of effect change

# back transform
e[, `:=`(
  fit_back = 10^fit,
  se_back = 10^se,
  lower_back = 10^lower,
  upper_back = 10^upper
)]

# percent change from baseline
e[, `:=`(
  perc_change = (fit_back - fit_back[GnRH_sample == "Baseline"]) /
    fit_back[GnRH_sample == "Baseline"] * 100,
  perc_change_low = (lower_back - fit_back[GnRH_sample == "Baseline"]) /
    fit_back[GnRH_sample == "Baseline"] * 100,
  perc_change_high = (upper_back - fit_back[GnRH_sample == "Baseline"]) /
    fit_back[GnRH_sample == "Baseline"] * 100
), by = species]

# summary
e[GnRH_sample == "GnRH-induced", .(
  species,
  perc_change,
  perc_change_low,
  perc_change_high
)]

# GnRH experiment plot for males
p1 <-
  ggplot() +
  ggtitle("Males") +
  geom_text(
    data = dms, aes(species_sample, Inf, label = sample_size), vjust = 1,
    hjust = -0.5, size = ls
  ) +
  geom_point(
    data = e[GnRH_sample == "Baseline"],
    aes(species_sample, 10^fit,
        color = species
    ),
    position = position_nudge(x = -0.2), size = 2
  ) +
  geom_point(
    data = e[GnRH_sample == "GnRH-induced"],
    aes(species_sample, 10^fit,
        color = species
    ),
    position = position_nudge(x = 0.2), size = 2
  ) +
  geom_linerange(
    data = e[GnRH_sample == "Baseline"],
    aes(
      x = species_sample, ymin = 10^upper, ymax = 10^lower,
      color = species
    ), linewidth = 0.5,
    position = position_nudge(x = -0.2)
  ) +
  geom_linerange(
    data = e[GnRH_sample == "GnRH-induced"],
    aes(
      x = species_sample, ymin = 10^upper, ymax = 10^lower,
      color = species
    ), linewidth = 0.5,
    position = position_nudge(x = 0.2)
  ) +
  scale_color_manual(values = c("steelblue4", "indianred3")) +
  ggnewscale::new_scale_fill() +
  geom_line(data = dm, aes(species_sample, testo, group = ID), size = 0.3) +
  geom_point(
    data = dm, aes(species_sample, testo, fill = GnRH),
    shape = 21, size = 1
  ) +
  scale_fill_manual(values = c("black", "white")) +
  scale_y_log10(
    limits = c(0.1, 50),
    breaks = c(0.01, 0.1, 1, 10),
    labels = c(0.01, 0.1, 1, 10)
  ) +
  annotation_logticks(sides = "l") +
  scale_x_discrete(
    breaks = c(
      "PESA_Baseline", "PESA_GnRH-induced",
      "REPH_Baseline", "REPH_GnRH-induced"
    ),
    labels = c("Baseline", "GnRH", "Baseline", "GnRH")
  ) +
  theme_classic(base_size = bs) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = bs, face = "bold"),
    axis.title.x = element_text(size = 11)
  ) +
  ylab("Testosterone (ng/ml)") +
  xlab("Pectoral Sandpiper   Red Phalarope     ")

# subset females
df <- ds[sex == "F"]

# sample size
dfs <- df[, .N, by = species]
dfs[, sample_size := paste0("N = ", N / 2)]
dfs[, species_sample := paste0(species, "_Baseline")]

# model
m <- glmmTMB(
  testo_log ~ species * GnRH_sample + GnRH + (1 | ID),
  family = gaussian(link = "identity"),
  data = df
)

# plot(allEffects(m))
summary(m)

e <- effect("species:GnRH_sample", m, xlevels = 2) |>
  data.frame() |>
  setDT()

e[, species_sample := paste0(species, "_", GnRH_sample)]

# create clean summary table
y <- tidy(m) |> data.table()
x <- r2(m) |> data.table()

setnames(x, c("estimate"))
y[term == "sd__(Intercept)", term := paste0(term, "_", group)]
x[, estimate := as.numeric(estimate)]
x[, term := c("r2cond", "r2marg")]
y <- rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
y[, row_order := rownames(y) |> as.numeric()]
y <- merge(y, pn, by.x = "term", by.y = "parname")
setorder(y, row_order)
y <- y[, .(Parameter = parameter, Estimate = estimate, SE = std.error,
           Statistic = statistic, p = p.value)]
y <- y %>% mutate_if(is.numeric, ~ round(., 3)) # round all numeric columns

# save table in word
ft <- flextable(y) |> autofit()
ft <- bold(ft, bold = TRUE, part = "header")
ESM <- ESM |>
  body_add_par(paste0("Table S4. LMM females GnRH")) |>
  body_add_par("") |>
  body_add_flextable(ft)
ESM <- ESM |> body_add_break(pos = "after")

### percentage of effect change

# back transform
e[, `:=`(
  fit_back = 10^fit,
  se_back = 10^se,
  lower_back = 10^lower,
  upper_back = 10^upper
)]

# percent change from baseline
e[, `:=`(
  perc_change = (fit_back - fit_back[GnRH_sample == "Baseline"]) /
    fit_back[GnRH_sample == "Baseline"] * 100,
  perc_change_low = (lower_back - fit_back[GnRH_sample == "Baseline"]) /
    fit_back[GnRH_sample == "Baseline"] * 100,
  perc_change_high = (upper_back - fit_back[GnRH_sample == "Baseline"]) /
    fit_back[GnRH_sample == "Baseline"] * 100
), by = species]

# summary
e[GnRH_sample == "GnRH-induced", .(
  species,
  perc_change,
  perc_change_low,
  perc_change_high
)]

# GnRH experiment plot for females
p2 <-
  ggplot() +
  ggtitle("Females") +
  geom_text(
    data = dfs, aes(species_sample, Inf, label = sample_size),
    vjust = 1, hjust = -0.5, size = ls
  ) +
  geom_point(
    data = e[GnRH_sample == "Baseline"], aes(species_sample, 10^fit,
                                             color = species
    ),
    position = position_nudge(x = -0.2), size = 2
  ) +
  geom_point(
    data = e[GnRH_sample == "GnRH-induced"], aes(species_sample, 10^fit,
                                                 color = species
    ),
    position = position_nudge(x = 0.2), size = 2
  ) +
  geom_linerange(
    data = e[GnRH_sample == "Baseline"],
    aes(x = species_sample, ymin = 10^upper, ymax = 10^lower, color = species),
    linewidth = 0.5,
    position = position_nudge(x = -0.2)
  ) +
  geom_linerange(
    data = e[GnRH_sample == "GnRH-induced"],
    aes(x = species_sample, ymin = 10^upper, ymax = 10^lower, color = species),
    linewidth = 0.5,
    position = position_nudge(x = 0.2)
  ) +
  scale_color_manual(values = c("steelblue4", "indianred3")) +
  ggnewscale::new_scale_fill() +
  geom_line(data = df, aes(species_sample, testo, group = ID), size = 0.3) +
  geom_point(
    data = df, aes(species_sample, testo, fill = GnRH), shape = 21,
    size = 1
  ) +
  scale_fill_manual(values = c("black", "white")) +
  scale_y_log10(
    limits = c(0.1, 50),
    breaks = c(0.01, 0.1, 1, 10),
    labels = c(0.01, 0.1, 1, 10)
  ) +
  annotation_logticks(sides = "l") +
  scale_x_discrete(
    breaks = c(
      "PESA_Baseline", "PESA_GnRH-induced",
      "REPH_Baseline", "REPH_GnRH-induced"
    ),
    labels = c("Baseline", "GnRH", "Baseline", "GnRH")
  ) +
  theme_classic(base_size = bs) +
  theme(
    legend.position = "none", 
    plot.title = element_text(hjust = 0.5, size = bs, face = "bold"),
    axis.title.x = element_text(size = 11)
  ) +
  ylab("") +
  xlab("Pectoral Sandpiper   Red Phalarope     ")

# merge plots
p1 + p2 +
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = "a")


ggsave(
  "./OUTPUTS/FIGURES/testo_GnRH.tiff",
  plot = last_plot(), width = 177, height = 88,
  units = c("mm"), dpi = "print"
)

#-------------------------------------------------------------------------------
#' # Testosterone influence on hematocrit
#-------------------------------------------------------------------------------

# exclude GnRH induced samples
ds <- d[is.na(GnRH)]

# exclude NA
ds <- ds[!is.na(haema)]

# sample size
dss <- ds[, .N, by = .(species, sex)]
du <- unique(ds, by = "ID")
du <- du[, .(N_ind = .N), by = .(species, sex)]
dss <- merge(dss, du, by = c("species", "sex"))
dss[, sample_size := paste0("N = ", N, " | ", N_ind)]

# start with full model to check interactions with sex and species

# model
m1 <- glmmTMB(
  haema ~ sex * species * poly(date_doy, 2) + sex * species * testo_log +
    sex * species * smi_z +
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
  haema ~ species * sex * date_doy + testo_log + smi_z +
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
  haema ~ species * sex * poly(date_doy, 2) + testo_log + smi_z +
    (1 | year_) + (1 | ID),
  family = gaussian(link = "identity"),
  data = ds
)

# model summary
summary(m)
plot(allEffects(m))

# check mpdel assumptions
res <-simulateResiduals(m, plot = T)
testDispersion(res)


### create clean summary table
y <- tidy(m) |> data.table()
x <- r2(m, tolerance = 1e-10) |> data.table()

setnames(x, c("estimate"))
y[term == "sd__(Intercept)", term := paste0(term, "_", group)]
x[, estimate := as.numeric(estimate)]
x[, term := c("r2cond", "r2marg")]
y <- rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
y[, row_order := rownames(y) |> as.numeric()]
y <- merge(y, pn, by.x = "term", by.y = "parname")
setorder(y, row_order)
y <- y[, .(
  Parameter = parameter, Estimate = estimate, SE = std.error,
  Statistic = statistic, p = p.value
)]
y <- y %>% mutate_if(is.numeric, ~ round(., 3)) # round all numeric columns

# save table in word
ft <- flextable(y) |> autofit()
ft <- bold(ft, bold = TRUE, part = "header")
ESM <- ESM |>
  body_add_par(paste0("Table S5. LMM haema")) |>
  body_add_par("") |>
  body_add_flextable(ft)
ESM <- ESM |> body_add_break(pos = "after")

# post-hoc tests
emm <- emmeans(m, ~ sex * species)
y <- pairs(emm) |> tidy() |> data.table()
y <- y[, .(
  Contrast = contrast, Estimate = estimate, SE = std.error,
  Statistic = statistic, p = adj.p.value
)]
y <- y %>% mutate_if(is.numeric, ~ round(., 3)) # round all numeric columns

# save table in word
ft <- flextable(y) |> autofit()
ft <- bold(ft, bold = TRUE, part = "header")
ESM <- ESM |>
  body_add_par(paste0("Table S6. LMM haema post-hoc")) |>
  body_add_par("") |>
  body_add_flextable(ft)
ESM <- ESM |> body_add_break(pos = "after")

# post-hoc tests

# emm <- emmeans(m, ~ species | sex)  
# pairs(emm, by = "sex")
# 
# emm <- emmeans(m, ~ sex | species)  
# pairs(emm, by = "species")



### extract mean effect of sex and species
e <- effect("species:sex", m) |>
  data.frame() |>
  setDT()

# species comparison
p1 <-
  ggplot() +
  ggtitle("Pectoral sandpiper") +
  geom_text(
    data = dss[species == "PESA"], aes(sex, Inf, label = sample_size),
    vjust = 1, size = ls
  ) +
  geom_violin(data = ds[species == "PESA"], aes(sex, haema, fill = sex), 
              alpha = 0.7, draw_quantiles = c(0.5)) +
  geom_point(
    data = e[species == "PESA"], aes(sex, fit), color = "black",
    position = position_dodge(0.5), size = 2
  ) +
  geom_linerange(
    data = e[species == "PESA"], aes(x = sex, ymin = upper, ymax = lower),
    color = "black", linewidth = 0.5,
    position = position_dodge(width = 0.5)
  ) +
  scale_fill_manual(values = c("#7aa048", "#E69F00")) +
  scale_y_continuous(limits = c(34, 73), expand = expansion(add = c(0, 0))) +
  scale_x_discrete(labels = c("M" = "Male", "F" = "Female")) +
  theme_classic(base_size = bs) +
  theme(
    legend.position = "none", 
    plot.title = element_text(hjust = 0.5, size = bs, face = "bold")
  ) +
  ylab("Haematocrit (%)") +
  xlab("Sex")

p2 <-
  ggplot() +
  ggtitle("Red Phalarope") +
  geom_text(
    data = dss[species == "REPH"], aes(sex, Inf, label = sample_size),
    vjust = 1, size = ls
  ) +
  geom_violin(data = ds[species == "REPH"], aes(sex, haema, fill = sex),
              alpha = 0.7, draw_quantiles = c(0.5)) +
  geom_point(
    data = e[species == "REPH"], aes(sex, fit), color = "black",
    position = position_dodge(0.5), size = 2
  ) +
  geom_linerange(
    data = e[species == "REPH"], aes(x = sex, ymin = upper, ymax = lower),
    color = "black", linewidth = 0.5,
    position = position_dodge(width = 0.5)
  ) +
  scale_fill_manual(values = c("#7aa048", "#E69F00")) +
  scale_y_continuous(limits = c(34, 73), expand = expansion(add = c(0, 0))) +
  scale_x_discrete(labels = c("M" = "Male", "F" = "Female")) +
  theme_classic(base_size = bs) +
  theme(
    legend.position = "none", 
    plot.title = element_text(hjust = 0.5, size = bs, face = "bold")
  ) +
  ylab("") +
  xlab("Sex")

### effect of testo on haema

# define range of testo_log
testo_range <- seq(
  min(ds$testo_log, na.rm = TRUE),
  max(ds$testo_log, na.rm = TRUE),
  length.out = 100
)

# predicted values along testo_log for each species and sex
es <- emmeans(
  m, ~ sex * species | testo_log,
  at = list(testo_log = testo_range)
) |>
  as.data.frame() |>
  setDT()

# subset period with data
dr <- ds[, .(
  first_data = min(testo_log, na.rm = TRUE),
  last_data = max(testo_log, na.rm = TRUE)
),
by = .(species, sex)
]
es <- merge(es, dr, by = c("species", "sex"), all.x = TRUE)
es[, in_range := testo_log %between% c(first_data, last_data), by = 1:nrow(es)]
es <- es[in_range == TRUE]

# plot effect of testosterone pectoral sandpiper
p3 <-
  ggplot() +
  geom_point(
    data = ds[species == "PESA"], aes(10^testo_log, haema, color = sex),
    size = 0.5, alpha = 0.5
  ) +
  geom_line(
    data = es[species == "PESA"], aes(y = emmean, x = 10^testo_log, color = sex), 
    linewidth = 0.8
  ) +
  geom_ribbon(data = es[species == "PESA"], aes(
    y = emmean, x = 10^testo_log, fill = sex,
    ymin = lower.CL, ymax = upper.CL
  ), alpha = 0.2) +
  scale_color_manual(values = c("#7aa048", "#E69F00")) +
  scale_fill_manual(values = c("#7aa048", "#E69F00")) +
  scale_y_continuous(limits = c(34, 73), expand = expansion(add = c(0, 0))) +
  scale_x_log10(
    limits = c(0.01, 350),
    breaks = c(0.01, 0.1, 1, 10, 100),
    labels = c(0.01, 0.1, 1, 10, 100)
  ) +
  annotation_logticks(sides = "b") +
  theme_classic(base_size = bs) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ylab("Haematocrit (%)") +
  xlab("Testosterone (ng/ml)")

# plot effect of testosterone red phalarope
p4 <-
  ggplot() +
  geom_point(
    data = ds[species == "REPH"], aes(10^testo_log, haema, color = sex),
    size = 0.5, alpha = 0.5
  ) +
  geom_line(
    data = es[species == "REPH"], aes(y = emmean, x = 10^testo_log, color = sex), 
    linewidth = 0.8
  ) +
  geom_ribbon(data = es[species == "REPH"], aes(
    y = emmean, x = 10^testo_log, fill = sex,
    ymin = lower.CL, ymax = upper.CL
  ), alpha = 0.2) +
  scale_color_manual(values = c("#7aa048", "#E69F00")) +
  scale_fill_manual(values = c("#7aa048", "#E69F00")) +
  scale_y_continuous(limits = c(34, 73), expand = expansion(add = c(0, 0))) +
  scale_x_log10(
    limits = c(0.01, 350),
    breaks = c(0.01, 0.1, 1, 10, 100),
    labels = c(0.01, 0.1, 1, 10, 100)
  ) +
  annotation_logticks(sides = "b") +
  theme_classic(base_size = bs) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ylab("") +
  xlab("Testosterone (ng/ml)")

### effect of season on haema
es <- effect("species:sex:poly(date_doy, 2)", m, 
             xlevels = list(date_doy = 1000)) |> 
  data.frame() |> 
  setDT()

# subset period with data
dr <- ds[, .(first_data = min(date_doy), last_data = max(date_doy)),
         by = .(species, sex)
]
es <- merge(es, dr, by = c("species", "sex"), all.x = TRUE)
es[, in_range := date_doy %between% c(first_data, last_data), by = 1:nrow(es)]
es <- es[in_range == TRUE]

# transform into date
es[, date_y := as.Date(date_doy - 1, origin = "2100-01-01")]
ds[, date_y := as.Date(format(date_, "2100-%m-%d"))]

# effect of season on haema pectoral sandpiper
p5 <-
  ggplot() +
  geom_point(
    data = ds[species == "PESA"], aes(date_y, haema, color = sex), 
    size = 0.5, alpha = 0.5
  ) +
  geom_line(
    data = es[species == "PESA"],
    aes(y = fit, x = date_y, color = sex), size = 0.8) +
  geom_ribbon(
    data = es[species == "PESA"], aes(
      y = fit, x = date_y, fill = sex, ymin = lower,
      ymax = upper
    ), alpha = 0.2
  ) +
  scale_color_manual(values = c("#7aa048", "#E69F00")) +
  scale_fill_manual(values = c("#7aa048", "#E69F00")) +
  scale_y_continuous(limits = c(34, 73), expand = expansion(add = c(0, 0))) +
  scale_x_date(
    limits = as.Date(c("2100-05-20", "2100-07-25")),
    expand = expansion(add = c(0, 0)),
    date_labels = "%b %d",
    breaks = seq(as.Date("2100-05-20"), as.Date("2100-07-25"), by = "14 days")
  ) +
  theme_classic(base_size = bs) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ylab("Haematocrit (%)") +
  xlab("Date")

# effect of season on haema pectoral sandpiper
p6 <-
  ggplot() +
  geom_point(
    data = ds[species == "REPH"], aes(date_y, haema, color = sex), 
    size = 0.5, alpha = 0.5
  ) +
  geom_line(
    data = es[species == "REPH"],
    aes(y = fit, x = date_y, color = sex), size = 0.8) +
  geom_ribbon(
    data = es[species == "REPH"], aes(
      y = fit, x = date_y, fill = sex, ymin = lower,
      ymax = upper
    ), alpha = 0.2
  ) +
  scale_color_manual(values = c("#7aa048", "#E69F00")) +
  scale_fill_manual(values = c("#7aa048", "#E69F00")) +
  scale_y_continuous(limits = c(34, 73), expand = expansion(add = c(0, 0))) +
  scale_x_date(
    limits = as.Date(c("2100-05-20", "2100-07-25")),
    expand = expansion(add = c(0, 0)),
    date_labels = "%b %d",
    breaks = seq(as.Date("2100-05-20"), as.Date("2100-07-25"), by = "14 days")
  ) +
  theme_classic(base_size = bs) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ylab("") +
  xlab("Date")


### effect of smi_z on haema

# define the range of smi_z
smi_range <- seq(
  min(ds$smi_z, na.rm = TRUE),
  max(ds$smi_z, na.rm = TRUE),
  length.out = 100
)

# get estimated marginal means along smi_z, separately by sex and species
es <- emmeans(m, ~ sex * species | smi_z, at = list(smi_z = smi_range)) |>
  as.data.frame() |>
  setDT()

# subset period with data
dr <- ds[, .(
  first_data = min(smi_z, na.rm = TRUE),
  last_data = max(smi_z, na.rm = TRUE)
),
by = .(species, sex)
]
es <- merge(es, dr, by = c("species", "sex"), all.x = TRUE)
es[, in_range := smi_z %between% c(first_data, last_data), by = 1:nrow(es)]
es <- es[in_range == TRUE]

# plot effect of scaled mass index pectoral sandpiper
p7 <-
  ggplot() +
  geom_point(
    data = ds[!is.na(smi_z) & species == "PESA"], 
    aes(smi_z, haema, color = sex), size = 0.5, alpha = 0.5
  ) +
  geom_line(
    data = es[species == "PESA"], 
    aes(y = emmean, x = smi_z, color = sex), size = 0.8) +
  geom_ribbon(
    data = es[species == "PESA"], aes(
      y = emmean, x = smi_z, fill = sex, ymin = lower.CL,
      ymax = upper.CL
    ), alpha = 0.2
  ) +
  scale_color_manual(values = c("#7aa048", "#E69F00")) +
  scale_fill_manual(values = c("#7aa048", "#E69F00")) +
  scale_y_continuous(limits = c(34, 73), expand = expansion(add = c(0, 0))) +
  scale_x_continuous(
    limits = c(-4.5, 4.5), expand = expansion(add = c(0, 0)),
    breaks = c(-4, -2, 0, 2, 4)
  ) +
  theme_classic(base_size = bs) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ylab("Haematocrit (%)") +
  xlab("Scaled mass index")

# plot effect of scaled mass index red phalaropes
p8 <-
  ggplot() +
  geom_point(
    data = ds[!is.na(smi_z) & species == "REPH"], 
    aes(smi_z, haema, color = sex), size = 0.5, alpha = 0.5
  ) +
  geom_line(
    data = es[species == "REPH"], 
    aes(y = emmean, x = smi_z, color = sex), size = 0.8) +
  geom_ribbon(
    data = es[species == "REPH"], aes(
      y = emmean, x = smi_z, fill = sex, ymin = lower.CL,
      ymax = upper.CL
    ), alpha = 0.2
  ) +
  scale_color_manual(values = c("#7aa048", "#E69F00")) +
  scale_fill_manual(values = c("#7aa048", "#E69F00")) +
  scale_y_continuous(limits = c(34, 73), expand = expansion(add = c(0, 0))) +
  scale_x_continuous(
    limits = c(-4.5, 4.5), expand = expansion(add = c(0, 0)),
    breaks = c(-4, -2, 0, 2, 4)
  ) +
  theme_classic(base_size = bs) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ylab("") +
  xlab("Scaled mass index")

# merge plots
p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 +
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = "a")

(p1 + p2) / (p3 + p4) / (p5 + p6) / (p7 + p8) +
  plot_layout(heights = c(1.5, 1, 1, 1)) +
  plot_annotation(tag_levels = "a")


ggsave(
  "./OUTPUTS/FIGURES/haematocrit_species_split2.tiff",
  plot = last_plot(), width = 177, height = 264,
  units = c("mm"), dpi = "print"
)



# save word file
# print(ESM, target = "./OUTPUTS/ESM/ESM_REPH_PESA_testo_haema_analysis.docx")


# session info
sessionInfo()
