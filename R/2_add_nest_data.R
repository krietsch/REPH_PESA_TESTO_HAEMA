



# packages
library(data.table)
library(ggplot2)
library(patchwork)
library(DBI)


#-------------------------------------------------------------------------------
# REPH and PESA nest data
#-------------------------------------------------------------------------------

# file path to SQLite databse files
scidb <- "C:/Users/jkrietsch/scidb/"

# database
con <- dbConnect(RSQLite::SQLite(), paste0(scidb, "REPHatBARROW.sqlite"))
dn <- dbGetQuery(con, "SELECT * FROM [NESTS]") |> data.table()
dbDisconnect(con)

# subset own data
dn <- dn[external == 0]

# transform datetimes (are actually AKDT)
dn[, initiation := as.POSIXct(initiation, tz = "UTC")]

# change to day of the year
dn[, initiation_doy := yday(initiation)]

# copy to combine later
dn_reph <- copy(dn)

# database
con <- dbConnect(RSQLite::SQLite(), paste0(scidb, "PESAatBARROW.sqlite"))
dn <- dbGetQuery(con, "SELECT * FROM [NESTS]") |> data.table()
dbDisconnect(con)

# set names
setnames(dn, "laying_date", "initiation")

# transform datetimes (are actually AKDT)
dn[, initiation := as.POSIXct(initiation, tz = "UTC")]

# change to day of the year
dn[, initiation_doy := yday(initiation)]

# copy to combine later
dn_pesa <- copy(dn)

# combine data
dn <- rbind(
  dn_reph[, .(species = "REPH", year_, nest, initiation, initiation_doy)],
  dn_pesa[, .(species = "PESA", year_, nest, initiation, initiation_doy)]
)






# p1 <- 
ggplot(dn, aes(x = initiation_doy, y = factor(year_))) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  scale_x_continuous(limits = c(140, 206), expand = expansion(add = c(0, 0))) +
  theme_classic(base_size = 12) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ylab("Year") +
  xlab("Day of the year")



# p1 <- 
ggplot(dn, aes(x = initiation_doy, y = factor(species))) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  scale_x_continuous(limits = c(140, 206), expand = expansion(add = c(0, 0))) +
  theme_classic(base_size = 12) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ylab("Year") +
  xlab("Day of the year")


# load data
d <- fread("./DATA/REPH_PESA_testo_haema.csv", yaml = TRUE)


# testo pg/ml to ng/ml
d[, testo := testo / 1000]
d[, testo_log := log10(testo)]

# bleeding time
d[, diff_caught_bled := difftime(bled_time, caught_time, units = 'mins') |>
    as.numeric()]

# data as Julian
d[, date_doy := yday(date_)]

# exclude GnRH induced samples
ds <- d[is.na(GnRH)]

##### subset males
dm <- ds[sex == "M"]

p2 <- 
ggplot() +
  geom_point(
    data = dm, aes(date_doy, testo, color = species), size = 0.5, alpha = 0.5
  ) +
  geom_smooth(
    data = dm, aes(date_doy, testo, color = species), size = 0.5, alpha = 0.5
  ) +
  scale_color_manual(values = c("steelblue4", "indianred3")) +
  scale_fill_manual(values = c("steelblue4", "indianred3")) +
  scale_y_log10(
    limits = c(0.001, 350),
    breaks = c(0.001, 0.01, 0.1, 1, 10, 100),
    labels = c(0.001, 0.01, 0.1, 1, 10, 100)
  ) +
  annotation_logticks(sides = "l") +
  scale_x_continuous(limits = c(140, 206), expand = expansion(add = c(0, 0))) +
  theme_classic(base_size = 12) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ylab("Testosterone (ng/ml)") +
  xlab("Day of the year")

p2 / p1

