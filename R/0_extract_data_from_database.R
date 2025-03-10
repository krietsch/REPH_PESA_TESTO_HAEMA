#===============================================================================
# Data and code from "Sex differences in testosterone and haematocrit levels
# reflect mating system differences of two Arctic-breeding shorebird species"
# Contributor: Johannes Krietsch
# ❗This script is provided as reference only. It contains links to the internal
# database of the Max Planck Institute for Ornithology, from which it pulls the
# data and exports all the collected data to ./DATA
#===============================================================================

# packages
library(data.table)
library(ggplot2)
library(DBI)

# file path to SQLite databse files
scidb <- "C:/Users/jkrietsch/scidb/"

#-------------------------------------------------------------------------------
# REPH data
#-------------------------------------------------------------------------------

# database
con <- dbConnect(RSQLite::SQLite(), paste0(scidb, "REPHatBARROW.sqlite"))
dc <- dbGetQuery(con, "SELECT * FROM [CAPTURES]") |> data.table()
dt <- dbGetQuery(con, "SELECT * FROM [TESTO]") |> data.table()
ds <- dbGetQuery(con, "SELECT * FROM [SEX]") |> data.table()
dbDisconnect(con)

# transform datetimes (are actually AKDT)
dc[, caught_time := as.POSIXct(caught_time, tz = "UTC")]
dc[, bled_time := as.POSIXct(bled_time, tz = "UTC")]

# merge with lab sex
ds <- unique(ds, by = "ID")
dc <- merge(dc, ds[, .(ID = as.integer(ID), sex)], by = "ID", all.x = TRUE)

# add date
dt[, date_ := as.Date(date_)]
dc[, date_ := as.Date(caught_time)]
dc[, caught_time := as.POSIXct(caught_time)]
dc[, bled_time := as.POSIXct(bled_time)]

# merge with testo data
dc <- merge(dc[, !c("GnRH"), with = FALSE], dt[, .(ID, date_, GnRH, volume, T)],
  by = c("ID", "date_"),
  all.x = TRUE
)

# exclude dead or injured birds
dc[is.na(dead), dead := 0]
dc <- dc[dead != 1]

dR <- dc[!is.na(T), .(
  species = "REPH", ID, date_, caught_time, bled_time, sex = sex, tarsus, wing,
  weight, testo = T, volume, GnRH, haema
)]

#-------------------------------------------------------------------------------
# PESA data
#-------------------------------------------------------------------------------

# database
con <- dbConnect(RSQLite::SQLite(), paste0(scidb, "PESAatBARROW.sqlite"))
dc <- dbGetQuery(con, "SELECT * FROM [CAPTURES]") |> data.table()
dt <- dbGetQuery(con, "SELECT * FROM [TESTO]") |> data.table()
ds <- dbGetQuery(con, "SELECT * FROM [SEX]") |> data.table()
dbDisconnect(con)

# transform datetimes (are actually AKDT)
dc[is.na(start_capture_date_time), start_capture_date_time := caught_date_time]
dc[, date_ := as.Date(start_capture_date_time)]
dc[, caught_date_time := as.POSIXct(
  caught_date_time,
  format = "%Y-%m-%d %H:%M:%S", tz = "UTC"
)]
# replace incorrect dates (1899-12-30)
dc[
  as.Date(caught_date_time) == "1899-12-30",
  caught_date_time := as.POSIXct(
    paste(
      date_, format(caught_date_time, "%H:%M:%S")
    ),
    format = "%Y-%m-%d %H:%M:%S", tz = "UTC"
  )
]

# all in proper POSIXct format
dc[nchar(bled_date_time) < 19, bled_date_time := NA] # remove when only date
dc[, bled_date_time := as.POSIXct(bled_date_time, tz = "UTC")]

dc[, bled_date_time := as.POSIXct(
  bled_date_time,
  format = "%Y-%m-%d %H:%M:%S", tz = "UTC"
)]
# replace incorrect dates (1899-12-30)
dc[
  as.Date(bled_date_time) == "1899-12-30",
  bled_date_time := as.POSIXct(
    paste(
      date_, format(bled_date_time, "%H:%M:%S")
    ),
    format = "%Y-%m-%d %H:%M:%S", tz = "UTC"
  )
]

# all in proper POSIXct format
dc[, bled_date_time := as.POSIXct(bled_date_time, tz = "UTC")]

# merge with lab sex
ds <- unique(ds, by = "ID")
dc <- merge(dc, ds[, .(ID = as.integer(ID), sex)], by = "ID", all.x = TRUE)
dc[sex == 1, sex_genetic := "M"]
dc[sex == 2, sex_genetic := "F"]

dc[, sex_observed := as.character(sex_observed)]
dc[sex_observed == 1, sex_observed := "M"]
dc[sex_observed == 2, sex_observed := "F"]

# add date
dc[, date_ := as.Date(start_capture_date_time)]
dt[, date_ := as.Date(date_)]

# merge with testo data
dc <- merge(dc, dt[, .(ID, date_, GnRH, volume, T)],
  by = c("ID", "date_"),
  all.x = TRUE
)

# exclude dead or injured birds
dc[is.na(dead), dead := 0]
dc <- dc[dead != 1]

# NA if caught equals bleed time & > 40 min
dc[caught_date_time == bled_date_time, bled_date_time := NA]

# bleeding time
dc[, diff_caught_bled := difftime(
  bled_date_time, caught_date_time,
  units = "mins"
) |> as.numeric()]
dc[diff_caught_bled > 30, bled_date_time := NA] # exclude mistakes in the field

# remove unrealistic measure
dc[ID == 250103853, wing := NA]

dP <- dc[!is.na(T), .(
  species = "PESA", ID, date_, caught_time = caught_date_time,
  bled_time = bled_date_time, sex = sex_genetic, tarsus, wing, weight,
  testo = T, volume, GnRH, haema = hematocrit
)]

#-------------------------------------------------------------------------------
# Merge data and save
#-------------------------------------------------------------------------------

# merge
d <- rbindlist(list(dR, dP))
d[, year_ := year(date_)]
d[, haema := round(as.numeric(haema), 1)]

# order table
setcolorder(d, c(
  "species", "ID", "year_",
  setdiff(names(d), c("species", "ID", "year_"))
))

# check outliers

# tarsus
ggplot(data = d) +
  geom_histogram(aes(tarsus, fill = species))

ggplot(data = d[species == "PESA"]) +
  geom_histogram(aes(tarsus, fill = sex))

ggplot(data = d[species == "REPH"]) +
  geom_histogram(aes(tarsus, fill = sex))

# wing
ggplot(data = d) +
  geom_histogram(aes(wing, fill = species))

ggplot(data = d[species == "PESA"]) +
  geom_histogram(aes(wing, fill = sex))

ggplot(data = d[species == "REPH"]) +
  geom_histogram(aes(wing, fill = sex))

# weight
ggplot(data = d) +
  geom_histogram(aes(weight, fill = species))

ggplot(data = d[species == "PESA"]) +
  geom_histogram(aes(weight, fill = sex))

ggplot(data = d[species == "REPH"]) +
  geom_histogram(aes(weight, fill = sex))

# save data
fwrite(d, "./DATA/REPH_PESA_testo_haema.csv", yaml = TRUE)

