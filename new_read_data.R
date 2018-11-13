rm(list = ls())
library(lubridate)
library(readxl)
library(dplyr)
library(tibbletime)
library(weathermetrics)
library(tidyverse)
# load data files from "data" folder
isotopes_KW = as_tibble(read_excel("data/data_isotopes.xlsx", sheet = 1))
isotopes_Krk = as_tibble(read_excel("data/data_isotopes.xlsx", sheet = 2))
agh_meteo = as_tibble(read.csv("data/data_meteo_rooftop.csv", sep = ";"))
garden_meteo = as_tibble(read.csv("data/export_csv_AGH_1540406463.csv", sep = ";"))
kw_meteo = as_tibble(read.delim("data/3674967762051dat.txt", sep = ""))
balice_meteo = as_tibble(read.delim("data/5857687762569dat.txt", sep = ""))

# unificate names of date columns
names(garden_meteo)[1] <- "date"
names(agh_meteo)[1] <- "date"
names(kw_meteo)[3] <- "date"
names(balice_meteo)[3] <- "date"

# clear dates in isotopes_Krk
tmp1 <- suppressWarnings(as_date(as.numeric(as.character(isotopes_Krk[["date"]])), origin = "1899-12-30"))
tmp2 <- as_date(isotopes_Krk[["date"]], format="%d.%m.%Y", tz="Europe/Warsaw")
tmp1[is.na(tmp1)] <- tmp2[!is.na(tmp2)]
isotopes_Krk[['date']] <- tmp1
rm(tmp1, tmp2)

# clean dates in both meteo
kw_meteo$date <- as_datetime(as.character(kw_meteo$date), format="%Y%m%d%H%M", tz="GMT")
balice_meteo$date <- as_datetime(as.character(balice_meteo$date), format="%Y%m%d%H%M", tz="GMT")
kw_meteo$date <- with_tz(kw_meteo$date, tzone = "UTC")
balice_meteo$date <- with_tz(balice_meteo$date, tzone = "UTC")

# fahrenheit to celsius in meteo data
balice_meteo[["TEMP"]] <- fahrenheit.to.celsius(balice_meteo[["TEMP"]])
kw_meteo[["TEMP"]] <- fahrenheit.to.celsius(kw_meteo[["TEMP"]])
balice_meteo[["DEWP"]] <- as.numeric(as.character(balice_meteo$DEWP))
kw_meteo[["DEWP"]] <- as.numeric(as.character(kw_meteo$DEWP))
balice_meteo[["DEWP"]] <- fahrenheit.to.celsius(balice_meteo[["DEWP"]])
kw_meteo[["DEWP"]] <- fahrenheit.to.celsius(kw_meteo[["DEWP"]])


# convert other data columns to POSIX
isotopes_Krk$date <- as.POSIXct(isotopes_Krk$date)
isotopes_KW$date <- as.POSIXct(isotopes_KW$date)
agh_meteo$date <- as.POSIXct(agh_meteo$date, tz = "UTC")
garden_meteo$date <- as.POSIXct(garden_meteo$date, tz = "UTC")
kw_meteo$date <- as.POSIXct(kw_meteo$date)
balice_meteo$date <- as.POSIXct(balice_meteo$date)

### AGREGGATING TO DAILY DATA
# add humidity
balice_meteo$HUM <- dewpoint.to.humidity(balice_meteo$DEWP, balice_meteo$TEMP)
kw_meteo$HUM <- dewpoint.to.humidity(kw_meteo$DEWP, kw_meteo$TEMP)

# balice_meteo
balice_meteo <- balice_meteo %>% as_tbl_time(date)
glimpse(balice_meteo)
balice_meteo_daily <- balice_meteo %>%
  collapse_by("daily") %>%
  group_by(date) %>%
  summarise(
    temp_balice = mean(TEMP, na.rm=T), 
    dewp_balice = mean(as.numeric(as.character(DEWP)), na.rm=T),
    hum_balice = mean(HUM, na.rm=T),
    pcp_balice = sum(as.numeric(as.character(PCP06)), na.rm=T),
    press_balice = mean(as.numeric(as.character(STP)), na.rm=T)
    )
glimpse(balice_meteo_daily)

# kw_meteo
kw_meteo <- kw_meteo %>% as_tbl_time(date)
glimpse(kw_meteo)
kw_meteo_daily <- kw_meteo %>%
  collapse_by("daily") %>%
  group_by(date) %>%
  summarise(
    temp_kw = mean(TEMP, na.rm=T), 
    dewp_kw = mean(as.numeric(as.character(DEWP)), na.rm=T),
    hum_kw = mean(HUM, na.rm=T),
    pcp_kw = sum(as.numeric(as.character(PCP06)), na.rm=T),
    press_kw = mean(as.numeric(as.character(STP)), na.rm=T)
    )
glimpse(kw_meteo_daily)

# garden_meteo
garden_meteo <- garden_meteo %>% as_tbl_time(date)
glimpse(garden_meteo)
garden_meteo_daily <- garden_meteo %>%
  collapse_by("daily") %>%
  group_by(date) %>%
  summarise(
    temp_garden = mean(as.numeric(as.character(AIRTEMP)), na.rm=T), 
    dewp_garden = mean(as.numeric(as.character(DEWPOINT)), na.rm=T),
    hum_garden = mean(as.numeric(as.character(AIRHUM)), na.rm=T),
    pcp_garden = sum(as.numeric(as.character(RAIN)), na.rm=T),
    press_garden = mean(as.numeric(as.character(PRESSURE)), na.rm=T)
    )
glimpse(garden_meteo_daily)
glimpse(garden_meteo)

### joining
data <- full_join(isotopes_Krk, isotopes_KW, by="date", suffix=c("_krk", "_kw"))
glimpse(data)
saveRDS(data, "data/isotope_data.rds")