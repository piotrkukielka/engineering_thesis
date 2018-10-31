rm(list = ls())
library(rJava)
library(lubridate)
library(readxl)
library(dplyr)
library(tibbletime)
library(weathermetrics)
library(tidyverse)
# load data files from "data" folder
isotopes_KW = as_tibble(read_excel("data/data_isotopes.xlsx", sheet = 1))
isotopes_Krk = as_tibble(read_excel("data/data_isotopes.xlsx", sheet = 2))
agh_rooftop_meteo = as_tibble(read.csv("data/data_meteo_rooftop.csv", sep = ";"))
garden_meteo = as_tibble(read.csv("data/export_csv_AGH_1540406463.csv", sep = ";"))
meteo_KW = as_tibble(read.delim("data/3674967762051dat.txt", sep = ""))
meteo_Krk = as_tibble(read.delim("data/5857687762569dat.txt", sep = ""))

# unificate names of date columns
names(garden_meteo)[1] <- "date"
names(agh_rooftop_meteo)[1] <- "date"
names(meteo_KW)[3] <- "date"
names(meteo_Krk)[3] <- "date"

# clear dates in isotopes_Krk
tmp1 <- suppressWarnings(as_date(as.numeric(as.character(isotopes_Krk[["date"]])), origin = "1899-12-30"))
tmp2 <- as_date(isotopes_Krk[["date"]], format="%d.%m.%Y", tz="UTC")
tmp1[is.na(tmp1)] <- tmp2[!is.na(tmp2)]
isotopes_Krk[['date']] <- tmp1
rm(tmp1, tmp2)

# clean dates in both meteo
meteo_KW$date <- as_datetime(as.character(meteo_KW$date), format="%Y%m%d%H%M", tz="UTC")
meteo_Krk$date <- as_datetime(as.character(meteo_Krk$date), format="%Y%m%d%H%M", tz="UTC")

# convert other data columns to POSIX
isotopes_Krk$date <- as.POSIXct(isotopes_Krk$date)
isotopes_KW$date <- as.POSIXct(isotopes_KW$date)
agh_rooftop_meteo$date <- as.POSIXct(agh_rooftop_meteo$date, tz = "UTC")
garden_meteo$date <- as.POSIXct(garden_meteo$date, tz = "UTC")
meteo_KW$date <- as.POSIXct(meteo_KW$date)
meteo_Krk$date <- as.POSIXct(meteo_Krk$date)

# fargenheit to celsius in meteo data
meteo_Krk[["TEMP"]] <- fahrenheit.to.celsius(meteo_Krk[["TEMP"]])
meteo_KW[["TEMP"]] <- fahrenheit.to.celsius(meteo_KW[["TEMP"]])

# add date_join columns
isotopes_Krk$date_join <- isotopes_Krk$date %>% as_date()
isotopes_KW$date_join <- isotopes_KW$date %>% as_date()
agh_rooftop_meteo$date_join <- agh_rooftop_meteo$date %>% as_date()
garden_meteo$date_join <- garden_meteo$date %>% as_date()
meteo_KW$date_join <- meteo_KW$date %>% as_date()
meteo_Krk$date_join <- meteo_Krk$date %>% as_date()

meteo_Krk <- meteo_Krk %>% add_column("date_join")

# daily mean for meteo temp data
meteo_Krk <- meteo_Krk %>% as_tbl_time(date)
meteo_Krk_daily <- meteo_Krk %>%
  collapse_by("daily") %>%
  group_by(date) %>%
  summarise(daily_mean_temp = mean(TEMP))
meteo_KW <- meteo_KW %>% as_tbl_time(date)
meteo_KW_daily <- meteo_KW %>%
  collapse_by("daily") %>%
  group_by(date) %>%
  summarise(daily_mean_temp = mean(TEMP))
agh_rooftop_meteo <- agh_rooftop_meteo %>% as_tbl_time(date)
agh_rooftop_meteo_daily <- agh_rooftop_meteo %>%
  collapse_by("daily") %>%
  group_by(date) %>%
  summarise(daily_mean_temp = mean(averageAirTemp))
# commas to dots
garden_meteo[["AIRTEMP"]] <- as.numeric(gsub(",", ".", garden_meteo[["AIRTEMP"]]))
garden_meteo <- garden_meteo %>% as_tbl_time(date)
garden_meteo_daily <- garden_meteo %>%
  collapse_by("daily") %>%
  group_by(date) %>%
  summarise(daily_mean_temp = mean(as.numeric(as.character(AIRTEMP)), na.rm = TRUE))
garden_meteo_daily


# to jest potrzebne? tak
meteo_Krk_daily$date_join <- meteo_Krk_daily$date %>% as_date()
meteo_KW_daily$date_join <- meteo_KW_daily$date %>% as_date()
agh_rooftop_meteo_daily$date_join <- agh_rooftop_meteo_daily$date %>% as_date()
garden_meteo_daily$date_join <- garden_meteo_daily$date %>% as_date()

### create master-file with isotope data+BIG DELTA
### tempofair, amountofrain, relativehumidity->+notrelative (steam)
### airvelocity, atmospheric pressure
# create main data tables
data_krk <- isotopes_Krk
data_kw <- isotopes_KW

# add temperature 
tmpkrk <- left_join(isotopes_Krk, meteo_Krk_daily, by = "date_join")
data_krk <- data_krk %>% add_column("temp [C] <balice>"= tmpkrk$daily_mean_temp)
glimpse(data_krk)

tmpkw <- left_join(isotopes_KW, meteo_KW_daily, by = "date_join")
data_kw <- data_kw %>% add_column("temp [C] <kasprowy>"= tmpkw$daily_mean_temp)
glimpse(data_kw)

tmpkrk <- left_join(isotopes_Krk, agh_rooftop_meteo_daily, by = "date_join")
data_krk <- data_krk %>% add_column("temp [C] <dach>"= tmpkrk$daily_mean_temp)
glimpse(data_krk)

# wstawia NA, bo nie zaczyna pomiarow razem z izotopami
tmpkrk <- left_join(isotopes_Krk, garden_meteo_daily, by = "date_join")
data_krk <- data_krk %>% add_column("temp [C] <ogrod>"= tmpkrk$daily_mean_temp)
glimpse(data_krk)
### maybe add temp from more sources?

# daily mean for rani amount data
# both meteo are not working correctly
agh_rooftop_meteo_daily <- agh_rooftop_meteo %>%
  collapse_by("daily") %>%
  group_by(date) %>%
  summarise(daily_mean_temp = mean(rainAccumulation))
# commas to dots
garden_meteo[["RAIN"]] <- as.numeric(gsub(",", ".", garden_meteo[["RAIN"]]))
garden_meteo_daily <- garden_meteo %>%
  collapse_by("daily") %>%
  group_by(date) %>%
  summarise(daily_mean_temp = mean(as.numeric(as.character(RAIN)), na.rm = TRUE))
garden_meteo_daily

# tak, to jest bardzo potrzebne
agh_rooftop_meteo_daily$date_join <- agh_rooftop_meteo_daily$date %>% as_date()
garden_meteo_daily$date_join <- garden_meteo_daily$date %>% as_date()

### add rain data to main tables
tmpkrk <- left_join(isotopes_Krk, agh_rooftop_meteo_daily, by = "date_join")
data_krk <- data_krk %>% add_column("rain [??] <dach>"= tmpkrk$daily_mean_temp)
glimpse(data_krk)

# wstawia NA, bo nie zaczyna pomiarow razem z izotopami
tmpkrk <- left_join(isotopes_Krk, garden_meteo_daily, by = "date_join")
data_krk <- data_krk %>% add_column("rain [??] <ogrod>"= tmpkrk$daily_mean_temp)
glimpse(data_krk)


### add relative humidity (is it relative?)
agh_rooftop_meteo_daily <- agh_rooftop_meteo %>%
  collapse_by("daily") %>%
  group_by(date) %>%
  summarise(daily_mean_temp = mean(averageRelativeHumidity))
garden_meteo[["AIRHUM"]] <- as.numeric(gsub(",", ".", garden_meteo[["AIRTEMP"]]))
garden_meteo <- garden_meteo %>%
  collapse_by("daily") %>%
  group_by(date) %>%
  summarise(daily_mean_temp = mean(AIRHUM))
names(meteo_Krk)
# no hum in other data sets?












### NOT WORKING !!!! ###
#add amount of rain
#names(meteo_Krk)
#meteo_Krk$test1 <- meteo_Krk$PCPXX %>% as.character() %>% as.numeric()
#meteo_Krk$test2 <- meteo_Krk$PCP06 %>% as.character() %>% as.numeric()
#
##test_meteo_Krk_daily <- 
#tmp <- meteo_Krk %>%
#  collapse_by("daily") %>%
#  group_by(date) %>%
#  summarise(daily_mean_temp = sum(test1, na.rm = TRUE))
#sum(tmp$daily_mean_temp)
#
#
#tmp2 <- meteo_Krk %>%
#  collapse_by("daily") %>%
#  group_by(date) %>%
#  summarise(daily_mean_temp = mean(test2, na.rm = TRUE))
#sum(tmp2$daily_mean_temp, na.rm = TRUE)
#
#
#sum(is.na(meteo_Krk$test1))
#sum(is.na(meteo_Krk$test2))
#meteo_Krk$test1
#meteo_Krk$test2
### SPRAWDZIC DATY, MOZE TRZEBA PRZEKONWERTOWAC STREFY CZY COS!!!!!!!!
### SPRAAWDZIC W JAKIEJ STREFIE W OGOLE JEST TEN CZAS PODANY