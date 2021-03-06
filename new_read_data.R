rm(list = ls())
library(lubridate)
library(readxl)
library(dplyr)
library(tibbletime)
library(weathermetrics)
library(tidyverse)
setwd("~/engineering_thesis/")

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

# clear dates in both meteo
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

# agh_meteo
agh_meteo_daily <- agh_meteo[,c("date", 
                                "averageAirTemp",
                                "averageDewPointTemperature",
                                "averageRelativeHumidity",
                                "averageAirPressure",
                                "rainAccumulation")]
colnames(agh_meteo_daily) <- c("date",
                               "temp_agh",
                               "dewp_agh",
                               "hum_agh",
                               "press_agh",
                               "pcp_agh")

### add d-excess
isotopes_Krk$dexcess <- isotopes_Krk$d2H - 8*isotopes_Krk$d18O
isotopes_KW$dexcess <- isotopes_KW$d2H - 8*isotopes_KW$d18O

### add Delta 17O
lambda <- 0.528
isotopes_Krk$DELTA17O <- (log(isotopes_Krk$d17O/1000+1) -  lambda*log(isotopes_Krk$d18O/1000+1))*10^6
isotopes_KW$DELTA17O <- (log(isotopes_KW$d17O/1000+1) -  lambda*log(isotopes_KW$d18O/1000+1))*10^6

### add u(Delta17O)
isotopes_Krk[["u(DELTA17O)"]] <- sqrt((-lambda/(isotopes_Krk$d18O/1000+1)*isotopes_Krk[["u(d18O)"]]/1000)^2 +
                                        (1/(isotopes_Krk$d17O/1000+1)*isotopes_Krk[["u(d17O)"]]/1000)^2)*10^6
isotopes_KW[["u(DELTA17O)"]] <- sqrt((lambda/(isotopes_KW$d18O/1000+1)*isotopes_KW[["u(d18O)"]]/1000)^2 +
                                        (1/(isotopes_KW$d17O/1000+1)*isotopes_KW[["u(d17O)"]]/1000)^2)*10^6
# chwilowe!!!!
isotopes_Krk$`u(DELTA17O)`[is.na(isotopes_Krk$`u(DELTA17O)`)]


### add dprim18O
isotopes_Krk$dprim18O <- log(isotopes_Krk$d18O/1000+1)*1000
isotopes_KW$dprim18O <- log(isotopes_KW$d18O/1000+1)*1000

### add dprim17O
isotopes_Krk$dprim17O <- log(isotopes_Krk$d17O/1000+1)*1000
isotopes_KW$dprim17O <- log(isotopes_KW$d17O/1000+1)*1000

# add date_join columns
isotopes_Krk$date_join <- isotopes_Krk$date %>% as_date()
isotopes_KW$date_join <- isotopes_KW$date %>% as_date()
agh_meteo_daily$date_join <- agh_meteo_daily$date %>% as_date()
garden_meteo_daily$date_join <- garden_meteo_daily$date %>% as_date()
kw_meteo_daily$date_join <- kw_meteo_daily$date %>% as_date()
balice_meteo_daily$date_join <- balice_meteo_daily$date %>% as_date()

# remove false data from garden_meteo_daily
del <- which(garden_meteo_daily$temp_garden>20 & garden_meteo_daily$date<as_date("2018-01-01"))
garden_meteo_daily[del, c("temp_garden", "dewp_garden", "hum_garden", "pcp_garden", "press_garden")] <- NA
### joining
data <- merge(isotopes_KW, isotopes_Krk, all.x = T, all.y = T,
                  by=c("date" = "date"), suffix=c("_kw", "_krk"))
data <- as_tbl_time(data, date)
glimpse(data)
saveRDS(data, "data/isotope_data.rds")

data$date_join <- data$date %>% as_date()
data_meteo <- merge(data, agh_meteo_daily, all.x = T, all.y = T,
                  by=c("date_join" = "date_join"))
data_meteo <- merge(data_meteo, balice_meteo_daily, all.x = T, all.y = T,
                    by=c("date_join" = "date_join"))
data_meteo <- merge(data_meteo, kw_meteo_daily, all.x = T, all.y = T,
                    by=c("date_join" = "date_join"))
data_meteo <- merge(data_meteo, garden_meteo_daily, all.x = T, all.y = T,
                    by=c("date_join" = "date_join"))
### UNSURE !!!!!!!!!

drops <- c("date.x","date.y")
data_meteo <- data_meteo[ , !(names(data_meteo) %in% drops)]
names(data_meteo)
data_meteo[["date"]] <- data_meteo$date_join
data_meteo <- as_tbl_time(data_meteo, date)

saveRDS(data_meteo, "data/meteo_data_premodel.rds")

### aggreggating to monthly
glimpse(data)
data_monthly <- data %>%
  collapse_by("monthly") %>%
  #group_by(date) %>%
  group_by(date=floor_date(date, "month")) %>%
  summarise_all(mean, na.rm=T)
glimpse(data_monthly)

saveRDS(data_monthly, "data/data_monthly.rds")

