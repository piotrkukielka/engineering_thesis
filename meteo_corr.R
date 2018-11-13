rm(list = ls())
library(lubridate)
#library(readxl)
#library(dplyr)
library(tibbletime)
#library(weathermetrics)
library(tidyverse)
library(ggplot2)

rooftop <- read_rds("data/fullrooftop.rds")
garden <- read_rds("data/garden.rds")
meteokrk <- read_rds("data/meteokrk.rds")

# get rooftop ready
rooftop$date <- as_date(rooftop$date)

# get ready meteokrk
meteokrk$date <- as_date(meteokrk$date)
meteokrk <- sapply(meteokrk, as.numeric, simplify = F)
meteokrk$date <- as_date(meteokrk$date)
meteokrk <- as_tibble(meteokrk)
meteokrk <- meteokrk %>% as_tbl_time(date)
glimpse(meteokrk)
daily_meteo <- meteokrk %>%
  collapse_by("daily") %>%
  group_by(date) %>%
  summarise_all(funs(mean(., na.rm=T)))



# get ready garden
garden <- sapply(garden, gsub, pattern=",", replacement=".", simplify = F)
garden$date <- as_date(garden$date)
garden <- sapply(garden, as.numeric, simplify = F)
garden$date <- as_date(garden$date)
garden <- as_tibble(garden)
garden <- garden %>% as_tbl_time(date)
glimpse(garden)
daily_garden <- garden %>%
  collapse_by("daily") %>%
  group_by(date) %>%
  summarise_all(funs(mean))

head(daily_garden$AIRTEMP, n=40) # trzymane w pomieszczeniu???


daily_cut <- left_join(rooftop, daily_meteo, by="date")
daily_cut <- left_join(daily_cut, daily_garden, by="date")
glimpse(tail(daily_cut))

names(daily_cut)
problem1 <- ggplot(daily_cut, aes(x=date)) +
  geom_line(aes(y = averageAirTemp, color = "averageAirTemp")) +
  geom_line(aes(y = TEMP, color = "TEMP")) +
  geom_line(aes(y = AIRTEMP, color = "AIRTEMP"))

garden$PRESSURE

ggsave("results/problems/problem1.pdf", problem1)

names(daily_cut)


garden2 <- read_rds("data/garden.rds")
garden2 <- sapply(garden2, gsub, pattern=",", replacement=".", simplify = F)
garden2$date <- as_date(garden2$date)
garden2 <- sapply(garden2, as.numeric, simplify = F)
garden2$date <- as_date(garden2$date)
garden2 <- as_tibble(garden2)
garden2 <- garden2 %>% as_tbl_time(date)
glimpse(garden2)
daily_garden2 <- garden2 %>%
  collapse_by("daily") %>%
  group_by(date) %>%
  summarise_all(funs(sum))
glimpse(daily_garden2)

daily_cut2 <- left_join(rooftop, daily_meteo, by="date")
daily_cut2 <- left_join(daily_cut2, daily_garden2, by="date")
names(daily_cut2)

problem2 <- ggplot(daily_cut2, aes(x=date)) +
  geom_line(aes(y = rainAccumulation, color = "rainAccumulation")) +
  geom_line(aes(y = RAIN, color = "RAIN")) 

ggsave("results/problems/problem2.pdf", problem1)