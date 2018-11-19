rm(list = ls())
library(lubridate)
library(readxl)
library(dplyr)
library(tibbletime)
library(weathermetrics)
library(tidyverse)

data <- read_rds("data/meteo_data_premodel.rds")

d2H_vs_press <- ggplot(data)+
  geom_point(aes(press_agh, d2H_krk, color="krk"))+
  geom_point(aes(press_kw, d2H_kw, color="kw"))
d2H_vs_press

# cOOOOOOOOOO XD
