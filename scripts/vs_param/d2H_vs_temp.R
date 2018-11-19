rm(list = ls())
library(lubridate)
library(readxl)
library(dplyr)
library(tibbletime)
library(weathermetrics)
library(tidyverse)

data <- read_rds("data/meteo_data_premodel.rds")
x_krk <- seq(-50, 50, length = length(data$temp_agh))
x_kw <- seq(-50, 50, length = length(data$temp_agh))
xdata_krk = data.frame(temp_agh = x_krk)
xdata_kw = data.frame(temp_kw = x_kw )
# temporary using agh, change to model later
# model for krk
model_krk <- lm(d2H_krk ~ temp_agh, data)
predicted_krk <- predict(model_krk, xdata_krk)
# model for kw
model_kw <- lm(d2H_kw ~ temp_kw, data)
predicted_kw <- predict(model_kw, xdata_kw) 
                        
d2H_vs_temp <- ggplot(data)+
  geom_point(aes(temp_agh, d2H_krk, color="krk"))+
  geom_point(aes(temp_kw, d2H_kw, color="kw"))+
  geom_line(aes(x_krk, predicted_krk, color="krk"))+
  geom_line(aes(x_kw, predicted_kw, color="kw"))+
  xlim(
    min(pmin(data$temp_agh, data$temp_kw, na.rm=T), na.rm = T), 
    max(pmax(data$temp_agh, data$temp_kw, na.rm=T), na.rm = T))+
  ylim(
    min(pmin(data$d2H_kw, data$d2H_krk, na.rm=T), na.rm = T), 
    max(pmax(data$d2H_kw, data$d2H_krk, na.rm=T), na.rm = T))
d2H_vs_temp

