rm(list = ls())
library(lubridate)
library(readxl)
library(dplyr)
library(tibbletime)
library(weathermetrics)
library(tidyverse)
library(ggrepel)
library(plotly)

data <- read_rds("data/meteo_data_premodel.rds")

temp_plot <- ggplot(data) + 
  geom_point(aes(date, temp_agh, color="red"), size=2, shape=0) +
  geom_point(aes(date, temp_balice, color="blue"), size=2, shape=0) +
  geom_point(aes(date, temp_garden, color="green"), size=2, shape=0) 
temp_plot


# garden vs agh
model_gardenvsagh <- lm(temp_garden ~ temp_agh, data)
predicted_gardenvsagh <- predict(model_gardenvsagh, 
                                  newdata = data.frame(temp_agh = seq(-50, 50, length = length(data$temp_agh))))
rsqr_kw <- paste(expression(R^2), "==", 
              format(summary(model_gardenvsagh)[["r.squared"]], digits = 4))

garden_vs_agh <- ggplot(data)+
  geom_point(aes(temp_agh, temp_garden, color="red"), size=2, shape=0)+
  geom_line(aes(
    seq(-50, 50, length = length(data$temp_agh)),
    predicted_gardenvsagh, 
    color="red"))+
  xlim(min(data$temp_agh, na.rm=T), max(data$temp_agh, na.rm=T))+
  ylim(min(data$temp_garden, na.rm=T), max(data$temp_garden, na.rm=T))+
  # tu jest do poprawy, bo ma byc min z iloczynu zbiorow
  theme(aspect.ratio=1)
garden_vs_agh

data$modeled_from_agh <- predict(model_gardenvsagh, data)
modelagh_plot <- ggplot(data) + 
  geom_point(aes(date, temp_agh, color="agh"), size=2, shape=0) +
  geom_point(aes(date, modeled_from_agh, color="model"), size=2, shape=0)+
  geom_point(aes(date, temp_garden, color="garden"), size=2, shape=0)
modelagh_plot


# garden vs balice
model_gardenvsbalice <- lm(temp_garden ~ temp_balice, data)
predicted_gardenvsbalice <- predict(model_gardenvsbalice, 
                                  newdata = data.frame(temp_balice = seq(-50, 50, length = length(data$temp_balice))))
rsqr_kw <- paste(expression(R^2), "==", 
              format(summary(model_gardenvsbalice)[["r.squared"]], digits = 4))

garden_vs_balice <- ggplot(data)+
  geom_point(aes(temp_balice, temp_garden, color="red"), size=2, shape=0)+
  geom_line(aes(
    seq(-50, 50, length = length(data$temp_balice)),
    predicted_gardenvsbalice, 
    color="red"))+
  xlim(min(data$temp_balice, na.rm=T), max(data$temp_balice, na.rm=T))+
  ylim(min(data$temp_garden, na.rm=T), max(data$temp_garden, na.rm=T))+
  # tu jest do poprawy, bo ma byc min z iloczynu zbiorow
  theme(aspect.ratio=1)
garden_vs_balice

data$modeled_from_balice <- predict(model_gardenvsbalice, data)
modelbalice_plot <- ggplot(data) + 
  geom_point(aes(date, temp_balice, color="balice"), size=2, shape=0) +
  geom_point(aes(date, modeled_from_balice, color="model"), size=2, shape=0)+
  geom_point(aes(date, temp_garden, color="garden"), size=2, shape=0)
modelbalice_plot


