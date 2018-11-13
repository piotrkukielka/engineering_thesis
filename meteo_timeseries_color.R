rm(list = ls())
library(lubridate)
#library(readxl)
#library(dplyr)
#library(tibbletime)
#library(weathermetrics)
library(tidyverse)
library(ggplot2)
# plotly?

datakrk <- read_rds("data/datakrk.rds")
datakw <- read_rds("data/datakw.rds")
rooftop <- read_rds("data/fullrooftop.rds")

all <- full_join(datakrk, rooftop)

tempplot <- ggplot(all %>%
              arrange(!is.na(d2H)),
            aes(date, averageAirTemp, col = is.na(d2H))) +
  scale_colour_manual(name = 'Dane izotopowe', 
                      values = setNames(c('limegreen','black'), c(F, T)),
                      labels = c('dostępne', 'niedostępne')) +
  geom_point()+
  xlab("Data")+
  ylab(expression("Temperatura ["*degree*"C]"))+
  ggtitle("Temperatura") +
  theme(plot.title = element_text(hjust = 0.5))
tempplot
ggsave("results/params/temp_vs_time.pdf", tempplot)

rainplot <- ggplot(all %>%
                     arrange(!is.na(d2H)),
                   aes(date, rainAccumulation, col = is.na(d2H))) +
  scale_colour_manual(name = 'Dane izotopowe', 
                      values = setNames(c('limegreen','black'), c(F, T)),
                      labels = c('dostępne', 'niedostępne')) +
  geom_histogram(stat="identity")+
  xlab("Data")+
  ylab(expression("Ilość opadów [mm]"))+
  ggtitle("Ilość opadów") +
  theme(plot.title = element_text(hjust = 0.5))
rainplot
ggsave("results/params/rain_vs_time.pdf", rainplot)

humplot <- ggplot(all %>%
                     arrange(!is.na(d2H)),
                   aes(date, averageRelativeHumidity, col = is.na(d2H))) +
  scale_colour_manual(name = 'Dane izotopowe', 
                      values = setNames(c('limegreen','black'), c(F, T)),
                      labels = c('dostępne', 'niedostępne')) +
  geom_point()+
  xlab("Data")+
  ylab(expression("Wilgotność względna [%]"))+
  ggtitle("Wilgotność względna") +
  theme(plot.title = element_text(hjust = 0.5))
humplot
ggsave("results/params/humidity_vs_time.pdf", humplot)

windplot <- ggplot(all %>%
                    arrange(!is.na(d2H)),
                  aes(date, averageWindSpeed, col = is.na(d2H))) +
  scale_colour_manual(name = 'Dane izotopowe', 
                      values = setNames(c('limegreen','black'), c(F, T)),
                      labels = c('dostępne', 'niedostępne')) +
  geom_point()+
  xlab("Data")+
  ylab(expression("Prędkość wiatru [m/s]"))+
  ggtitle("Prędkość wiatru") +
  theme(plot.title = element_text(hjust = 0.5))
windplot
ggsave("results/params/wind_vs_time.pdf", windplot)

pressplot <- ggplot(all %>%
                     arrange(!is.na(d2H)),
                   aes(date, averageAirPressure, col = is.na(d2H))) +
  scale_colour_manual(name = 'Dane izotopowe', 
                      values = setNames(c('limegreen','black'), c(F, T)),
                      labels = c('dostępne', 'niedostępne')) +
  geom_point()+
  xlab("Data")+
  ylab(expression("Ciśnienie [hPa]"))+
  ggtitle("Ciśnienie atomsferyczne") +
  theme(plot.title = element_text(hjust = 0.5))
pressplot
ggsave("results/params/pressure_vs_time.pdf", pressplot)

