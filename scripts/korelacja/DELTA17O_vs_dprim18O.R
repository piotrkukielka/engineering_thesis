rm(list = ls())
library(lubridate)
library(readxl)
library(dplyr)
library(tibbletime)
library(weathermetrics)
library(tidyverse)
library(ggrepel)
library(plotly)

theme_set(theme_bw())

setwd("~/engineering_thesis/")
data <- read_rds("data/isotope_data.rds")

DELTA17Ovsdprim18O <- ggplot(data) + 
  geom_point(aes(dprim18O_krk, DELTA17O_krk, color="red"), size=2, shape=0) +
  geom_point(aes(dprim18O_kw, DELTA17O_kw, color="blue"), size=2, shape=1) +
  #geom_errorbar(aes(dprim18O_krk, ymin = DELTA17O_krk-`u(DELTA17O)_krk`, ymax = DELTA17O_krk+`u(DELTA17O)_krk`))
  #geom_errorbar(aes(dprim18O_kw, ymin = DELTA17O_kw-`u(DELTA17O)_kw`, ymax = DELTA17O_kw+`u(DELTA17O)_kw`))+
  # limity niszcza errory duze
  xlim(
    min(pmin(data$dprim18O_kw, data$dprim18O_krk, na.rm=T)), 
    max(pmax(data$dprim18O_kw, data$dprim18O_krk, na.rm=T)))+
  ylim(
    min(pmin(data$DELTA17O_kw, data$DELTA17O_krk, na.rm=T)), 
    max(pmax(data$DELTA17O_kw, data$DELTA17O_krk, na.rm=T)))+ 
  xlab(expression(delta^18*"'O [\u2030]")) +
  ylab(expression(Delta^17*"O [per mag]")) +
  theme(aspect.ratio=1)+
  scale_color_manual(values=c("steelblue3", "orangered"), 
                     name="", 
                     labels=c("Kasprowy Wierch", "Kraków"))+
  guides(colour = guide_legend(override.aes = list(shape = c(1,0),
                                                   linetype = c(1, 1))))+
  theme(legend.background = element_rect(fill=NA,
                                 size=0.5, linetype="solid", 
                                 colour ="black"))+
  theme(legend.title = element_blank(), 
        legend.position = c(0.1, 0.95))
DELTA17Ovsdprim18O
ggsave(plot=DELTA17Ovsdprim18O, filename = "plots/DELTA17O_vs_dprim18O.pdf",
       width = 30, height = 30, units = "cm", device = cairo_pdf)
