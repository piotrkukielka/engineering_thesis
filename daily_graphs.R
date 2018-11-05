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

plot_isotope <- function(isotope){
  massnum <- parse_number(isotope)
  isoname <- strsplit(isotope, split = "[0-9]+")
  isoname <- isoname[[1]][[2]]
  
  ylabel <- bquote(Delta^.(massnum)*.(isoname))
  
  isotope <- sym(isotope)
  uisotope <- paste("u(", isotope, ")", sep="")
  uisotope <- sym(uisotope)
  ggplot(datakrk, aes(date, (!!isotope) )) + 
    geom_line() +
    geom_point() +
    geom_errorbar(aes(ymin = (!!isotope) - (!!uisotope), ymax = (!!isotope) + (!!uisotope) )) +
    xlab("data") +
    ylab(as.expression(ylabel)) + 
    theme(axis.text.x = element_text(angle = -45, hjust = 0.5, vjust = 0.5)) +
    scale_x_datetime(date_labels = "%d.%m.%Y")
}

#errorbars smaller than dots
isotopes <- c("d2H", "d18O", "d17O")
isotopes_timeseries <- lapply(isotopes, plot_isotope)
isotopes_timeseries[1]
