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
  
  ylabel <- bquote(delta^.(massnum)*.(isoname))
  
  isotope <- sym(isotope)
  uisotope <- paste("u(", isotope, ")", sep="")
  uisotope <- sym(uisotope)
  q <-ggplot(datakrk, aes(date, (!!isotope) )) + 
    geom_line() +
    geom_point() +
    geom_errorbar(aes(ymin = (!!isotope) - (!!uisotope), ymax = (!!isotope) + (!!uisotope) )) +
    xlab("data") +
    ylab(as.expression(ylabel)) + 
    theme(axis.text.x = element_text(angle = -45, hjust = 0.5, vjust = 0.5)) +
    scale_x_datetime(date_labels = "%d.%m.%Y")
  return(q)
}

# errorbars smaller than dots
isotopes <- c("d2H", "d18O", "d17O")
isotopes_timeseries <- lapply(isotopes, plot_isotope)
isotopes_timeseries[1]
class(isotopes_timeseries[1])
# poprawic to

ggsave("results/time/d2H_vs_time.pdf", plot_isotope("d2H"))
ggsave("results/time/d18O_vs_time.pdf", plot_isotope("d18O"))
ggsave("results/time/d17O_vs_time.pdf", plot_isotope("d17O"))

dexcessplot <- ggplot(datakrk, aes(date, dexcess )) + 
  geom_line() +
  geom_point() +
  xlab("data") +
  ylab("d-excess") + 
  theme(axis.text.x = element_text(angle = -45, hjust = 0.5, vjust = 0.5)) +
  scale_x_datetime(date_labels = "%d.%m.%Y")

ggsave("results/time/dexcess_vs_time.pdf", dexcessplot)

delta17oplot <- ggplot(datakrk, aes(date, DELTA17O )) + 
  geom_line() +
  geom_point() +
  xlab("data") +
  ylab(expression(Delta^17*"O")) + 
  theme(axis.text.x = element_text(angle = -45, hjust = 0.5, vjust = 0.5)) +
  scale_x_datetime(date_labels = "%d.%m.%Y")

ggsave("results/time/DELTA17O_vs_time.pdf", delta17oplot)

########################### test
testplot <- ggplot(datakrk, aes(date, d2H )) + 
  geom_line() +
  geom_point() +
  xlab("data") +
  ylab("d2H") + 
  theme(axis.text.x = element_text(angle = -45, hjust = 0.5, vjust = 0.5)) +
  scale_x_datetime(date_labels = "%d.%m.%Y") +
  geom_hline(yintercept = mean(datakrk$d2H), color='red') + 
  geom_hline(yintercept = mean(datakrk$d2H)+sd(datakrk$d2H), linetype="dashed", color = "blue")+
  geom_hline(yintercept = mean(datakrk$d2H)-sd(datakrk$d2H), linetype="dashed", color = "blue")
testplot
ggsave("results/test.pdf", testplot)

mean(datakrk$d2H)
sd(datakrk$d2H)



# analogicznie zrobic dla wszystkich meteo