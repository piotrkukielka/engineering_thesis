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

plot_meteo <- function(meteo){
  #ylabel <- bquote(Delta^.(massnum)*.(isoname))
  
  meteo <- sym(meteo)
  ggplot(datakrk, aes(date, (!!meteo) )) + 
    geom_line() +
    geom_point() +
    #xlab("data") +
    #ylab(as.expression(ylabel)) + 
    theme(axis.text.x = element_text(angle = -45, hjust = 0.5, vjust = 0.5)) +
    scale_x_datetime(date_labels = "%d.%m.%Y")
}

### Krakow
# temperature
temp_balice <- plot_meteo("temp [C] <balice>")
temp_dach <- plot_meteo("temp [C] <dach>")
temp_ogrod <- plot_meteo("temp [C] <ogrod>") # trzy pierwsze wartosci kompletnie oderwane od rzeczywistosci 

# rain
rain_dach <- plot_meteo("rain [??] <dach>")
rain_ogrod <- plot_meteo("rain [??] <ogrod>")

# humidity

datakrk$`temp [C] <ogrod>`


# analogicznie zrobic dla wszystkich meteo
# wykresy dla meteo maja byc tylko z tych dni dla ktorych mamy dane izo?