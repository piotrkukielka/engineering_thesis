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

data <- read_rds("data/isotope_data.rds")
data_monthly <- read_rds("data/data_monthly.rds")


firststep_krk <- as.POSIXct(as_date(c(as.character(data_monthly$date), "2018-06-30")))
secondstep_krk <- c(data_monthly$d18O_krk, data_monthly$d18O_krk[length(data_monthly$d18O_krk)])
stepdata_krk <- data.frame( x= firststep_krk[-length(firststep_krk)],
                            y=secondstep_krk[-length(firststep_krk)], 
                            xend=firststep_krk[-1], 
                            yend=secondstep_krk[-length(firststep_krk)]) 
firststep_kw <- as.POSIXct(as_date(c(as.character(data_monthly$date), "2018-06-30")))
secondstep_kw <- c(data_monthly$d18O_kw, data_monthly$d18O_kw[length(data_monthly$d18O_kw)])
stepdata_kw <- data.frame( x= firststep_kw[-length(firststep_kw)],
                            y=secondstep_kw[-length(firststep_kw)], 
                            xend=firststep_kw[-1], 
                            yend=secondstep_kw[-length(firststep_kw)]) 

d18Ovstime <-ggplot(data) + 
  geom_line(data = data[!is.na(data$d18O_krk),], 
            aes(date, d18O_krk, color="krk"),
            size=0.1) +
  geom_point(aes(date, d18O_krk, color="krk"), na.rm = TRUE) +
  geom_line(data = data[!is.na(data$d18O_kw),], 
            aes(date, d18O_kw, color="kw"),
            size=0.1) +
  geom_point(aes(date, d18O_kw, color="kw"), na.rm = TRUE) +
  geom_segment(data=stepdata_krk, aes(x=x, y=y, xend=xend, yend=yend), color="black", size=1.4)+
  geom_segment(data=stepdata_krk, aes(x=x, y=y, xend=xend, yend=yend, color="krk", linetype="krk"), size=1)+
  geom_segment(data=stepdata_kw, aes(x=x, y=y, xend=xend, yend=yend), color="black", size=1.4)+
  geom_segment(data=stepdata_kw, aes(x=x, y=y, xend=xend, yend=yend, color="kw", linetype="kw"), size=1)+
  xlab("Data") +
  ylab(as.expression(expression(delta^18*"O [\u2030]"))) + 
  theme(axis.text.x = element_text(angle = -45, hjust = 0.5, vjust = 0.5)) +
  scale_x_datetime(date_labels = "%d.%m.%Y")+
  theme(aspect.ratio=1)+
  scale_color_manual(values=c(kw = "steelblue3", krk = "orangered"), 
                     name="Dane dzienne", 
                     labels=c(kw = "Kasprowy Wierch", krk = "Kraków"))+
  scale_linetype_manual(values=c(1, 1),
                    name="Średnie miesięczne",
                    labels=c(kw="Kasprowy Wierch", krk="Kraków"))+
  guides(linetype = guide_legend(override.aes = 
                                list(color=c(krk="orangered", kw="steelblue3"))),
         color = guide_legend(override.aes = 
                                list(linetype = NA)))+
  theme(legend.background = element_rect(fill=NA,
                                 size=0.5, linetype="solid", 
                                 colour ="black"))+
  theme(legend.position = c(0.1, 0.15))
d18Ovstime
ggsave(plot=d18Ovstime, filename = "plots/d18O_vs_time.pdf",
       width = 30, height = 30, units = "cm", dpi = 600, device = cairo_pdf)