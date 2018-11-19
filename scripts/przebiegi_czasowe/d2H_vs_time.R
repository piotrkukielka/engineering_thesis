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
secondstep_krk <- c(data_monthly$d2H_krk, data_monthly$d2H_krk[length(data_monthly$d2H_krk)])
stepdata_krk <- data.frame( x= firststep_krk[-length(firststep_krk)],
                            y=secondstep_krk[-length(firststep_krk)], 
                            xend=firststep_krk[-1], 
                            yend=secondstep_krk[-length(firststep_krk)]) 
firststep_kw <- as.POSIXct(as_date(c(as.character(data_monthly$date), "2018-06-30")))
secondstep_kw <- c(data_monthly$d2H_kw, data_monthly$d2H_kw[length(data_monthly$d2H_kw)])
stepdata_kw <- data.frame( x= firststep_kw[-length(firststep_kw)],
                            y=secondstep_kw[-length(firststep_kw)], 
                            xend=firststep_kw[-1], 
                            yend=secondstep_kw[-length(firststep_kw)]) 

d2Hvstime <-ggplot(data) + 
  geom_line(data = data[!is.na(data$d2H_krk),], 
            aes(date, d2H_krk, color="krk"),
            size=0.1, alpha=0.3) +
  geom_point(aes(date, d2H_krk, color="krk"), na.rm = TRUE) +
  geom_line(data = data[!is.na(data$d2H_kw),], 
            aes(date, d2H_kw, color="kw"),
            size=0.1, alpha=0.3) +
  geom_point(aes(date, d2H_kw, color="kw"), na.rm = TRUE) +
  #geom_step(data=stepdata_krk, aes(x, y, color="krk"), size=1)+
  #geom_step(data=stepdata_kw, aes(x, y, color="kw"), size=1)+
  geom_segment(data=stepdata_krk, aes(x=x, y=y, xend=xend, yend=yend), color="black", size=1.4)+
  geom_segment(data=stepdata_krk, aes(x=x, y=y, xend=xend, yend=yend, color="krk", linetype="krk"), size=1)+
  geom_segment(data=stepdata_kw, aes(x=x, y=y, xend=xend, yend=yend), color="black", size=1.4)+
  geom_segment(data=stepdata_kw, aes(x=x, y=y, xend=xend, yend=yend, color="kw", linetype="kw"), size=1)+
  xlab("Data") +
  ylab(as.expression(expression(delta^2*"H [\u2030]"))) + 
  theme(axis.text.x = element_text(angle = -45, hjust = 0.5, vjust = 0.5)) +
  scale_x_datetime(date_labels = "%d.%m.%Y")+
  theme(aspect.ratio=1)+
  scale_color_manual(values=c(kw = "steelblue3", krk = "orangered"), 
                     name="Dane dzienne", 
                     labels=c(kw = "Kasprowy Wierch", krk = "Kraków"))+
  scale_linetype_manual(values=c(kw=1, krk=1),
                    name="Średnie miesięczne",
                    labels=c(kw = "Kasprowy Wierch", krk = "Kraków"))+
  guides(linetype = guide_legend(override.aes = 
                                list(color=c(krk="orangered", kw="steelblue3"))),
         color = guide_legend(override.aes = 
                                list(linetype = NA)))+
  theme(legend.background = element_rect(fill=NA,
                                 size=0.5, linetype="solid", 
                                 colour ="black"))+
  theme(legend.position = c(0.1, 0.15))
d2Hvstime
ggsave(plot=d2Hvstime, filename = "plots/d2H_vs_time.pdf",
       width = 30, height = 30, units = "cm", dpi = 600, device = cairo_pdf)

# WIP
#d2Hvstime <-ggplot(data) + geom_line(data = data[!is.na(data$d2H_krk),], 
#            aes(date, d2H_krk, color="krk"),
#            size=0.1) +
#  geom_point(aes(date, d2H_krk, color="krk"), na.rm = TRUE) +
#  geom_line(data = data[!is.na(data$d2H_kw),], 
#            aes(date, d2H_kw, color="kw"),
#            size=0.1) +
#  geom_point(aes(date, d2H_kw, color="kw"), na.rm = TRUE) +
#  #geom_step(data=stepdata_krk, aes(x, y, color="krk"), size=1)+
#  #geom_step(data=stepdata_kw, aes(x, y, color="kw"), size=1)+
#  geom_segment(data=stepdata_krk, aes(x=x, y=y, xend=xend, yend=yend, color="krk_msc", linetype="krk"), size=1)+
#  geom_segment(data=stepdata_kw, aes(x=x, y=y, xend=xend, yend=yend, color="kw_msc", linetype="kw"), size=1)+
#  xlab("Data") +
#  ylab(as.expression(expression(delta^2*"H [\u2030]"))) + 
#  theme(axis.text.x = element_text(angle = -45, hjust = 0.5, vjust = 0.5)) +
#  scale_x_datetime(date_labels = "%d.%m.%Y")+
#  theme(aspect.ratio=1)+
#  scale_color_manual(values=c(c(kw = "steelblue3", krk = "orangered", kw_msc = "blue", krk_msc = "red")), 
#                     name="Dane dzienne", 
#                     labels=c("Kasprowy Wierch", "Kraków"))+
#  scale_linetype_manual(values=c(1, 1),
#                    name="Średnie miesięczne",
#                    labels=c("Kasprowy Wierch", "Kraków"))+
#  guides(linetype = guide_legend(override.aes = 
#                                list(color=c("blue", "red"),
#                                     shape=c(1, 1))),
#         color = guide_legend(override.aes =
#                                list(color=c(kw = NA, krk = "orangered", NA, NA),
#                                     fill = c(NA))))