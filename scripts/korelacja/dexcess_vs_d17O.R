rm(list = ls())
library(lubridate)
library(readxl)
library(dplyr)
library(tibbletime)
library(weathermetrics)
library(tidyverse)
library(ggrepel)
library(plotly)

setwd("~/engineering_thesis/")
data <- read_rds("data/isotope_data.rds")

# dexcess against d17O 
# krk pred
#model_d2Hvsd17O_krk <- lm(d2H_krk ~ d17O_krk, data)
## PREDICTION ON KW DATA FOR LENGTH
#predicted_d2Hvsd17O_krk <- predict(model_d2Hvsd17O_krk, 
#                                  newdata = data.frame(d17O_krk = data$d17O_kw))
#
#rsqr_krk <- paste(expression(R^2), "==", 
#              format(summary(model_d2Hvsd17O_krk)[["r.squared"]], digits = 4))
#coeff_a_krk <- format(coef(model_d2Hvsd17O_krk)[["d17O_krk"]], digits = 4)
#coeff_b_krk <- format(coef(model_d2Hvsd17O_krk)[["(Intercept)"]], digits = 4)
#eq_krk <- paste(expression("Kraków: "*delta^2*"H"), 
#            "==",
#            format(coeff_a_krk, digits = 4),
#            "*",
#            expression(delta^17*"O"),
#            "+",
#            format(coeff_b_krk, digits = 4))
## kw pred
#model_d2Hvsd17O_kw <- lm(d2H_kw ~ d17O_kw, data)
#predicted_d2Hvsd17O_kw <- predict(model_d2Hvsd17O_kw, 
#                                  #newdata = data.frame(d17O_kw = data$d17O_kw))
#                                  newdata = data.frame(d17O_kw = seq(-50, 20, length = length(data$d17O_kw))))
#
#rsqr_kw <- paste(expression(R^2), "==", 
#              format(summary(model_d2Hvsd17O_kw)[["r.squared"]], digits = 4))
#coeff_a_kw <- format(coef(model_d2Hvsd17O_kw)[["d17O_kw"]], digits = 4)
#coeff_b_kw <- format(coef(model_d2Hvsd17O_kw)[["(Intercept)"]], digits = 4)
#eq_kw <- paste(expression("Kasprowy Wierch: "*delta^2*"H"), 
#            "==",
#            format(coeff_a_kw, digits = 4),
#            "*",
#            expression(delta^17*"O"),
#            "+",
#            format(coeff_b_kw, digits = 4))
#
## GMWL
#eq_gmwl <- paste(expression("GMWL: "*delta^2*"H"), 
#               "==8*",
#               expression(delta^17*"O"),
#               "+10")
#lin_eq = function(x){8*x+10}
dexcessvsd17O <- ggplot(data) + 
  geom_point(aes(d17O_krk, dexcess_krk, color="red"), size=2, shape=0) +
#  geom_line(aes(
#    d17O_kw, 
#    predicted_dexcessvsd17O_krk, 
#    color="red")) +
  geom_point(aes(d17O_kw, dexcess_kw, color="blue"), size=2, shape=1) +
#  geom_line(aes(
#    seq(-50, 20, length = length(data$d17O_kw)),
#    predicted_dexcessvsd17O_kw,
#    color="blue")) +
#  geom_line(aes(
#    seq(-50, 20, length = length(data$d17O_kw)),
#    lin_eq(seq(-50, 20, length = length(data$d17O_kw))),
#    color="black"),
#    linetype="dashed")+
#  xlim(min(data$d17O_kw, na.rm=T), max(data$d17O_kw, na.rm=T))+
#  ylim(min(data$dexcess_kw, na.rm=T), max(data$dexcess_krk, na.rm=T))+
  xlab(expression(delta^17*"O [\u2030]")) +
  ylab(expression("d-excess [\u2030]")) +
  theme(aspect.ratio=1)+
  scale_color_manual(values=c("steelblue3", "orangered"), 
                     name="", 
                     labels=c("Kasprowy Wierch", "Kraków"))+
  guides(colour = guide_legend(override.aes = list(shape = c(1,0),
                                                   linetype = c(1, 1))))#+
#  annotate("label",
#           label = eq_krk, x=min(data$d17O_kw, na.rm=T), y = 0,
#           hjust = 0, vjust = 0, parse=TRUE)+
#  annotate("label", x=min(data$d17O_kw, na.rm=T), y = 0,
#           label = rsqr_krk,
#           hjust = -3.5, vjust = 0, parse=TRUE)+
#  annotate("label", x=min(data$d17O_kw, na.rm=T), y = -15, 
#           label = eq_kw,
#           hjust = 0, vjust = 0, parse=TRUE)+
#  annotate("label", x=min(data$d17O_kw, na.rm=T), y = -15, 
#           label = rsqr_kw,
#           hjust = -3.5, vjust = 0, parse=TRUE)+
#  annotate("label", x=min(data$d17O_kw, na.rm=T), y = -30, 
#           label = eq_gmwl,
#           hjust = 0, vjust = 0, parse=TRUE)
dexcessvsd17O
ggsave(plot=dexcessvsd17O, filename = "plots/dexcess_vs_d17O.pdf",
       width = 30, height = 30, units = "cm")