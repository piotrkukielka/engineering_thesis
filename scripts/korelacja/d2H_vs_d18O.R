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

# d2H against d18O 
# krk pred
model_d2Hvsd18O_krk <- lm(d2H_krk ~ d18O_krk, data)
# PREDICTION ON KW DATA FOR LENGTH

predicted_d2Hvsd18O_krk <- predict(model_d2Hvsd18O_krk, 
                                  #newdata = data.frame(d18O_kw = data$d18O_kw))
                                  newdata = data.frame(d18O_krk = seq(-50, 20, length = length(data$d18O_krk))))

rsqr_krk <- paste(expression(R^2), "==", 
              format(summary(model_d2Hvsd18O_krk)[["r.squared"]], digits = 4))
coeff_a_krk <- format(coef(model_d2Hvsd18O_krk)[["d18O_krk"]], digits = 4)
coeff_b_krk <- format(coef(model_d2Hvsd18O_krk)[["(Intercept)"]], digits = 4)
eq_krk <- paste(expression("Kraków: "*delta^2*"H"), 
            "==",
            format(coeff_a_krk, digits = 4),
            "*",
            expression(delta^18*"O"),
            "+",
            format(coeff_b_krk, digits = 4))
# kw pred
model_d2Hvsd18O_kw <- lm(d2H_kw ~ d18O_kw, data)
predicted_d2Hvsd18O_kw <- predict(model_d2Hvsd18O_kw, 
                                  #newdata = data.frame(d18O_kw = data$d18O_kw))
                                  newdata = data.frame(d18O_kw = seq(-50, 20, length = length(data$d18O_kw))))

rsqr_kw <- paste(expression(R^2), "==", 
              format(summary(model_d2Hvsd18O_kw)[["r.squared"]], digits = 4))
coeff_a_kw <- format(coef(model_d2Hvsd18O_kw)[["d18O_kw"]], digits = 4)
coeff_b_kw <- format(coef(model_d2Hvsd18O_kw)[["(Intercept)"]], digits = 4)
eq_kw <- paste(expression("Kasprowy Wierch: "*delta^2*"H"), 
            "==",
            format(coeff_a_kw, digits = 4),
            "*",
            expression(delta^18*"O"),
            "+",
            format(coeff_b_kw, digits = 4))

# GMWL
eq_gmwl <- paste(expression("GMWL: "*delta^2*"H"), 
               "==8*",
               expression(delta^18*"O"),
               "+10")
lin_eq = function(x){8*x+10}
d2Hvsd18O <- ggplot(data) + 
  geom_point(aes(d18O_krk, d2H_krk, color="krk"), size=2, shape=0) +
  geom_line(aes(
    seq(-50, 20, length = length(data$d18O_kw)),
    predicted_d2Hvsd18O_krk, 
    color="krk")) +
  geom_point(aes(d18O_kw, d2H_kw, color="kw"), size=2, shape=1) +
  geom_line(aes(
    seq(-50, 20, length = length(data$d18O_kw)),
    predicted_d2Hvsd18O_kw,
    color="kw")) +
  geom_line(aes(
    seq(-50, 20, length = length(data$d18O_kw)),
    lin_eq(seq(-50, 20, length = length(data$d18O_kw))),
    color="gmwl"),
    linetype="dashed")+
  xlim(
    min(pmin(data$d18O_kw, data$d18O_krk, na.rm=T)), 
    max(pmax(data$d18O_kw, data$d18O_krk, na.rm=T)))+
  ylim(
    min(pmin(data$d2H_kw, data$d2H_krk, na.rm=T)), 
    max(pmax(data$d2H_kw, data$d2H_krk, na.rm=T)))+ 
  xlab(expression(delta^18*"O [\u2030]")) +
  ylab(expression(delta^2*"H [\u2030]")) +
  theme(aspect.ratio=1)+
  scale_color_manual(values=c(gmwl="black", kw="steelblue3", krk="orangered"), 
                     name="", 
                     labels=c(gmwl="GMWL", kw="Kasprowy Wierch", krk="Kraków"))+
  guides(colour = guide_legend(override.aes = list(shape = c(NA,1,0),
                                                   linetype = c(2, 1, 1))))+
  annotate("label",
           label = eq_krk, 
           x=min(pmin(data$d18O_kw, data$d18O_krk, na.rm=T)), y = 0,
           hjust = 0, vjust = 0, parse=TRUE)+
  annotate("label", 
           x=min(pmin(data$d18O_kw, data$d18O_krk, na.rm=T)), y = 0,
           label = rsqr_krk,
           hjust = -3.5, vjust = 0, parse=TRUE)+
  annotate("label", 
           x=min(pmin(data$d18O_kw, data$d18O_krk, na.rm=T)), y = -15, 
           label = eq_kw,
           hjust = 0, vjust = 0, parse=TRUE)+
  annotate("label", 
           x=min(pmin(data$d18O_kw, data$d18O_krk, na.rm=T)), y = -15, 
           label = rsqr_kw,
           hjust = -3.5, vjust = 0, parse=TRUE)+
  annotate("label", 
           x=min(pmin(data$d18O_kw, data$d18O_krk, na.rm=T)), y = -30, 
           label = eq_gmwl,
           hjust = 0, vjust = 0, parse=TRUE)+
  theme(legend.background = element_rect(fill=NA,
                                 size=0.5, linetype="solid", 
                                 colour ="black"))+
  theme(legend.title = element_blank(), 
        legend.position = c(0.9, 0.1))
d2Hvsd18O
ggsave(plot=d2Hvsd18O, filename = "plots/d2H_vs_d18O.pdf",
       width = 30, height = 30, units = "cm", device=cairo_pdf)
