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

model_dprim17Ovsdprim18O_krk <- lm(dprim17O_krk ~ dprim18O_krk, data)
predicted_dprim17Ovsdprim18O_krk <- predict(model_dprim17Ovsdprim18O_krk, 
                                  newdata = data.frame(dprim18O_krk = seq(-50, 20, length = length(data$dprim18O_krk))))

rsqr_krk <- paste(expression(R^2), "==", 
              format(summary(model_dprim17Ovsdprim18O_krk)[["r.squared"]], digits = 4))
coeff_a_krk <- format(coef(model_dprim17Ovsdprim18O_krk)[["dprim18O_krk"]], digits = 4)
coeff_b_krk <- format(coef(model_dprim17Ovsdprim18O_krk)[["(Intercept)"]], digits = 4)
eq_krk <- paste(expression("Kraków: "*delta*"'"^17*"O"), 
            "==",
            format(coeff_a_krk, digits = 4),
            "*",
            expression(delta*"'"^18*"O"),
            "+",
            format(coeff_b_krk, digits = 4))
# kw pred
model_dprim17Ovsdprim18O_kw <- lm(dprim17O_kw ~ dprim18O_kw, data)
predicted_dprim17Ovsdprim18O_kw <- predict(model_dprim17Ovsdprim18O_kw, 
                                  #newdata = data.frame(dprim18O_kw = data$dprim18O_kw))
                                  newdata = data.frame(dprim18O_kw = seq(-50, 20, length = length(data$dprim18O_kw))))

rsqr_kw <- paste(expression(R^2), "==", 
              format(summary(model_dprim17Ovsdprim18O_kw)[["r.squared"]], digits = 4))
coeff_a_kw <- format(coef(model_dprim17Ovsdprim18O_kw)[["dprim18O_kw"]], digits = 4)
coeff_b_kw <- format(coef(model_dprim17Ovsdprim18O_kw)[["(Intercept)"]], digits = 4)
eq_kw <- paste(expression("Kasprowy Wierch: "*delta*"'"^17*"O"), 
            "==",
            format(coeff_a_kw, digits = 4),
            "*",
            expression(delta*"'"^18*"O"),
            "+",
            format(coeff_b_kw, digits = 4))

# GMWL
eq_gmwl <- paste(expression("GMWL: "*delta^17*"O"), 
               "==0.528*",
               expression(delta^18*"O"),
               "+0.000033")
lin_eq = function(x){0.528*x+0.000033}
dprim17Ovsdprim18O <- ggplot(data) + 
  geom_point(aes(dprim18O_krk, dprim17O_krk, color="red"), size=2, shape=0) +
  geom_line(aes(
    seq(-50, 20, length = length(data$dprim18O_kw)),
    predicted_dprim17Ovsdprim18O_krk, 
    color="red")) +
  geom_point(aes(dprim18O_kw, dprim17O_kw, color="blue"), size=2, shape=1) +
  geom_line(aes(
    seq(-50, 20, length = length(data$dprim18O_kw)),
    predicted_dprim17Ovsdprim18O_kw,
    color="blue")) +
  geom_line(aes(
    seq(-50, 20, length = length(data$dprim18O_kw)),
    lin_eq(seq(-50, 20, length = length(data$dprim18O_kw))),
    color="black"),
    linetype="dashed")+
  xlim(
    min(pmin(data$dprim18O_kw, data$dprim18O_krk, na.rm=T)), 
    max(pmax(data$dprim18O_kw, data$dprim18O_krk, na.rm=T)))+
  ylim(
    min(pmin(data$dprim17O_kw, data$dprim17O_krk, na.rm=T)), 
    max(pmax(data$dprim17O_kw, data$dprim17O_krk, na.rm=T)))+ 
  xlab(expression(delta*"'"^18*"O [\u2030]")) +
  ylab(expression(delta*"'"^17*"H [\u2030]")) +
  theme(aspect.ratio=1, legend.position = c(0.9, 0.1))+
  scale_color_manual(values=c("black", "steelblue3", "orangered"), 
                     labels=c("GMWL", "Kasprowy Wierch", "Kraków"))+
  guides(colour = guide_legend(override.aes = list(shape = c(NA,1,0),
                                                   linetype = c(2, 1, 1))))+
  annotate("label",
           label = eq_krk, 
           x=min(pmin(data$dprim18O_kw, data$dprim18O_krk, na.rm=T)), y = -1,
           hjust = 0, vjust = 0, parse=TRUE)+
  annotate("label", 
           x=min(pmin(data$dprim18O_kw, data$dprim18O_krk, na.rm=T)), y = -1,
           label = rsqr_krk,
           hjust = -3.5, vjust = 0, parse=TRUE)+
  annotate("label", 
           x=min(pmin(data$dprim18O_kw, data$dprim18O_krk, na.rm=T)), y = -3, 
           label = eq_kw,
           hjust = 0, vjust = 0, parse=TRUE)+
  annotate("label", 
           x=min(pmin(data$dprim18O_kw, data$dprim18O_krk, na.rm=T)), y = -3, 
           label = rsqr_kw,
           hjust = -4, vjust = 0, parse=TRUE)+
  annotate("label", 
           x=min(pmin(data$dprim18O_kw, data$dprim18O_krk, na.rm=T)), y = -5, 
           label = eq_gmwl,
           hjust = 0, vjust = 0, parse=TRUE)+
  theme(legend.background = element_rect(fill=NA,
                                 size=0.5, linetype="solid", 
                                 colour ="black"))+
  theme(legend.title = element_blank(), 
        legend.position = c(0.9, 0.1))
  # geom_point(data=data[which.min(data$dprim17O_krk),], 
  #            aes(dprim18O_krk, dprim17O_krk),
  #            color="green", size=5)
dprim17Ovsdprim18O
ggsave(plot=dprim17Ovsdprim18O, filename = "plots/dprim17O_vs_dprim18O.pdf",
       width = 30, height = 30, units = "cm", device=cairo_pdf)