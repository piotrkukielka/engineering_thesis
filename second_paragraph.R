rm(list = ls())
library(lubridate)
#library(readxl)
#library(dplyr)
#library(tibbletime)
#library(weathermetrics)
library(tidyverse)
library(ggplot2)
library(ggrepel)

# load data
datakrk <- read_rds("data/datakrk.rds")
datakw <- read_rds("data/datakw.rds")

# d2H against d18O 
### Krakow
model_d2Hvsd18O <- lm(d2H ~ d18O, datakrk)
predicted_d2Hvsd18O <- predict(model_d2Hvsd18O, datakrk)

rsqr <- paste(expression(R^2), "==", 
              format(summary(model_d2Hvsd18O)[["r.squared"]], digits = 4))
coeff_a <- format(coef(model_d2Hvsd18O)[["d18O"]], digits = 4)
coeff_b <- format(coef(model_d2Hvsd18O)[["(Intercept)"]], digits = 4)
eq <- paste(expression(delta^2*"H"), 
            "==",
            format(coeff_a, digits = 4),
            "*",
            expression(delta^18*"O"),
            "+",
            format(coeff_b, digits = 4))

d2Hvsd18O <- ggplot() + 
  geom_point(data=datakrk, aes(d18O, d2H)) +
  geom_line(data=datakrk, aes(d18O, predicted_d2Hvsd18O)) + 
  xlab(expression(delta^(18)*"O")) +
  ylab(expression(delta^(2)*"H")) +
  annotate("label", -Inf, Inf, 
           label = eq,
           hjust = 0, vjust = 1.5, parse=TRUE)+
  annotate("label", -Inf, Inf, 
           label = rsqr,
           hjust = 0, vjust = 3.5, parse=TRUE)
d2Hvsd18O
### Kasprowy
model_d2Hvsd18O <- lm(d2H ~ d18O, datakw)
predicted_d2Hvsd18O <- predict(model_d2Hvsd18O, datakw)

rsqr <- paste(expression(R^2), "==", 
              format(summary(model_d2Hvsd18O)[["r.squared"]], digits = 4))
coeff_a <- format(coef(model_d2Hvsd18O)[["d18O"]], digits = 4)
coeff_b <- format(coef(model_d2Hvsd18O)[["(Intercept)"]], digits = 4)
eq <- paste(expression(delta^2*"H"), 
            "==",
            format(coeff_a, digits = 4),
            "*",
            expression(delta^18*"O"),
            "+",
            format(coeff_b, digits = 4))

d2Hvsd18O + 
  geom_point(data = datakw, aes(d18O, d2H)) +
  geom_line(data = datakw, aes(d18O, predicted_d2Hvsd18O)) + 
  xlab(expression(delta^(18)*"O")) +
  ylab(expression(delta^(2)*"H")) +
  annotate("label", -Inf, Inf, 
           label = eq,
           hjust = 0, vjust = 1.5, parse=TRUE)+
  annotate("label", -Inf, Inf, 
           label = rsqr,
           hjust = 0, vjust = 3.5, parse=TRUE)
d2Hvsd18O
#ggsave("results/time/d2H_vs_d18O.pdf", d2Hvsd18O)

# d2H against temperature 
model_d2Hvstemp <- lm(d2H ~ `temp [C] <balice>`, datakrk)
predicted_d2Hvstemp <- predict(model_d2Hvstemp, datakrk)

rsqr <- paste(expression(R^2), "==", 
              format(summary(model_d2Hvstemp)[["r.squared"]], digits = 4))
coeff_a <- format(coef(model_d2Hvstemp)[["`temp [C] <balice>`"]], digits = 4)
coeff_b <- format(coef(model_d2Hvstemp)[["(Intercept)"]], digits = 4)
eq <- paste(expression(delta^2*"H"), 
            "==",
            format(coeff_a, digits = 4),
            "*",
            expression("T"),
            "+",
            format(coeff_b, digits = 4))

d2Hvstemp <- ggplot(datakrk, aes(`temp [C] <balice>`, d2H)) + 
  geom_point() +
  geom_line(aes(`temp [C] <balice>`, predicted_d2Hvstemp)) + 
  xlab(expression("Temperatura ["*degree*"C]")) +
  ylab(expression(delta^(2)*"H")) +
  annotate("label", -Inf, Inf, 
           label = eq,
           hjust = 0, vjust = 1.5, parse=TRUE)+
  annotate("label", -Inf, Inf, 
           label = rsqr,
           hjust = 0, vjust = 3.5, parse=TRUE)
d2Hvstemp
#ggsave("results/time/d2H_vs_temp.pdf", d2Hvstemp)
# proponuję - dać to jako linię trendu, a tak ogólnie to widać, że to nie jest na prostej
# pokazać korelacje między wartościami tego samego, ale tylko dla jednego wykres
# wypisac brakujace dane
# jednostki? np rain?
# co zrobic ze strefa czasowa

# d2H against rain amount 
model_d2Hvsrain <- lm(d2H ~ `rain [??] <dach>`, datakrk)
predicted_d2Hvsrain <- predict(model_d2Hvsd18O, datakrk)

rsqr <- paste(expression(R^2), "==", 
              format(summary(model_d2Hvsrain)[["r.squared"]], digits = 4))
coeff_a <- format(coef(model_d2Hvsrain)[["`rain [??] <dach>`"]], digits = 4)
coeff_b <- format(coef(model_d2Hvsrain)[["(Intercept)"]], digits = 4)
eq <- paste(expression(delta^2*"H"), 
            "==",
            format(coeff_a, digits = 4),
            "*",
            expression("rain"),
            "+",
            format(coeff_b, digits = 4))

d2Hvsrain <- ggplot(datakrk, aes(`rain [??] <dach>`, d2H)) + 
  geom_point() +
  #geom_line(aes(`rain [??] <dach>`, predicted_d2Hvsrain)) + 
  xlab(expression("Ilość opadów [mm]")) +
  ylab(expression(delta^(2)*"H")) #+
 # annotate("label", -Inf, Inf, 
 #          label = eq,
 #          hjust = 0, vjust = 1.5, parse=TRUE)+
 # annotate("label", -Inf, Inf, 
 #          label = rsqr,
 #          hjust = 0, vjust = 3.5, parse=TRUE)
d2Hvsrain
#ggsave("results/time/d2H_vs_rainamount.pdf", d2Hvsrain)

# d2H against humidity 
model_d2Hvshum <- lm(d2H ~ `rel. humidity [??] <dach>`, datakrk)
predicted_d2Hvshum <- predict(model_d2Hvsd18O, datakrk)

rsqr <- paste(expression(R^2), "==", 
              format(summary(model_d2Hvshum)[["r.squared"]], digits = 4))
coeff_a <- format(coef(model_d2Hvshum)[["`rel. humidity [??] <dach>`"]], digits = 4)
coeff_b <- format(coef(model_d2Hvshum)[["(Intercept)"]], digits = 4)
eq <- paste(expression(delta^2*"H"), 
            "==",
            format(coeff_a, digits = 4),
            "*",
            expression("hum"),
            "+",
            format(coeff_b, digits = 4))

d2Hvshum <- ggplot(datakrk, aes(`rel. humidity [??] <dach>`, d2H)) + 
  geom_point() +
  #geom_line(aes(`rel. humidity [??] <dach>`, predicted_d2Hvshum)) + 
  xlab(expression("Wilgotność względna [%]")) +
  ylab(expression(delta^(2)*"H")) #+
 # annotate("label", -Inf, Inf, 
 #          label = eq,
 #          hjust = 0, vjust = 1.5, parse=TRUE)+
 # annotate("label", -Inf, Inf, 
 #          label = rsqr,
 #          hjust = 0, vjust = 3.5, parse=TRUE)
d2Hvshum
#ggsave("results/time/d2H_vs_humidity.pdf", d2Hvshum)

d2Hvspress <- ggplot(datakrk, aes(`pressure [hPa] <dach>`, d2H)) + 
  geom_point() +
  #geom_line(aes(`pressure [hPa] <dach>`, predicted_d2Hvshum)) + 
  xlab(expression("Ciśnienie atmosferyczne [hPa]")) +
  ylab(expression(delta^(2)*"H")) #+
# annotate("label", -Inf, Inf, 
#          label = eq,
#          hjust = 0, vjust = 1.5, parse=TRUE)+
# annotate("label", -Inf, Inf, 
#          label = rsqr,
#          hjust = 0, vjust = 3.5, parse=TRUE)
d2Hvspress
#ggsave("results/time/d2H_vs_pressure.pdf", d2Hvspress)

### trzeci akapit
model_dexcess <- lm(dexcess ~ d18O, datakrk)
predicted_dexcess <- predict(model_dexcess, datakrk)

rsqr <- paste(expression(R^2), "==", 
              format(summary(model_dexcess)[["r.squared"]], digits = 4))
coeff_a <- format(coef(model_dexcess)[["d18O"]], digits = 4)
coeff_b <- format(coef(model_dexcess)[["(Intercept)"]], digits = 4)
eq <- paste(expression(d-excess), 
            "==",
            format(coeff_a, digits = 4),
            "*",
            expression(delta^18*"O"),
            "+",
            format(coeff_b, digits = 4))

dexcessvsd18O <- ggplot(datakrk, aes(d18O, dexcess)) + 
  geom_point() +
  geom_line(aes(d18O, predicted_dexcess)) + 
  xlab(expression(delta^(18)*"O")) +
  ylab(expression("dexcess")) +
  annotate("label", -Inf, Inf, 
           label = eq,
           hjust = 0, vjust = 1.5, parse=TRUE)+
  annotate("label", -Inf, Inf, 
           label = rsqr,
           hjust = 0, vjust = 3.5, parse=TRUE)
dexcessvsd18O
#ggsave("results/time/dexcess_vs_d18O.pdf", dexcessvsd18O)

# DELTA
model_DELTA17O <- lm(DELTA17O ~ d18O, datakrk)
predicted_DELTA17O <- predict(model_DELTA17O, datakrk)

rsqr <- paste(expression(R^2), "==", 
              format(summary(model_DELTA17O)[["r.squared"]], digits = 4))
coeff_a <- format(coef(model_DELTA17O)[["d18O"]], digits = 4)
coeff_b <- format(coef(model_DELTA17O)[["(Intercept)"]], digits = 4)
eq <- paste(expression(Delta^17*"O"), 
            "==",
            format(coeff_a, digits = 4),
            "*",
            expression(delta^18*"O"),
            "+",
            format(coeff_b, digits = 4))

DELTA17Ovsd18O <- ggplot(datakrk, aes(d18O, DELTA17O)) + 
  geom_point() +
  geom_line(aes(d18O, predicted_DELTA17O)) + 
  xlab(expression(delta^(18)*"O")) +
  ylab(expression(Delta^17*"O")) +
  annotate("label", -Inf, Inf, 
           label = eq,
           hjust = 0, vjust = 1.5, parse=TRUE)+
  annotate("label", -Inf, Inf, 
           label = rsqr,
           hjust = 0, vjust = 3.5, parse=TRUE)
DELTA17Ovsd18O
#ggsave("results/time/DELTA17O_vs_d18O.pdf", DELTA17Ovsd18O)