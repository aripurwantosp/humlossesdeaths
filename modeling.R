#modelling

#load libraries----
library(tidyverse)
library(MASS)
library(msme)
library(lmtest)
library(gridExtra)


#read data----
hloss_deaths <- read.csv("humlossesdeaths/hloss-deaths.csv", header = TRUE, sep = ",",
                         stringsAsFactors = FALSE) %>% as_tibble()
hloss_deaths


#deaths----

#/eda----
ipm_plot <- hloss_deaths  %>%  
  ggplot(aes(log(HDI), DL)) + geom_point() + geom_smooth(method = "lm")
gdp_plot <- hloss_deaths  %>%   
  ggplot(aes(log(GDP), DL)) + geom_point() + geom_smooth(method = "lm")
grid.arrange(ipm_plot, gdp_plot, ncol = 2)

#/model----

#//poisson
mdl_poiss_ <- glm(DL ~ HDI + log(GDP),
                  family = poisson, data = hloss_deaths)
P__disp(mdl_poiss_)

#//negative-binomial
mdl_ <- glm.nb(DL ~ HDI + log(GDP), data = hloss_deaths)
summary(mdl_)
lmtest::lrtest(mdl_)