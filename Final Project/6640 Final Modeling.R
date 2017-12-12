library(tidyverse)
library(lme4) ## For lmer function
library(MASS) ## For glmmPQL function and final model

data3 <- read.csv("~/Documents/BIOS 6640/Final_Data.csv")

##########################
## POISSON MIXED MODELS ##
##########################

mod3 <- glmmPQL(cases~raintot2+raintot4+raintot8+I(raintot2^2)
                +rh+ITN+I(ITN^2)+IRS+tavg+I(tavg^2)+year+Epiweek+I(Epiweek^2)
                +offset(log(u5total)), random =  ~1|District,
                data = dat, family=poisson())

Anova(mod3)
summary(mod3)

## This looks terrible
qqnorm(residuals(mod3))
qqline(residuals(mod3))
