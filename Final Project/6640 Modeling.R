library(tidyverse)
library(lme4) ## For lmer function
library(MASS) ## For glmmPQL function and final model

data3 <- read.csv("~/Documents/BIOS 6640/Final_Data.csv")

##########################
## POISSON MIXED MODELS ##
##########################

## Doesn't converge
mod1 <- glmer(cases~raintot2+raintot4+raintot8
              +rh+ITN+I(ITN^2)+IRS+tavg+I(tavg^2)
                +(1|District)+offset(log(u5total)),
              data = dat, family=poisson())

## Converges
mod2 <- glmmPQL(cases~raintot2+raintot4+raintot8+I(raintot2^2)+I(raintot4^2)+I(raintot8^2)
                +rh+ITN+I(ITN^2)+IRS+tavg+I(tavg^2)
                +offset(log(u5total)), random =  ~1|District,
                data = dat, family=poisson())

## Don't need square 8 week lag
Anova(mod2)

mod3 <- glmmPQL(cases~raintot2+raintot4+raintot8+I(raintot2^2)+I(raintot4^2)
                +rh+ITN+I(ITN^2)+IRS+tavg+I(tavg^2)
                +offset(log(u5total)), random =  ~1|District,
                data = dat, family=poisson())

## Don't need 8 week lag
Anova(mod3)

mod4 <- glmmPQL(cases~raintot2+raintot4+I(raintot2^2)+I(raintot4^2)
                +rh+ITN+I(ITN^2)+IRS+tavg+I(tavg^2)
                +offset(log(u5total)), random =  ~1|District,
                data = dat, family=poisson())
## Don't need IRS
Anova(mod4)
