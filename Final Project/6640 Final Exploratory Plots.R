#######################
## EXPLORATORY PLOTS ##
#######################

library(tidyverse)

dat <- read.csv("~/Documents/BIOS 6640/Final_Data.csv")

## Clear dip in the middle of the year
ggplot(dat, aes(x=Epiweek, y=cases)) + 
  geom_smooth() + 
  theme_bw() + 
  theme(legend.position="none")

## Not quite as clear but still there
ggplot(dat, aes(x=Epiweek, y=cases, col=District)) + 
  geom_smooth() + 
  theme_bw() + 
  theme(legend.position="none")

## Not very informative
ggplot(dat, aes(x=raintot, y=cases, col=District)) + 
  geom_smooth(se=F) + 
  theme_bw() + 
  theme(legend.position="none")

## In general cases increase but not within districts
ggplot(dat, aes(x=raintot, y=cases)) + 
  geom_smooth() + 
  theme_bw() + 
  theme(legend.position="none")

## Possibly square tavg for within District?
## This isn't pretty at all but I still need it to justify the square in the model
ggplot(dat, aes(x=tavg, y=cases, col=District)) + 
  geom_smooth(se=F) + 
  theme_bw() + 
  labs(title = "Malaria Cases over Temperature by District",
       x = "Weekly Average Temperature", y = "Malaria Cases") +
  theme(legend.position="none")

ggplot(dat, aes(x=raintot2,y=cases)) +
  geom_smooth() +
  geom_smooth(data=dat, aes(x=raintot4,y=cases, col="red")) +
  geom_smooth(data=dat, aes(x=raintot8,y=cases, col="green")) +
  theme_bw()

## Lagged Rainfall
ggplot(dat, aes(x=raintot2, y=cases, col="2 Week Lag")) +
  geom_smooth(se=F) +
  geom_smooth(aes(x=raintot4, y=cases, col="4 Week Lag"), se=F) +
  geom_smooth(aes(x=raintot8, y=cases, col="8 Week Lag"), se=F) +
  labs(title="Malaria Cases vs. Lagged Rainfall",
       x ="Total Rainfall Weekly", y ="Malaria Cases") +
  theme_minimal()

## Curved relationship??
ggplot(dat, aes(x=ITN, y=cases, col="ITNs")) +
  geom_smooth(se=F) +
  geom_smooth(aes(x=IRS, y=cases, col="IRS"), se=F) +
  labs(titler = "Intervention Efficacy vs Malaria Cases",
       x = "Intervention Efficacy (%)", y = "Malaria Cases") +
  theme_minimal()

## This probably needs something more complicated like splines or harmonics, 
## but I'm not confident in using them correctly. Go with linear relation
ggplot(dat, aes(x=IRS, y=cases)) +
  geom_smooth() +
  theme_minimal()

## Mostly linear within district
ggplot(dat, aes(x=rh, y=cases, col=District)) +
  geom_smooth(se=F) +
  theme_minimal() +
  theme(legend.position="none")

## Year is mostly flat
ggplot(dat, aes(x=year, y=cases, col=District)) +
  geom_smooth(se=F) +
  theme_minimal() +
  theme(legend.position="none")

