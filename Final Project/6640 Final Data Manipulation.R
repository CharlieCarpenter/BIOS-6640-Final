#######################
## Charlie Carpenter ##
## BIOS 6640 Final   ##
#######################

library(tidyverse) ## For data manipulation
library(XML) ## For reading in HTML Tables
library(epitools) ## For epiweek function mostly
library(Hmisc) ## For Lag() function

districts <- readHTMLTable("http://rap.ucar.edu/staff/monaghan/colborn/mozambique/daily/v2/", 
                           skip.rows=1:2)[[1]]$Name
districts <- as.character(districts)

# remove NA in final row
districts <- districts[-143]


# loop attaches file path, read in each file, pulls the district name, adds 'District' as a new column
# and binds each successive district to the last to make one long dataframe
all_districts <- NULL
for(i in 1:length(districts)) {
  districts2<- paste("http://rap.ucar.edu/staff/monaghan/colborn/mozambique/daily/v2/", districts[i],sep="")
  dis <- read.table(districts2, header=F, sep="", skip=3)
  name <- strsplit(districts[i], '_f') [[1]] [1]
  name_vector <- rep(name, times=length(dis$V1))
  dis$District <- NA
  dis$District <- name_vector
  if (i==1){
    all_districts <- dis
  } else {
    all_districts <- rbind(all_districts, dis)
  }
}

# Change names of variables
names(all_districts) <- c("year", "month", "day", "raint", "tavg", "rh",
                          "vapor(mmHg)", "baro(hPa)", "District")

# write to txt file
write.table(all_districts, file="Districts.txt")

## Reading in file from line 37
dri <- read.table(file="~/Documents/BIOS 6640/Districts.txt")

dri$District <- dri$District %>% # first remove last 4 positions in character string
                as.character() %>%
                sapply(function(x) substr(x,1,nchar(x)-4)) %>% 
                gsub("_", " ", .) %>% # Replacing "_" with " "
                as.factor() # making a factor again

# Make a date column
Epi <- dri %>%
  with(paste(year,month,day,sep = "-")) %>% ## Formatting for epiweek
  as.Date("%Y-%m-%d") %>% ## Making a date
  as.character %>% 
  as.week(origin = as.Date("2009-01-01"), format = "%Y-%m-%d", sunday = TRUE) ## Making and Epiweek

# add epiweek to districts
dri$Epiweek <- Epi$week %>% as.numeric

## Summarizing weather by DISTCODE, year, and Epiweek
weather <- dri %>% 
  filter(year >= 2010) %>%
    group_by(District,year,Epiweek) %>%
    summarise(tavg = mean(tavg), raintot = sum(raint), rh = mean(rh), 
              sd = mean(vapor.mmHg.), psfc = mean(baro.hPa.))

## Reading in incidence and intervention data
incidence <- read.csv("~/Downloads/incidence.csv", sep=",")
incidence$Epiweek <- as.numeric(incidence$Epiweek)

intervention <- read.csv("~/Downloads/intervention.csv", sep=",")

## merging intervention and incidence with weather and summarizing weather 
data <- weather %>% 
        left_join(incidence, by=c('year'='Epiyear', 'Epiweek', 'District'= 'District')) %>%
        left_join(intervention %>%
                    select(DISTCODE, IRSyear, IRSepiWeek) %>% # select only IRS 
                    filter(!is.na(IRSyear) & IRSyear > 2009) %>% #Select for Epiyear > 2009
                    rename(year =  IRSyear, Epiweek = IRSepiWeek) %>% # Rename to match
                    mutate(IRSprot = ifelse(!is.na(year), 1, NA)), 
                    # Create protection variable to spread down the col later
                  by = c("DISTCODE","year","Epiweek"), all = TRUE)
                    
data2 <- merge(data,
               intervention %>% # here we only select the IRS year first 
                 select(DISTCODE, ITNyear, ITNepiWeek) %>% # select only IRS 
                 filter(!is.na(ITNyear) & ITNyear > 2009) %>% #Select for Epiyear > 2009
                 rename(year =  ITNyear, Epiweek = ITNepiWeek) %>% # Rename to match
                 mutate(ITNprot = ifelse(!is.na(year), 1, NA)), # Create protection variable 
               by = c("DISTCODE","year","Epiweek"), all = TRUE)

## Making lag rain totals withing district
data.lag <- data2 %>%
  group_by(DISTCODE) %>% 
  mutate(raintot2 = Lag(raintot, 2), ## 2 week lag
         raintot4 = Lag(raintot, 4), ## 4 week lag
         raintot8 = Lag(raintot, 8)) ## 8 week lag

## Removing unknown DISTCODES and Distcodes with one row
data.dist <- data.lag %>% 
  filter(!is.na(DISTCODE), DISTCODE != 503, DISTCODE != 701, 
         DISTCODE != 801, DISTCODE != 902, DISTCODE != 912)

## NAs to 0s
data.dist$ITNprot[is.na(data.dist$ITNprot)] <- 0
data.dist$IRSprot[is.na(data.dist$IRSprot)] <- 0

## Spreading Intervention effectiveness down columns

## For loops are bad and you should feel bad...
## but it's quick enough and I think these are pretty slick

ITN.s <- NULL
for(j in unique(data.dist$DISTCODE)){
  d <- data.dist %>% 
    filter(DISTCODE == j)
  for(i in 1:(nrow(d)-1)){
     if(d[i,]$ITNprot != 0){
      if(d[i+1,]$ITNprot != 1) d[i+1,]$ITNprot <- d[i,]$ITNprot*.995
    }
  }                 ## Taking
  ITN.s <- c(ITN.s, d$ITNprot) 
}

IRS.s <- NULL
for(j in unique(data.dist$DISTCODE)){
  d <- data.dist %>% 
    filter(DISTCODE == j)
  for(i in 1:(nrow(d)-1)){
    if(d[i,]$ITNprot != 0){
      if(d[i+1,]$IRSprot != 1) d[i+1,]$IRSprot <- d[i,]$IRSprot*.99
    }
  } 
  IRS.s <- c(IRS.s, d$IRSprot) 
}

Final_data <- cbind(data.dist,ITN=ITN.s,IRS=IRS.s)

## Make data file to read in later
write_csv(Final_data,"~/Documents/BIOS 6640/Final_Data.csv")

