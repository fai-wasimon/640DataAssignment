setwd("C:/Users/tosur/OneDrive/Desktop/POL640/DataHW/AllData")
#library(haven)  # library for read dta files in R
install.packages("tidyverse") 
library(dplyr) # in tidyverse for merging
library(readxl) # to read .xls file  

#### Loading IV datasets ####
## HIEF
HIEF <- read.csv("HIEF_data.csv") #from 1945-2013

## WDI 
WDI <- read.csv("WDI-selected-longform.csv") # from 1960-2020
names(WDI)[names(WDI) == "ï..Time"] <- "Year"
names(WDI)[names(WDI) == "Country.Name"] <- "Country"
names(WDI)[names(WDI) == "Total.natural.resources.rents....of.GDP...NY.GDP.TOTL.RT.ZS."] <- "RRents" #Total natural resources rents (% of GDP)
names(WDI)[names(WDI) == "GDP.per.capita..PPP..constant.2017.international.....NY.GDP.PCAP.PP.KD."] <- "GDPperCap" #GDP per capita, PPP (constant 2017 international $)
names(WDI)[names(WDI) == "GDP.growth..annual.....NY.GDP.MKTP.KD.ZG."] <- "GDPgrowth" #GDP growth (annual %)
## 


#### Putting two IV datasets together####
##only from 1960-2013 
summary(HIEF$Year)
HIEFsmol <- HIEF %>% filter(Year > 1959 & Year < 2014)
summary(HIEFsmol$Year)

typeof(WDI$Year) #It's integer
summary(WDI$Year) 
WDI$Year <- as.numeric(WDI$Year)+1958 #Make it numeric but 1960 -> 2 so we add 1958
summary(WDI$RRents)
summary(WDI$GDPgrowth)
summary(WDI$GDPperCap)
WDI$GDPgrowth <- as.numeric(as.character(WDI$GDPgrowth))
WDI$RRents <- as.numeric(as.character(WDI$RRents))
WDI$GDPperCap <- as.numeric(as.character(WDI$GDPperCap))/1000


WDIsmol <- WDI %>% filter(Year > 1959 & Year < 2014)


#View(WDIsmol)

nrow(HIEFsmol) #7608
nrow(WDIsmol) #14256 -- I am curious to why this is about twice as many as the HIEF data set. This is something that I should into later if I actually have to use the dataset for non-casual analysis.

IV = merge(HIEFsmol, WDIsmol, by.x=c("Year", "Country"), by.y=c("Year", "Country"))

nrow(IV) #6447 --> This might be that the merging command drop some countries whose names don't match between two data sets. Again this is something I should look into if I want to use the data for future analysis

#########################################################################
#########################################################################
#### MODEL 1: Using DD revisited ####
#### ** loading DD revisited
list.files(path = "C:/Users/tosur/OneDrive/Desktop/POL640/DataHW/AllData")
DD <- read_excel("ddrevisited_data_v1.xls")
#View(DD)
names(DD)[names(DD) == "ctryname"] <- "Country"
names(DD)[names(DD) == "year"] <- "Year"
#### ** Merge & Regress
df1 = merge(DD, IV, by.x=c("Year", "Country"), by.y=c("Year", "Country"))
#View(df1)
#nrow(df1) #5676 --> Again this drop quite a bit of data. I will have to come back later to check what happened.
summary(df1$democracy)
summary(df1$EFindex)
summary(df1$Year)
summary(df1$RRents)
summary(df1$GDPperCap)
summary(df1$GDPgrowth)
#All of these look okay so I didn't clean the data/recode but in reality I should.

#Because the scale of democracy is only 0 and 1, I also shouldn't use linear model here. But, for now, we'll assume that democracy is a continuous variable not bounded by 0 and 1. I also should control for fixed and time effects in the future.
lm1 <- lm(democracy ~ EFindex + RRents + GDPperCap + GDPgrowth, df1)
summary(lm1)


#########################################################################
#########################################################################
#### MODEL 2: using World Bank Database of Political Institutions ####

#### ** loading World Bank Database of Political Institutions
dpi2012 <- read_excel("dpi2012.xls")
names(dpi2012)[names(dpi2012) == "countryname"] <- "Country"
names(dpi2012)[names(dpi2012) == "year"] <- "Year"
#### ** Merge & Regress
df2 = merge(dpi2012, IV, by.x=c("Year", "Country"), by.y=c("Year", "Country"))
table(df2$Year)

summary(df2$eiec)
table(df2$eiec)
df2$eiec.r <- ifelse(df2$eiec < 1, NA, df2$eiec) #clean data
table(df2$eiec.r) #EIEC ranges from 1 to 7 
lm2 <- lm(eiec.r ~ EFindex + RRents + GDPperCap + GDPgrowth, df2)
summary(lm2)


#########################################################################
#########################################################################
#### MODEL 3: using Freedom House data ####

#### ** loading Freedom House data
FreedomHouse <- read_excel("2020_Aggregate_Category_and_Subcategory_Scores_FIW_2003-2020.xlsx", 2)

names(FreedomHouse)[names(FreedomHouse) == "Country/Territory"] <- "Country"
names(FreedomHouse)[names(FreedomHouse) == "Edition"] <- "Year"
colnames(FreedomHouse)
table(FreedomHouse$Country)
table(FreedomHouse$Year)
#### ** Merge & Regress
df3 = merge(FreedomHouse, IV, by.x=c("Year", "Country"), by.y=c("Year", "Country"))
summary(df3$Year)
summary(df3$Total)
lm3 <- lm(Total ~ EFindex + RRents + GDPperCap + GDPgrowth, df3)
summary(lm3)


#########################################################################
#########################################################################
#### MODEL 4: using Polity V data ####

#### ** loading Polity V data
PolityV <- read_excel("p5v2018.xls")

names(PolityV)[names(PolityV) == "country"] <- "Country"
names(PolityV)[names(PolityV) == "year"] <- "Year"

table(PolityV$Country)
table(PolityV$Year)
#### ** Merge & Regress
df4 = merge(PolityV, IV, by.x=c("Year", "Country"), by.y=c("Year", "Country"))
summary(df4$Year)
summary(df4$democ)
df4$democ.r <- ifelse(df4$democ < 0, NA, df4$democ) #clean data

lm4 <- lm(democ.r ~ EFindex + RRents + GDPperCap + GDPgrowth, df4)
summary(lm4)

