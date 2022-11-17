########################################
# CORESIDENCE IPUMS                    #
# MOTHER_CODE TO SPLIT DATA FROM IPUMS #
# contact: jgaleano@ced.uab.es         #
########################################

# 1. WD ####

setwd("G:\\Shared drives\\CORESIDENCE\\WP2_DATA\\2_1_Input Data\\CENSUS_IPUMS\\A1_IMPUS_MOTHERS");getwd()

# 2. Libraries ####
 
library(ipumsr)
library(readxl)

# 3. Import data ####

ddi <- read_ipums_ddi("ipumsi_00115.xml")
data <- read_ipums_micro(ddi)
head(data,10) # VARIABLES HAVE THEIR LABELS

# 4. Mini Test ####

mini_test<-data[c(1:100),]
levels(mini_test$COUNTRY)
head(data,10)
class(mini_test$SAMPLE)
mini_test$COUNTRY2<-as_factor(mini_test$COUNTRY)
codes_countries <- read_excel("G:/Shared drives/CORESIDENCE/WP2. DATA/2.1 Input Data/CENSUS_IPUMS/A1_IMPUS_MOTHERS/codes_countries.xlsx")
mini_test<-as.data.frame(mini_test)
codes_countries<-as.data.frame(codes_countries)
mini_test <- data.frame(mini_test, codes_countries[match(mini_test[,"COUNTRY2"], codes_countries[,"country"]),c("iso3")])
colnames(mini_test)[70]<-"ISO_CODE"

# 5. Read codes iso countries and add it to ipums data ####

codes_countries <- read_excel("G:/Shared drives/CORESIDENCE/WP2. DATA/2.1 Input Data/CENSUS_IPUMS/A1_IMPUS_MOTHERS/codes_countries.xlsx")
data$COUNTRY2<-as_factor(data$COUNTRY)
data<-as.data.frame(data)
codes_countries<-as.data.frame(codes_countries)
data <- data.frame(data, codes_countries[match(data[,"COUNTRY2"], codes_countries[,"country"]),c("iso3")])
colnames(data)[70]<-"ISO_CODE"

# 6. Split data by SAMPLE and save file by country and year ####

IPUMS_list<-split(data , f = data$SAMPLE )
save(IPUMS_list, file="G:/Shared drives/CORESIDENCE/WP2. DATA/2.1 Input Data/CENSUS_IPUMS/A1_IMPUS_MOTHERS/IPUMS_list.Rdata")

lapply(names(IPUMS_list), function(x) {
  df <- IPUMS_list[[x]]
  df_ISO_CODE<-unique(df$ISO_CODE)
  df_YEAR<-unique(df$YEAR)
  save(df, file=paste0(getwd(),'/',"IPUMS","_",df_ISO_CODE,"_",df_YEAR, '.RData'))
})
