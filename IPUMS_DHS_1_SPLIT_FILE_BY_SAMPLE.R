############################################
# CORESIDENCE IPUMS                        #
# MOTHER_CODE TO SPLIT DATA FROM IPUMS DHS #
# contact: jgaleano@ced.uab.es             #
############################################

# 1. WD ####

setwd("G:\\Shared drives\\CORESIDENCE\\WP2_DATA\\2_1_Input Data\\DHS_IPUMS\\A1_IPUMS_DHS_MOTHERS");getwd()

# 2. Libraries ####

library(ipumsr)
library(readxl)

# 3. Import data ####

ddi <- read_ipums_ddi("idhs_00007.xml")
data <- read_ipums_micro(ddi)
head(data,10) # VARIABLES HAVE THEIR LABELS


# 5. Read codes iso countries and add it to ipums data ####

codes_countries <- read_excel("G:\\Shared drives\\CORESIDENCE\\WP2_DATA\\2_1_Input Data\\CENSUS_IPUMS\\A1_IMPUS_MOTHERS\\codes_countries.xlsx")
data$COUNTRY2<-as_factor(data$COUNTRY)
data<-as.data.frame(data)
codes_countries<-as.data.frame(codes_countries)
data <- data.frame(data, codes_countries[match(data[,"COUNTRY2"], codes_countries[,"country"]),c("iso3")])
colnames(data)[213]<-"ISO_CODE"

# 6. Split data by SAMPLE and save file by country and year ####

IPUMS_DHS_list<-split(data , f = data$SAMPLE )

not_all_na <- function(x) any(!is.na(x))

IPUMS_DHS_list<-lapply(IPUMS_DHS_list, function(df) {
  df<-df %>% select(where(not_all_na))
  df
})

save(IPUMS_DHS_list, file="G:\\Shared drives\\CORESIDENCE\\WP2_DATA\\2_1_Input Data\\DHS_IPUMS\\IPUMS_DHS_list.Rdata")

setwd("G:\\Shared drives\\CORESIDENCE\\WP2_DATA\\2_1_Input Data\\DHS_IPUMS\\")

lapply(names(IPUMS_DHS_list), function(x) {
  df <- IPUMS_DHS_list[[x]]
  df_ISO_CODE<-unique(df$ISO_CODE)
  df_YEAR<-unique(df$YEAR)
  save(df, file=paste0(getwd(),'/',df_ISO_CODE,"_",df_YEAR,"_","IPUMS_DHS",'.RData'))
})
