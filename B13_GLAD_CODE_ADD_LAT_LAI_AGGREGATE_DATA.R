library(tidyverse)
library(readxl)
library(readr)

# Living arrangement index ######

data <- read_csv("./input_data/B_GLAD_FROM_LAV_TO_LAT_LAI.csv")

data<-data|>select(LAV, LAT, LAI)

# IPUMS samples #########
# Set working directory ####

setwd("./output_microdata/")
# Get a list of all Rda files in the directory ####
files <- list.files(path = getwd(),
                    pattern = 'Rda',
                    full.names = FALSE)

samples<-substr(files,1,14)
# Read each file, select variables, add LAT AND LAI, aggregate data and save in separate files######

system.time(
  for (x in samples) {
    
    i <- x
    
    loaded_name <-load(paste(i,"Rda",sep=".")) 
    
    # Assign the loaded object to a new name 'df'
    df <- get(loaded_name)
    
    variables<-c("SOURCE", "SAMPLE","CONTINENT","CNTRY_ISO","CNTRY","GEOLEVEL1","YEAR",
                 "SERIAL","PERNUM","GQ","HWEIGHT","PWEIGHT","PWEIGHT_A","SEX","AGE", "AGE2",      
                 "RELATE","RELATED","RELATED2","MOMLOC","POPLOC","SPLOC","NCHILD","MARST",    
                 "EDATTAIN","n","GELAI","GELAIQ")
    
    df<-df|>select(all_of(variables))
    df<-df|>
      select(1:28)|>
      rename(LAV = GELAI)
    
    df<-df|>
      left_join(data, by="LAV")
    
    df<-df|>
      group_by(SAMPLE,YEAR,CNTRY_ISO,SEX,AGE,MARST, EDATTAIN,LAT,LAI)|>
      summarise(unweighted_pop=n(),
                weighted_pop=sum(PWEIGHT))|>
      ungroup()
    
    # Save aggregated data ######
    dataframe_name<-paste(unique(df$SAMPLE),"_LA",sep="")
    assign(dataframe_name,df)
    
    objects_to_save1 <-dataframe_name
    save(list=objects_to_save1,
         file = paste("./output_microdata/aggregated_data_ipums",objects_to_save1, ".Rda",sep=""))
    
  })

# Read all IPUMS aggreagated data, extract into a dataframe and save a single element. ######
setwd("./output_microdata/aggregated_data_ipums")
patt.shp <- ".*Rda*"

data.shp <-list.files(pattern=patt.shp)

LA_IPUMS_LIST <- sapply(data.shp, function(x) mget(load(x)), simplify = TRUE) 

DF_LA_IPUMS <- data.table::rbindlist(LA_IPUMS_LIST)

save(DF_LA_IPUMS, file="./input_data/IPUMS_301.Rda")


# LFS samples #########
# Set working directory ######
setwd("./input_data/LFS1_K2/")
# Get a list of all Rdata files in the directory ####
files <- list.files(path = getwd(),
                    pattern = 'Rda',
                    full.names = FALSE)

samples<-substr(files,1,12)

# Read each file, select variables, add LAT AND LAI, aggregate data and save in separate files######
system.time(
  for (x in samples) {
     i <- x
    
    loaded_name <-load(paste(i,"Rda",sep=".")) 
    
    # Assign the loaded object to a new name 'df'
    df <- get(loaded_name)
    
    variables<-c("SAMPLE","YEAR","CNTRY_ISO","SEX","AGE","MARST", "EDATTAIN","pop_w", "GELAI")
    
    df<-df|>select(all_of(variables))|>
      rename(LAV = GELAI)

    df<-df|>
      left_join(data, by="LAV")
    
    df<-df|>
      group_by(SAMPLE,YEAR,CNTRY_ISO,SEX,AGE,MARST, EDATTAIN,LAT,LAI )|>
      summarise(unweighted_pop=n(),
                weighted_pop=sum(pop_w*1000))|>
      ungroup()
    
    # 8. Save aggregated data GELAI ######
    dataframe_name<-unique(df$SAMPLE)
    assign(dataframe_name,df)
    
    objects_to_save1 <-dataframe_name
    save(list=objects_to_save1,
         file = paste("./input_data/LFS1_K2/aggregated_data_lfs",objects_to_save1,"_LA", ".Rda",sep=""))
    
    
  })

# Read all LFS aggreagated data, extract into a dataframe, add age groups and save a single element. ######
setwd("./input_data/LFS1_K2/aggregated_data_lfs")
patt.shp <- ".*Rda*"

data.shp <-list.files(pattern=patt.shp)

LA_IPUMS_LIST <- sapply(data.shp, function(x) mget(load(x)), simplify = TRUE) 

DF_LA_LFS <- data.table::rbindlist(LA_IPUMS_LIST)

DF_LA_LFS$AGE<-with(DF_LA_LFS,  ifelse(AGE==999, "999",
                                       ifelse(AGE<5, "0-4",
                                              ifelse(AGE<10, "5-9", 
                                                     ifelse(AGE<15, "10-14",
                                                            ifelse(AGE<20, "15-19",
                                                                   ifelse(AGE<25, "20-24",
                                                                          ifelse(AGE<30, "25-29",
                                                                                 ifelse(AGE<35, "30-34",
                                                                                        ifelse(AGE<40, "35-39",
                                                                                               ifelse(AGE<45, "40-44",
                                                                                                      ifelse(AGE<50, "45-49",
                                                                                                             ifelse(AGE<55, "50-54",
                                                                                                                    ifelse(AGE<60, "55-59",
                                                                                                                           ifelse(AGE<65, "60-64",
                                                                                                                                  ifelse(AGE<70, "65-69",
                                                                                                                                         ifelse(AGE<75, "70-74",
                                                                                                                                                ifelse(AGE<80, "75-79",
                                                                                                                                                       ifelse(AGE<85, "80-84",
                                                                                                                                                              ifelse(AGE<90, "85-89",
                                                                                                                                                                     ifelse(AGE<95, "90-94","95+")))))))))))))))))))))


save(DF_LA_LFS, file="./input_data/LFS_86.Rda")

