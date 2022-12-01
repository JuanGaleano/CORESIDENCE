
library(foreign)
library(Hmisc)
library(haven)
library(tidyverse)
library(readr)
library(xlsx)
library(dplyr)
library(reshape)

setwd("G:\\Unidades compartidas\\CORESIDENCE\\WP2_DATA\\2_1_Input Data\\DHS_ORIGINALS")

originales<-paste("D:\\Google Drive\\DATA\\originals\\DHS\\DHS_originals_Nov2022\\spss\\")

files_info<-read.xlsx("lista_dhs.xlsx", sheetIndex=1,colIndex=1:10,header=T)

var<-c("HHID","HVIDX","HV000","HV001","HV002","HV003","HV005","HV006","HV007","HV009","HV010","HV011","HV012","HV013","HV014","HV024","HV025",
       "HV026","HV217","HV218","HV219","HV220","SHDISTR","HV101","HV102","HV103","HV104","HV105","HV106","HV107","HV108","HV109","HV110","HV111",
       "HV112","HV113","HV114","HV115","HV116","HV134","HV135","HV136","SH07AY","HA51")

######files_nomissing<-files_info[which(files_info$missing_var!=1),]

n_samples1<-length(files_nomissing$country)

n_samples<-length(files_info$country)

for (i in (1:n_samples)){
  df_file<-files_info$file[i]
  year<-files_info$year[i]
  ISO<-files_info$ISO_CODE[i]
  df<-read_sav(paste(originales,df_file, sep=""))
  df<-as.data.frame(df)
  df<-df%>%select(any_of(var))
  df$country_control<-ISO
  df$year_control<-year
  save(x = df, file = paste(ISO,"_",year,"_DHS", ".RData", sep = ""))
}
