
# DESCPRITION: Evaluate share of NAs in each variable ####

setwd("G:\\Shared drives\\CORESIDENCE\\WP2_DATA\\2_1_Input Data\\CENSUS_IPUMS")
library(tidyverse)

load("ARG_1970_IPUMS.Rdata")
load("A1_IPUMS_MOTHERSIPUMS_list.Rdata")
load("G:/Shared drives/CORESIDENCE/WP2_DATA/2_1_Input Data/CENSUS_IPUMS/A1_IMPUS_MOTHERS/all_NAs_list.Rdata")

df_Nas <- lapply(IPUMS_list, function(df) {
  df<-df%>%
  select(colnames(df[colSums(is.na(df))>0]))%>%
  summarise_all(funs(sum(is.na(.))/length(df$COUNTRY)))%>%
  pivot_longer(cols = everything())%>%
  arrange(-value)%>%
  mutate(name=fct_reorder(name, value, .desc = FALSE))

  df$cat<-with(df, ifelse(value==1,"100%",
                                ifelse(value<0.01,"<1%",
                                          ifelse(value<0.03,"1-3%",
                                                 ifelse(value<0.05,"3-5%",
                                                        ifelse(value<0.1,"5-10%",
                                                               ifelse(value<0.25,"10-25%",
                                                                      ifelse(value<0.5,"25-50%",">50%"
                                                                      ))))))))
  df
})

df_base<-df

df_Nas2 <- lapply(df_Nas, function(df) {
  base<-as.data.frame(colnames(df_base))
  colnames(base)[1]<-"name"
  df<-as.data.frame(df)
  base <- data.frame(base, df[match(base[,"name"], df[,"name"]),c("cat")])
  colnames(base)[2]<-"variables"
  base[1]<-NULL
  base
})

dffinal <- do.call("cbind", df_Nas2)

isoc<-lapply(names(IPUMS_list), function(x) {
  df <- IPUMS_list[[x]]
  df_ISO_CODE<-unique(df$ISO_CODE)
  df_YEAR<-unique(df$YEAR)
  df_iso<-paste(df_ISO_CODE,df_YEAR, sep="_")
  df_iso
})

finaliso <- do.call("rbind", isoc)


base[2]<-NULL
colnames(dffinal)<-finaliso
df_Nas_final<-cbind(base,dffinal)
colnames(final_ana)[1]<-"originales"

df_Nas_final[is.na(df_Nas_final)] <- "0%"
write.csv(df_Nas_final, file="samples_NAs.csv")
