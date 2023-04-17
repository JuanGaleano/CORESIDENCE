
# LIBRARIES AND FUNCTIONS ####
`%notin%` <- Negate(`%in%`)
library(tidyverse)
library(tidylog)
library(haven)



# IPUMS CORE LIST #####
   
setwd("G:\\Shared drives\\CORESIDENCE\\WP2_DATA\\2_4_CORESIDENCE_DATABASE\\CORESIDENCE_IPUMS_HARMO") 
  
patt.shp <- ".*RData*"
  
data.shp <-list.files(pattern=patt.shp)
  
CORE_LIST <- sapply(data.shp, function(x) mget(load(x)), simplify = TRUE)   



# STEP 1 LIST ######
VALID_GQ<-c(10,99)

PASO1<-lapply(CORE_LIST, function(df) {
  df<-df %>%
    filter(GQ %in% VALID_GQ) %>% #filtrem només els private households
    add_count(SERIAL)%>%
    mutate(child5=ifelse(AGE<=4,1,0),
           adult18=ifelse(AGE>=18,1,0),
           adult18M=ifelse((AGE>=18&SEX==1),1,0),
           adult18F=ifelse((AGE>=18&SEX==2),1,0),
           child18=ifelse(AGE<18,1,0),
           elderly65=ifelse(AGE>=65,1,0),
           
           age00_09=ifelse((AGE2=="0-4"|AGE2=="5-9"),1,0),
           age10_19=ifelse((AGE2=="10-14"|AGE2=="15-19"),1,0),
           age20_29=ifelse((AGE2=="20-24"|AGE2=="25-29"),1,0),
           age30_39=ifelse((AGE2=="30-34"|AGE2=="35-39"),1,0),
           age40_49=ifelse((AGE2=="40-44"|AGE2=="45-49"),1,0),
           age50_59=ifelse((AGE2=="50-54"|AGE2=="55-59"),1,0),
           age60_69=ifelse((AGE2=="60-64"|AGE2=="65-69"),1,0),
           age70_79=ifelse((AGE2=="70-74"|AGE2=="75-79"),1,0),
           age80=ifelse(AGE>=80,1,0),
           
           heads=ifelse(RELATE==1,1,0), 
           spouses=ifelse(RELATE==2,1,0),
           childs=ifelse(RELATE==3,1,0),
           other_relatives=ifelse(RELATE==4,1,0),
           non_relatives=ifelse(RELATE==5,1,0),
           other_rel_or_non_rel=ifelse(RELATE==6,1,0),
           
           grandchilds=ifelse(RELATED==4100,1,0),
           parents=ifelse(RELATED==4200,1,0),
           siblings=ifelse(RELATED==4400,1,0),
           
           WEIGHT_POP= sum(PWEIGHT,na.rm = TRUE)*1,
           #WEIGHT_A= sum(df[df$RELATE==1,"PWEIGHT_A"]),
           WEIGHT_A= sum(PWEIGHT[RELATE==1]),
           WEIGHT_A2= sum(PWEIGHT[RELATE==1&n==2]),
           WEIGHT_A3= sum(PWEIGHT[RELATE==1&n==3]),
           WEIGHT_A4= sum(PWEIGHT[RELATE==1&n==4]),
           WEIGHT_A5= sum(PWEIGHT[RELATE==1&n==5]),
           WEIGHT_AM= sum(PWEIGHT[RELATE==1&SEX==1]),
           WEIGHT_AF= sum(PWEIGHT[RELATE==1&SEX==2]),
           
           M_headed=ifelse((RELATE==1 & SEX==1),1,0),
           F_headed=ifelse((RELATE==1 & SEX==2),1,0))
})

rm(CORE_LIST)
gc()
# STEP 2 LIST #######  
PASO2<-lapply(PASO1, function(df) {  
  df<-df%>%
    group_by(SERIAL)%>%
    summarise(hh_per1=n(),
              hh_parents=sum(parents*PWEIGHT),
              hh_grandchilds=sum(grandchilds*PWEIGHT),
              hh_heads=sum(heads*PWEIGHT),
              hh_spouses=sum(spouses*PWEIGHT),
              hh_childs=sum(childs*PWEIGHT),
              hh_other_relatives=sum(other_relatives*PWEIGHT),
              hh_non_relatives=sum(non_relatives*PWEIGHT),
              hh_other_rel_or_non_rel=sum(other_rel_or_non_rel*PWEIGHT))%>%
    mutate(family_type= ifelse(hh_per1==1, 10,
                               ifelse(hh_spouses>0 & hh_childs>0 & hh_other_relatives ==0 & hh_non_relatives==0,20,
                                      ifelse(hh_spouses>0 & hh_childs==0 & hh_other_relatives ==0 & hh_non_relatives==0,20,
                                             ifelse(hh_spouses==0 & hh_childs>0 & hh_other_relatives ==0 & hh_non_relatives==0,20,
                                                    
                                                    ifelse(hh_spouses>0 & hh_childs>0 &  hh_other_relatives >0 & hh_non_relatives==0,31,
                                                           ifelse(hh_spouses>0 & hh_childs==0 & hh_other_relatives >0 & hh_non_relatives==0,31,
                                                                  ifelse(hh_spouses==0 & hh_childs>0 & hh_other_relatives >0 & hh_non_relatives==0,31,
                                                                         
                                                                         ifelse(hh_spouses>0 & hh_childs>0 &  hh_other_relatives ==0 & hh_non_relatives>0,32,
                                                                                ifelse(hh_spouses>0 & hh_childs==0 & hh_other_relatives ==0 & hh_non_relatives>0,32,
                                                                                       ifelse(hh_spouses==0 & hh_childs>0 & hh_other_relatives ==0 & hh_non_relatives>0,32,       
                                                                                              
                                                                                              ifelse(hh_spouses>0 & hh_childs>0 &  hh_other_relatives >0 & hh_non_relatives>0,33,
                                                                                                     ifelse(hh_spouses>0 & hh_childs==0 & hh_other_relatives >0 & hh_non_relatives>0,33,
                                                                                                            ifelse(hh_spouses==0 & hh_childs>0 & hh_other_relatives >0 & hh_non_relatives>0,33,        
                                                                                                                   
                                                                                                                   ifelse(hh_spouses==0 & hh_childs==0 &  hh_other_relatives >0 & hh_non_relatives==0,34,
                                                                                                                          ifelse(hh_spouses==0 & hh_childs==0 &  hh_other_relatives ==0 & hh_non_relatives>0,35,
                                                                                                                                 ifelse(hh_spouses==0 & hh_childs==0 &  hh_other_relatives >0 & hh_non_relatives>0,36,
                                                                                                                                        0)))))))))))))))))%>%
    mutate(family_type2=ifelse(hh_per1==1, 10,
                               ifelse(hh_spouses>0 & hh_childs>0 & hh_other_relatives ==0 & hh_non_relatives==0,20,
                                      ifelse(hh_spouses>0 & hh_childs==0 & hh_other_relatives ==0 & hh_non_relatives==0,20,
                                             ifelse(hh_spouses==0 & hh_childs>0 & hh_other_relatives ==0 & hh_non_relatives==0,20,       
                                                    ifelse(hh_non_relatives>0,30, # "Other non family",
                                                           ifelse((hh_parents>0 | hh_grandchilds>0),40,# "Stem family",
                                                                  ifelse(hh_other_relatives>0,50, 9999)))))))) #"Other family", 
  
  
  df<-df%>%select(SERIAL,family_type,family_type2)             
  df
})

# STEP 3 LIST ####
PASO3<- map2(PASO1, PASO2, ~left_join(.x, .y, by = 'SERIAL'))

# STEP 4 LIST ####
auxlist<-lapply(PASO3, function(df) {
  
  df<-df %>%
    group_by(S3H)%>%
    summarise(WEIGHT_POP= sum(PWEIGHT,na.rm = TRUE)*1,
              #WEIGHT_A= sum(df[df$RELATE==1,"PWEIGHT_A"]),
              WEIGHT_A= sum(PWEIGHT[RELATE==1]),
              WEIGHT_A2= sum(PWEIGHT[RELATE==1&n==2]),
              WEIGHT_A3= sum(PWEIGHT[RELATE==1&n==3]),
              WEIGHT_A4= sum(PWEIGHT[RELATE==1&n==4]),
              WEIGHT_A5= sum(PWEIGHT[RELATE==1&n==5]),
              WEIGHT_AM= sum(PWEIGHT[RELATE==1&SEX==1]),
              WEIGHT_AF= sum(PWEIGHT[RELATE==1&SEX==2]),
              
              WEIGHT_FT10= sum(PWEIGHT[RELATE==1&family_type==10]),
              WEIGHT_FT20= sum(PWEIGHT[RELATE==1&family_type==20]),
              WEIGHT_FT31= sum(PWEIGHT[RELATE==1&family_type==31]),
              WEIGHT_FT32= sum(PWEIGHT[RELATE==1&family_type==32]),
              WEIGHT_FT33= sum(PWEIGHT[RELATE==1&family_type==33]),
              WEIGHT_FT34= sum(PWEIGHT[RELATE==1&family_type==34]),
              WEIGHT_FT35= sum(PWEIGHT[RELATE==1&family_type==35]),
              WEIGHT_FT36= sum(PWEIGHT[RELATE==1&family_type==36]),
              WEIGHT_FT00= sum(PWEIGHT[RELATE==1&family_type==0]),
              
              WEIGHT_FT210= sum(PWEIGHT[RELATE==1&family_type2==10]),
              WEIGHT_FT220= sum(PWEIGHT[RELATE==1&family_type2==20]),
              WEIGHT_FT230= sum(PWEIGHT[RELATE==1&family_type2==30]),
              WEIGHT_FT240= sum(PWEIGHT[RELATE==1&family_type2==40]),
              WEIGHT_FT250= sum(PWEIGHT[RELATE==1&family_type2==50]),
              WEIGHT_FT299= sum(PWEIGHT[RELATE==1&family_type2==9999]))
  df
})

rm(PASO1)
# STEP 5 LIST #####
PASO4<-lapply(PASO3, function(df) {
  df<-df %>% ##### STEP 4 #######
  group_by(S3H,SAMPLE,CNTRY,YEAR,SERIAL,CONTINENT,CNTRY_ISO,SOURCE,CENSUS_ROUND,DECADE,FIVE_YEAR,CONTINENT2)%>%
    summarise(hh=1,
              hh_per1=n(),
              hh_m=sum(M_headed),
              hh_f=sum(F_headed),
              hh_per=sum(hh*PWEIGHT),
              hh_child5=sum(child5*PWEIGHT),
              hh_adult18=sum(adult18*PWEIGHT),
              
              hh_00_09=sum(age00_09*PWEIGHT),
              hh_10_19=sum(age10_19*PWEIGHT),
              hh_20_29=sum(age20_29*PWEIGHT),
              hh_30_39=sum(age30_39*PWEIGHT),
              hh_40_49=sum(age40_49*PWEIGHT),
              hh_50_59=sum(age50_59*PWEIGHT),
              hh_60_69=sum(age60_69*PWEIGHT),
              hh_70_79=sum(age70_79*PWEIGHT),
              hh_80=sum(age80*PWEIGHT),
              
              hh_adult18M=sum(adult18M*PWEIGHT),
              hh_adult18F=sum(adult18F*PWEIGHT),
              
              hh_child18=sum(child18*PWEIGHT),
              hh_elderly65=sum(elderly65*PWEIGHT),
              
              hh_heads=sum(heads*PWEIGHT),
              hh_spouses=sum(spouses*PWEIGHT),
              hh_childs=sum(childs*PWEIGHT),
              hh_other_relatives=sum(other_relatives*PWEIGHT),
              hh_non_relatives=sum(non_relatives*PWEIGHT),
              hh_other_rel_or_non_rel=sum(other_rel_or_non_rel*PWEIGHT),
              
              hh_grandchilds=sum(grandchilds*PWEIGHT),
              hh_parents=sum(parents*PWEIGHT),
              hh_siblings=sum(siblings*PWEIGHT), 
              hh_M_headed=sum(M_headed*PWEIGHT),
              
              hh_F_headed=sum(F_headed*PWEIGHT))%>%
    mutate(family_type= ifelse(hh_per1==1, 10,
                               ifelse(hh_spouses>0 & hh_childs>0 & hh_other_relatives ==0 & hh_non_relatives==0,20,
                                      ifelse(hh_spouses>0 & hh_childs==0 & hh_other_relatives ==0 & hh_non_relatives==0,20,
                                             ifelse(hh_spouses==0 & hh_childs>0 & hh_other_relatives ==0 & hh_non_relatives==0,20,
                                                    
                                                    ifelse(hh_spouses>0 & hh_childs>0 &  hh_other_relatives >0 & hh_non_relatives==0,31,
                                                           ifelse(hh_spouses>0 & hh_childs==0 & hh_other_relatives >0 & hh_non_relatives==0,31,
                                                                  ifelse(hh_spouses==0 & hh_childs>0 & hh_other_relatives >0 & hh_non_relatives==0,31,
                                                                         
                                                                         ifelse(hh_spouses>0 & hh_childs>0 &  hh_other_relatives ==0 & hh_non_relatives>0,32,
                                                                                ifelse(hh_spouses>0 & hh_childs==0 & hh_other_relatives ==0 & hh_non_relatives>0,32,
                                                                                       ifelse(hh_spouses==0 & hh_childs>0 & hh_other_relatives ==0 & hh_non_relatives>0,32,       
                                                                                              
                                                                                              ifelse(hh_spouses>0 & hh_childs>0 &  hh_other_relatives >0 & hh_non_relatives>0,33,
                                                                                                     ifelse(hh_spouses>0 & hh_childs==0 & hh_other_relatives >0 & hh_non_relatives>0,33,
                                                                                                            ifelse(hh_spouses==0 & hh_childs>0 & hh_other_relatives >0 & hh_non_relatives>0,33,        
                                                                                                                   
                                                                                                                   ifelse(hh_spouses==0 & hh_childs==0 &  hh_other_relatives >0 & hh_non_relatives==0,34,
                                                                                                                          ifelse(hh_spouses==0 & hh_childs==0 &  hh_other_relatives ==0 & hh_non_relatives>0,35,
                                                                                                                                 ifelse(hh_spouses==0 & hh_childs==0 &  hh_other_relatives >0 & hh_non_relatives>0,36,
                                                                                                                                        0)))))))))))))))))%>%
    mutate(family_type2=ifelse(hh_per1==1, 10,
                               ifelse(hh_spouses>0 & hh_childs>0 & hh_other_relatives ==0 & hh_non_relatives==0,20,
                                      ifelse(hh_spouses>0 & hh_childs==0 & hh_other_relatives ==0 & hh_non_relatives==0,20,
                                             ifelse(hh_spouses==0 & hh_childs>0 & hh_other_relatives ==0 & hh_non_relatives==0,20,       
                                                    ifelse(hh_non_relatives>0,30, # "Other non family",
                                                           ifelse((hh_parents>0 | hh_grandchilds>0),40,# "Stem family",
                                                                  ifelse(hh_other_relatives>0,50, 9999))))))))
  
  
  
  df
})



# STEP 6 LIST  #####
final<-Map(left_join, PASO4,auxlist, by="S3H")
gc()

# STEP 7 LIST ######
agrr_database_list<-lapply(final, function(df) {
  
  df<-df %>%
    group_by(S3H,CNTRY,CNTRY_ISO,YEAR,SOURCE,SAMPLE,CONTINENT,CONTINENT2,CENSUS_ROUND,DECADE,FIVE_YEAR)%>%
    summarise(WPOP=labelled(x=round(unique(WEIGHT_POP),0),label="Weighted population in sample"),
              
              HS01=labelled(x=sum(ifelse(hh_per1==1,hh,0))/sum(hh),label="Proportion of 1-person households"),
              HS02=labelled(x=sum(ifelse(hh_per1==2,hh,0))/sum(hh),label="Proportion of 2-person households"),
              HS03=labelled(x=sum(ifelse(hh_per1==3,hh,0))/sum(hh),label="Proportion of 3-person households"),
              HS04=labelled(x=sum(ifelse(hh_per1==4,hh,0))/sum(hh),label="Proportion of 4-person households"),
              HS05=labelled(x=sum(ifelse(hh_per1==5,hh,0))/sum(hh),label="Proportion of 5-person households"),
              HS06=labelled(x=sum(ifelse(hh_per1==6,hh,0))/sum(hh),label="Proportion of 6-person households"),
              HS07=labelled(x=sum(ifelse(hh_per1==7,hh,0))/sum(hh),label="Proportion of 7-person households"),
              HS08=labelled(x=sum(ifelse(hh_per1==8,hh,0))/sum(hh),label="Proportion of 8-person households"),
              HS09=labelled(x=sum(ifelse(hh_per1==9,hh,0))/sum(hh),label="Proportion of 9-person households"),
              HS10=labelled(x=sum(ifelse(hh_per1==10,hh,0))/sum(hh),label="Proportion of 10-person households"),
              HS11=labelled(x=sum(ifelse(hh_per1>10,hh,0))/sum(hh),label="Proportion of 11+person households"),
              
              HS12=labelled(x=sum(hh_child5 != 0)/sum(hh),label="Proportion of households with at least one person below 0-4 years old"),
              HS13=labelled(x=sum(hh_elderly65 != 0)/sum(hh),label="Proportion of households with at least one person 65+"),
              
              HS14=labelled(x=sum(hh_per)/unique(WEIGHT_A),label="Average household size"),
              HS15=labelled(x=sum(hh_child5)/unique(WEIGHT_A),label="Average number of 0-4 children in the household"),
              HS16=labelled(x=sum(hh_adult18)/unique(WEIGHT_A),label="Average number of adults in the household (aged 18+)"),
              HS17=labelled(x=sum(hh_child18)/unique(WEIGHT_A),label="Average number of children in the household (aged < 18)"),
              HS18=labelled(x=sum(hh_elderly65)/unique(WEIGHT_A), label="Average number of elderly people in the household (aged >65)"),
              
              HS19=labelled(x=sum(hh_00_09)/unique(WEIGHT_A),label="Average number of 0-9 individuals in the household"),
              HS20=labelled(x=sum(hh_10_19)/unique(WEIGHT_A),label="Average number of 10-19 individuals in the household"),
              HS21=labelled(x=sum(hh_20_29)/unique(WEIGHT_A),label="Average number of 20-29 individuals in the household"),
              HS22=labelled(x=sum(hh_30_39)/unique(WEIGHT_A),label="Average number of 30-39 individuals in the household"),
              HS23=labelled(x=sum(hh_40_49)/unique(WEIGHT_A),label="Average number of 40-49 individuals in the household"),
              HS24=labelled(x=sum(hh_50_59)/unique(WEIGHT_A),label="Average number of 50-59 individuals in the household"),
              HS25=labelled(x=sum(hh_60_69)/unique(WEIGHT_A),label="Average number of 60-69 individuals in the household"),
              HS26=labelled(x=sum(hh_70_79)/unique(WEIGHT_A),label="Average number of 70-79 individuals in the household"),
              HS27=labelled(x=sum(hh_80)/unique(WEIGHT_A),label="Average number of 80+ individuals in the household"),
              
              HS28=labelled(x=sum(ifelse((hh_per1==2|hh_per1==3),hh,0))/sum(hh),label="Proportion of 2-3 persons households"),
              HS29=labelled(x=sum(ifelse((hh_per1==4|hh_per1==5),hh,0))/sum(hh),label="Proportion of 4-5 persons households"),
              HS30=labelled(x=sum(ifelse(hh_per1>6,hh,0))/sum(hh),label="Proportion of 6+ person households"),
              
              HR01=labelled(x=sum(hh_heads)/unique(WEIGHT_A),label="Average number of Heads in the household"),
              HR02=labelled(x=sum(hh_spouses)/unique(WEIGHT_A),label="Average number of Spouses in the household"),
              HR03=labelled(x=sum(hh_childs)/unique(WEIGHT_A),label="Average number of Children in the household"),
              HR04=labelled(x=sum(hh_other_relatives)/unique(WEIGHT_A),label="Average number of other relatives in the household"),
              HR05=labelled(x=sum(hh_non_relatives)/unique(WEIGHT_A),label="Average number of non relatives in the household"),
              HR06=labelled(x=sum(hh_other_rel_or_non_rel)/unique(WEIGHT_A),label="Average number of relative or non relatives in the household"),
              
              HR07=labelled(x=sum(hh_heads[hh_per1==2])/unique(WEIGHT_A2) ,label="Average number of Heads in 2 persons household"),
              HR08=labelled(x=sum(hh_spouses[hh_per1==2])/unique(WEIGHT_A2) ,label="Average number of Spouses in 2 persons household"),
              HR09=labelled(x=sum(hh_childs[hh_per1==2])/unique(WEIGHT_A2) ,label="Average number of Children in 2 persons household"),
              HR10=labelled(x=sum(hh_other_relatives[hh_per1==2])/unique(WEIGHT_A2) ,label="Average number of Other relatives in 2 persons household"),
              HR11=labelled(x=sum(hh_non_relatives[hh_per1==2])/unique(WEIGHT_A2) ,label="Average number of non relatives in 2 persons household"),
              HR12=labelled(x=sum(hh_other_rel_or_non_rel[hh_per1==2])/unique(WEIGHT_A2) ,label="Average number of of relative or non relatives in 2 persons household"),
              
              HR13=labelled(x=sum(hh_heads[hh_per1==3])/unique(WEIGHT_A3) ,label="Average number of Heads in 3 persons household"),
              HR14=labelled(x=sum(hh_spouses[hh_per1==3])/unique(WEIGHT_A3) ,label="Average number of Spouses in 3 persons household"),
              HR15=labelled(x=sum(hh_childs[hh_per1==3])/unique(WEIGHT_A3) ,label="Average number of Children in 3 persons household"),
              HR16=labelled(x=sum(hh_other_relatives[hh_per1==3])/unique(WEIGHT_A3) ,label="Average number of Other relatives in 3 persons household"),
              HR17=labelled(x=sum(hh_non_relatives[hh_per1==3])/unique(WEIGHT_A3) ,label="Average number of non relatives in 3 persons household"),
              HR18=labelled(x=sum(hh_other_rel_or_non_rel[hh_per1==3])/unique(WEIGHT_A3) ,label="Average number of of relative or non relatives in 3 persons household"),
              
              HR19=labelled(x=sum(hh_heads[hh_per1==4])/unique(WEIGHT_A4) ,label="Average number of Heads in 4 persons household"),
              HR20=labelled(x=sum(hh_spouses[hh_per1==4])/unique(WEIGHT_A4) ,label="Average number of Spouses in 4 persons household"),
              HR21=labelled(x=sum(hh_childs[hh_per1==4])/unique(WEIGHT_A4) ,label="Average number of Children in 4 persons household"),
              HR22=labelled(x=sum(hh_other_relatives[hh_per1==4])/unique(WEIGHT_A4) ,label="Average number of Other relatives in 4 persons household"),
              HR23=labelled(x=sum(hh_non_relatives[hh_per1==4])/unique(WEIGHT_A4) ,label="Average number of non relatives in 4 persons household"),
              HR24=labelled(x=sum(hh_other_rel_or_non_rel[hh_per1==4])/unique(WEIGHT_A4) ,label="Average number of of relative or non relatives in 4 persons household"),
              
              HR25=labelled(x=sum(hh_heads[hh_per1==5])/unique(WEIGHT_A5) ,label="Average number of Heads in 5 persons household"),
              HR26=labelled(x=sum(hh_spouses[hh_per1==5])/unique(WEIGHT_A5) ,label="Average number of Spouses in 5 persons household"),
              HR27=labelled(x=sum(hh_childs[hh_per1==5])/unique(WEIGHT_A5) ,label="Average number of Children in 5 persons household"),
              HR28=labelled(x=sum(hh_other_relatives[hh_per1==5])/unique(WEIGHT_A5) ,label="Average number of Other relatives in 5 persons household"),
              HR29=labelled(x=sum(hh_non_relatives[hh_per1==5])/unique(WEIGHT_A5) ,label="Average number of non relatives in 5 persons household"),
              HR30=labelled(x=sum(hh_other_rel_or_non_rel[hh_per1==5])/unique(WEIGHT_A5) ,label="Average number of of relative or non relatives in 5 persons household"),
              
              HT01=labelled(x=sum(ifelse(family_type==10,1,0)/sum(hh)),label="Proportion of unipersonal households based on relationship to the head"),
              HT02=labelled(x=sum(ifelse(family_type==20,1,0)/sum(hh)),label="Proportion of nuclear households based on relationship to the head"),
              HT03=labelled(x=sum(ifelse(family_type==31,1,0)/sum(hh)),label="Proportion of nuclear plus other relative households based on relationship to the head"),
              HT04=labelled(x=sum(ifelse(family_type==32,1,0)/sum(hh)),label="Proportion of nuclear plus other non relative households based on relationship to the head"),
              HT05=labelled(x=sum(ifelse(family_type==33,1,0)/sum(hh)),label="Proportion of nuclear plus other relative plus other non relative households based on relationship to the head"),
              HT06=labelled(x=sum(ifelse(family_type==34,1,0)/sum(hh)),label="Proportion of other relative households based on relationship to the head"),
              HT07=labelled(x=sum(ifelse(family_type==35,1,0)/sum(hh)),label="Proportion of other non relative households based on relationship to the head"),
              HT08=labelled(x=sum(ifelse(family_type==36,1,0)/sum(hh)),label="Proportion of other relative plus other non relative households based on relationship to the head"),
              HT09=labelled(x=sum(ifelse(family_type==0,1,0)/sum(hh)),label="Proportion of other relative or non relative households based on relationship to the head"),
              
              HT10=labelled(x=sum(hh_per[family_type==10])/unique(WEIGHT_FT10),label="Average size of unipersonal households based on relationship to the head"),
              HT11=labelled(x=sum(hh_per[family_type==20])/unique(WEIGHT_FT20),label="Average size of nuclear households based on relationship to the head"),
              HT12=labelled(x=sum(hh_per[family_type==31])/unique(WEIGHT_FT31),label="Average size of nuclear plus other relative households based on relationship to the head"),
              HT13=labelled(x=sum(hh_per[family_type==32])/unique(WEIGHT_FT32),label="Average size of nuclear plus other non relative households based on relationship to the head"),
              HT14=labelled(x=sum(hh_per[family_type==33])/unique(WEIGHT_FT33),label="Average size of nuclear plus other relative plus other non relative households based on relationship to the head"),
              HT15=labelled(x=sum(hh_per[family_type==34])/unique(WEIGHT_FT34),label="Average size of other relative households based on relationship to the head"),
              HT16=labelled(x=sum(hh_per[family_type==35])/unique(WEIGHT_FT35),label="Average size of other non relative households based on relationship to the head"),
              HT17=labelled(x=sum(hh_per[family_type==36])/unique(WEIGHT_FT36),label="Average size of other relative plus other non relative households based on relationship to the head"),
              HT18=labelled(x=sum(hh_per[family_type==0])/unique(WEIGHT_FT00),label="Average size of other relative or non relative households based on relationship to the head"),
              
              HT20=labelled(x=sum(ifelse(family_type2==10,1,0)/sum(hh)),label="Proportion of unipersonal households based on relationship to the head"),
              HT21=labelled(x=sum(ifelse(family_type2==20,1,0)/sum(hh)),label="Proportion of nuclear households based on relationship to the head"),
              HT22=labelled(x=sum(ifelse(family_type2==40,1,0)/sum(hh)),label="Proportion of stem-family households based on relationship to the head"),
              HT23=labelled(x=sum(ifelse(family_type2==50,1,0)/sum(hh)),label="Proportion of Other family households based on relationship to the head"),
              HT24=labelled(x=sum(ifelse(family_type2==30,1,0)/sum(hh)),label="Proportion of Other non family households based on relationship to the head"),
              HT25=labelled(x=sum(ifelse(family_type2==9999,1,0)/sum(hh)),label="Proportion of other relative or non relative households based on relationship to the head"),
              
              HT26=labelled(x=sum(hh_per[family_type2==10])/unique(WEIGHT_FT210),label="Average household size of unipersonal based on relationship to the head"),
              HT27=labelled(x=sum(hh_per[family_type2==20])/unique(WEIGHT_FT220),label="Average household size of nuclear family based on relationship to the head"),
              HT28=labelled(x=sum(hh_per[family_type2==40])/unique(WEIGHT_FT240),label="Average household size of Stem family based on relationship to the head"),
              HT29=labelled(x=sum(hh_per[family_type2==50])/unique(WEIGHT_FT250),label="Average household size of Other family based on relationship to the head"),
              HT30=labelled(x=sum(hh_per[family_type2==30])/unique(WEIGHT_FT230),label="Average household size of Other non family based on relationship to the head"),
              HT31=labelled(x=sum(hh_per[family_type2==9999])/unique(WEIGHT_FT299),label="Average household size of other relative or non relative based on relationship to the head"),
              
              HH01=labelled(x=sum(hh_M_headed[hh_M_headed!=0])/unique(WEIGHT_A),label="Proportion of male-headed households"),
              HH02=labelled(x=sum(hh_F_headed[hh_F_headed!=0])/unique(WEIGHT_A),label="Proportion of female-headed households"),
              
              HH03=labelled(x=sum(ifelse((hh_m==1 &hh_per1==1),hh,0))/sum(hh_m),label="Proportion of 1-person households of male-headed households"),
              HH04=labelled(x=sum(ifelse((hh_m==1 &hh_per1==2),hh,0))/sum(hh_m),label="Proportion of 2-person households of male-headed households"),
              HH05=labelled(x=sum(ifelse((hh_m==1 &hh_per1==3),hh,0))/sum(hh_m),label="Proportion of 3-person households of male-headed households"),
              HH06=labelled(x=sum(ifelse((hh_m==1 &hh_per1==4),hh,0))/sum(hh_m),label="Proportion of 4-person households of male-headed households"),
              HH07=labelled(x=sum(ifelse((hh_m==1 &hh_per1==5),hh,0))/sum(hh_m),label="Proportion of 5-person households of male-headed households"),
              HH08=labelled(x=sum(ifelse((hh_m==1 &hh_per1==6),hh,0))/sum(hh_m),label="Proportion of 6-person households of male-headed households"),
              HH09=labelled(x=sum(ifelse((hh_m==1 &hh_per1==7),hh,0))/sum(hh_m),label="Proportion of 7-person households of male-headed households"),
              HH10=labelled(x=sum(ifelse((hh_m==1 &hh_per1==8),hh,0))/sum(hh_m),label="Proportion of 8-person households of male-headed households"),
              HH11=labelled(x=sum(ifelse((hh_m==1 &hh_per1==9),hh,0))/sum(hh_m),label="Proportion of 9-person households of male-headed households"),
              HH12=labelled(x=sum(ifelse((hh_m==1 &hh_per1==10),hh,0))/sum(hh_m),label="Proportion of 10-person households of male-headed households"),
              HH13=labelled(x=sum(ifelse((hh_m==1 &hh_per1>10),hh,0))/sum(hh_m),label="Proportion of 11+person households of male-headed households"),
              
              HH14=labelled(x=sum(ifelse((hh_f==1 &hh_per1==1),hh,0))/sum(hh_f),label="Proportion of 1-person households of female-headed households"),
              HH15=labelled(x=sum(ifelse((hh_f==1 &hh_per1==2),hh,0))/sum(hh_f),label="Proportion of 2-person households of female-headed households"),
              HH16=labelled(x=sum(ifelse((hh_f==1 &hh_per1==3),hh,0))/sum(hh_f),label="Proportion of 3-person households of female-headed households"),
              HH17=labelled(x=sum(ifelse((hh_f==1 &hh_per1==4),hh,0))/sum(hh_f),label="Proportion of 4-person households of female-headed households"),
              HH18=labelled(x=sum(ifelse((hh_f==1 &hh_per1==5),hh,0))/sum(hh_f),label="Proportion of 5-person households of female-headed households"),
              HH19=labelled(x=sum(ifelse((hh_f==1 &hh_per1==6),hh,0))/sum(hh_f),label="Proportion of 6-person households of female-headed households"),
              HH20=labelled(x=sum(ifelse((hh_f==1 &hh_per1==7),hh,0))/sum(hh_f),label="Proportion of 7-person households of female-headed households"),
              HH21=labelled(x=sum(ifelse((hh_f==1 &hh_per1==8),hh,0))/sum(hh_f),label="Proportion of 8-person households of female-headed households"),
              HH22=labelled(x=sum(ifelse((hh_f==1 &hh_per1==9),hh,0))/sum(hh_f),label="Proportion of 9-person households of female-headed households"),
              HH23=labelled(x=sum(ifelse((hh_f==1 &hh_per1==10),hh,0))/sum(hh_f),label="Proportion of 10-person households of female-headed households"),
              HH24=labelled(x=sum(ifelse((hh_f==1 &hh_per1>10),hh,0))/sum(hh_f),label="Proportion of 11+person households of female-headed households"),
              
              HH25=labelled(x=sum(ifelse((hh_m==1 &family_type2==10),1,0)/sum(hh_m)),label="Proportion of unipersonal households based on relationship to the head of male-headed households"),
              HH26=labelled(x=sum(ifelse((hh_m==1 &family_type2==20),1,0)/sum(hh_m)),label="Proportion of nuclear households based on relationship to the head of male-headed households"),
              HH27=labelled(x=sum(ifelse((hh_m==1 &family_type2==40),1,0)/sum(hh_m)),label="Proportion of stem-family households based on relationship to the head of male-headed households"),
              HH28=labelled(x=sum(ifelse((hh_m==1 &family_type2==50),1,0)/sum(hh_m)),label="Proportion of Other family households based on relationship to the head of male-headed households"),
              HH29=labelled(x=sum(ifelse((hh_m==1 &family_type2==30),1,0)/sum(hh_m)),label="Proportion of Other non family households based on relationship to the head of male-headed households"),
              HH30=labelled(x=sum(ifelse((hh_m==1 &family_type2==9999),1,0)/sum(hh_m)),label="Proportion of other relative or non relative households based on relationship to the head of male-headed households"),
              
              HH31=labelled(x=sum(ifelse((hh_f==1 &family_type2==10),1,0)/sum(hh_f)),label="Proportion of unipersonal households based on relationship to the head of female-headed households"),
              HH32=labelled(x=sum(ifelse((hh_f==1 &family_type2==20),1,0)/sum(hh_f)),label="Proportion of nuclear households based on relationship to the head of frmale-headed households"),
              HH33=labelled(x=sum(ifelse((hh_f==1 &family_type2==40),1,0)/sum(hh_f)),label="Proportion of stem-family households based on relationship to the head of female-headed households"),
              HH34=labelled(x=sum(ifelse((hh_f==1 &family_type2==50),1,0)/sum(hh_f)),label="Proportion of Other family households based on relationship to the head of female-headed households"),
              HH35=labelled(x=sum(ifelse((hh_f==1 &family_type2==30),1,0)/sum(hh_f)),label="Proportion of Other non family households based on relationship to the head of female-headed households"),
              HH36=labelled(x=sum(ifelse((hh_f==1 &family_type2==9999),1,0)/sum(hh_f)),label="Proportion of other relative or non relative households based on relationship to the head of female-headed households"),
              
              HH37=labelled(x=sum(hh_per[hh_M_headed!=0])/unique(WEIGHT_AM),label="Average household size of male-headed households"),
              HH38=labelled(x=sum(hh_per[hh_F_headed!=0])/unique(WEIGHT_AF),label="Average household size of female-headed households"),
              HH39=labelled(x=sum(hh_child18[hh_M_headed!=0])/unique(WEIGHT_AM),label="Average number of children of male-headed households"),
              HH40=labelled(x=sum(hh_child18[hh_F_headed!=0])/unique(WEIGHT_AF),label="Average number of children of female-headed households"),
              HH41=labelled(x=sum(hh_adult18[hh_M_headed!=0])/unique(WEIGHT_AM),label="Average number of adults of male-headed households"),
              HH42=labelled(x=sum(hh_adult18[hh_F_headed!=0])/unique(WEIGHT_AF),label="Average number of adults of female-headed households"),
              HH43=labelled(x=sum(hh_elderly65[hh_M_headed!=0])/unique(WEIGHT_AM),label="Average number of elderly people (aged >65) of male-headed households"),
              HH44=labelled(x=sum(hh_elderly65[hh_F_headed!=0])/unique(WEIGHT_AF),label="Average number of elderly people (aged >65) of female-headed households"),
              
              HH45=labelled(x=sum(hh_spouses[hh_M_headed!=0])/unique(WEIGHT_AM),label="Average number of spouses of male-headed households"),
              HH46=labelled(x=sum(hh_spouses[hh_F_headed!=0])/unique(WEIGHT_AF),label="Average number of spouses of female-headed households"),
              HH47=labelled(x=sum(hh_childs[hh_M_headed!=0])/unique(WEIGHT_AM),label="Average number of childs of male-headed households"),
              HH48=labelled(x=sum(hh_childs[hh_F_headed!=0])/unique(WEIGHT_AF),label="Average number of childs of female-headed households"),
              HH49=labelled(x=sum(hh_other_relatives[hh_M_headed!=0])/unique(WEIGHT_AM),label="Average number of other relatives of male-headed households"),
              HH50=labelled(x=sum(hh_other_relatives[hh_F_headed!=0])/unique(WEIGHT_AF),label="Average number of other relatives of female-headed households"),
              HH51=labelled(x=sum(hh_non_relatives[hh_M_headed!=0])/unique(WEIGHT_AM),label="Average number of other non relatives of male-headed households"),
              HH52=labelled(x=sum(hh_non_relatives[hh_F_headed!=0])/unique(WEIGHT_AF),label="Average number of other non relatives of female-headed households"),
              
              HH53=labelled(x=sum(hh_adult18M[hh_M_headed!=0])/unique(WEIGHT_AM),label="Average number of males adults in male-headed households"),
              HH54=labelled(x=sum(hh_adult18M[hh_F_headed!=0])/unique(WEIGHT_AF),label="Average number of males adults in female-headed households"),
              HH55=labelled(x=sum(hh_adult18F[hh_M_headed!=0])/unique(WEIGHT_AM),label="Average number of female adults in male-headed households"),
              HH56=labelled(x=sum(hh_adult18F[hh_F_headed!=0])/unique(WEIGHT_AF),label="Average number of female adults in female-headed households")
    )
  
})

# STEP 8 LIST #####

agrr_database_ipums <- do.call("rbind", agrr_database_list)

clipr::write_clip(agrr_database_ipums)

agrr_database_ipums_long<-agrr_database_ipums%>%
  pivot_longer(c(WPOP:HH56), names_to = "indicator", values_to = "value") 


save(agrr_database_ipums, file="G:\\Shared drives\\CORESIDENCE\\TEAM FOLDERS\\Juan Galeano\\AGGR_INDICATORS_IPUMS_SUBNATIONAL_HARMONIZED.RData")
save(agrr_database_ipums_long, file="G:\\Shared drives\\CORESIDENCE\\TEAM FOLDERS\\Juan Galeano\\AGGR_INDICATORS_IPUMS_SUBNATIONAL_HARMONIZED_long.RData")

agrr_database_ipums_long$CNTRY<-as_factor(agrr_database_ipums_long$CNTRY)
library(writexl)
write_xlsx(agrr_database_ipums_long,"G:\\Shared drives\\CORESIDENCE\\TEAM FOLDERS\\Juan Galeano\\AGGR_INDICATORS_IPUMS_SUBNATIONAL_long_new.xlsx")

gc()

#

# DHS #####
# READ CODES REGIONS #####

library(readxl)
regionw <- read_excel("C:/Users/jgaleano/Downloads/regionw.xlsx")


# CORE LIST DHS #####

setwd("G:\\Shared drives\\CORESIDENCE\\WP2_DATA\\2_4_CORESIDENCE_DATABASE\\CORESIDENCE_DHS_HARMO\\")

patt.shp <- ".*RData*"

data.shp <-list.files(pattern=patt.shp)

CORE_DHS_LIST <- sapply(data.shp, function(x) mget(load(x)), simplify = TRUE)   

# STEP 1 LIST ######

PASO1<-lapply(CORE_DHS_LIST, function(df) {
  
  df$HV104[is.na(df$HV104)] <- 3 #(NA, missing)
  df<-df %>%
    # filter(GQ %in% VALID_GQ) %>% #filtrem només els private households
    add_count(HHID)%>%
    mutate(child5=ifelse(AGE<=4,1,0),
           adult18=ifelse(AGE>=18,1,0),
           adult18M=ifelse((AGE>=18&HV104==1),1,0),
           adult18F=ifelse((AGE>=18&HV104==2),1,0),
           child18=ifelse(AGE<18,1,0),
           elderly65=ifelse(AGE>=65,1,0),
           
           age00_09=ifelse((AGE2=="0-4"|AGE2=="5-9"),1,0),
           age10_19=ifelse((AGE2=="10-14"|AGE2=="15-19"),1,0),
           age20_29=ifelse((AGE2=="20-24"|AGE2=="25-29"),1,0),
           age30_39=ifelse((AGE2=="30-34"|AGE2=="35-39"),1,0),
           age40_49=ifelse((AGE2=="40-44"|AGE2=="45-49"),1,0),
           age50_59=ifelse((AGE2=="50-54"|AGE2=="55-59"),1,0),
           age60_69=ifelse((AGE2=="60-64"|AGE2=="65-69"),1,0),
           age70_79=ifelse((AGE2=="70-74"|AGE2=="75-79"),1,0),
           age80=ifelse(AGE>=80,1,0),
           
           heads=ifelse(RELATE==1,1,0), 
           spouses=ifelse(RELATE==2,1,0),
           childs=ifelse(RELATE==3,1,0),
           other_relatives=ifelse(RELATE==4,1,0),
           non_relatives=ifelse(RELATE==5,1,0),
           other_rel_or_non_rel=ifelse(RELATE==6,1,0),
           
           grandchilds=ifelse(RELATED==4100,1,0),
           parents=ifelse(RELATED==4200,1,0),
           siblings=ifelse(RELATED==4400,1,0),
           
           WEIGHT_POP= sum(PWEIGHT_A,na.rm = TRUE)*1,
           #WEIGHT_A= sum(df[df$RELATE==1,"PWEIGHT_A"]),
           WEIGHT_A= sum(PWEIGHT_A[RELATE==1]),
           WEIGHT_A2= sum(PWEIGHT_A[RELATE==1&n==2]),
           WEIGHT_A3= sum(PWEIGHT_A[RELATE==1&n==3]),
           WEIGHT_A4= sum(PWEIGHT_A[RELATE==1&n==4]),
           WEIGHT_A5= sum(PWEIGHT_A[RELATE==1&n==5]),
           WEIGHT_AM= sum(PWEIGHT_A[RELATE==1&HV104==1]),
           WEIGHT_AF= sum(PWEIGHT_A[RELATE==1&HV104==2]),
           
           M_headed=ifelse((RELATE==1 & HV104==1),1,0),
           F_headed=ifelse((RELATE==1 & HV104==2),1,0))
})

rm(CORE_DHS_LIST)
gc()

# STEP 2 LIST #######  
PASO2<-lapply(PASO1, function(df) {  
  df<-df%>%
    group_by(HHID)%>%
    summarise(hh_per1=n(),
              hh_parents=sum(parents*PWEIGHT_A),
              hh_grandchilds=sum(grandchilds*PWEIGHT_A),
              hh_heads=sum(heads*PWEIGHT_A),
              hh_spouses=sum(spouses*PWEIGHT_A),
              hh_childs=sum(childs*PWEIGHT_A),
              hh_other_relatives=sum(other_relatives*PWEIGHT_A),
              hh_non_relatives=sum(non_relatives*PWEIGHT_A),
              hh_other_rel_or_non_rel=sum(other_rel_or_non_rel*PWEIGHT_A))%>%
    mutate(family_type= ifelse(hh_per1==1, 10,
                               ifelse(hh_spouses>0 & hh_childs>0 & hh_other_relatives ==0 & hh_non_relatives==0,20,
                                      ifelse(hh_spouses>0 & hh_childs==0 & hh_other_relatives ==0 & hh_non_relatives==0,20,
                                             ifelse(hh_spouses==0 & hh_childs>0 & hh_other_relatives ==0 & hh_non_relatives==0,20,
                                                    
                                                    ifelse(hh_spouses>0 & hh_childs>0 &  hh_other_relatives >0 & hh_non_relatives==0,31,
                                                           ifelse(hh_spouses>0 & hh_childs==0 & hh_other_relatives >0 & hh_non_relatives==0,31,
                                                                  ifelse(hh_spouses==0 & hh_childs>0 & hh_other_relatives >0 & hh_non_relatives==0,31,
                                                                         
                                                                         ifelse(hh_spouses>0 & hh_childs>0 &  hh_other_relatives ==0 & hh_non_relatives>0,32,
                                                                                ifelse(hh_spouses>0 & hh_childs==0 & hh_other_relatives ==0 & hh_non_relatives>0,32,
                                                                                       ifelse(hh_spouses==0 & hh_childs>0 & hh_other_relatives ==0 & hh_non_relatives>0,32,       
                                                                                              
                                                                                              ifelse(hh_spouses>0 & hh_childs>0 &  hh_other_relatives >0 & hh_non_relatives>0,33,
                                                                                                     ifelse(hh_spouses>0 & hh_childs==0 & hh_other_relatives >0 & hh_non_relatives>0,33,
                                                                                                            ifelse(hh_spouses==0 & hh_childs>0 & hh_other_relatives >0 & hh_non_relatives>0,33,        
                                                                                                                   
                                                                                                                   ifelse(hh_spouses==0 & hh_childs==0 &  hh_other_relatives >0 & hh_non_relatives==0,34,
                                                                                                                          ifelse(hh_spouses==0 & hh_childs==0 &  hh_other_relatives ==0 & hh_non_relatives>0,35,
                                                                                                                                 ifelse(hh_spouses==0 & hh_childs==0 &  hh_other_relatives >0 & hh_non_relatives>0,36,
                                                                                                                                        0)))))))))))))))))%>%
    mutate(family_type2=ifelse(hh_per1==1, 10,
                               ifelse(hh_spouses>0 & hh_childs>0 & hh_other_relatives ==0 & hh_non_relatives==0,20,
                                      ifelse(hh_spouses>0 & hh_childs==0 & hh_other_relatives ==0 & hh_non_relatives==0,20,
                                             ifelse(hh_spouses==0 & hh_childs>0 & hh_other_relatives ==0 & hh_non_relatives==0,20,       
                                                    ifelse(hh_non_relatives>0,30, # "Other non family",
                                                           ifelse((hh_parents>0 | hh_grandchilds>0),40,# "Stem family",
                                                                  ifelse(hh_other_relatives>0,50, 9999)))))))) #"Other family", 
  
  
  df<-df%>%select(HHID,family_type,family_type2)             
  df
})

# STEP 3 LIST ####
PASO3<- map2(PASO1, PASO2, ~left_join(.x, .y, by = 'HHID'))

# STEP 4 LIST ####
auxlist<-lapply(PASO3, function(df) {
  
  df<-df %>%
    group_by(S3H)%>%
    summarise(WEIGHT_POP= sum(PWEIGHT_A,na.rm = TRUE)*1,
              #WEIGHT_A= sum(df[df$RELATE==1,"PWEIGHT_A"]),
              WEIGHT_A= sum(PWEIGHT_A[RELATE==1]),
              WEIGHT_A2= sum(PWEIGHT_A[RELATE==1&n==2]),
              WEIGHT_A3= sum(PWEIGHT_A[RELATE==1&n==3]),
              WEIGHT_A4= sum(PWEIGHT_A[RELATE==1&n==4]),
              WEIGHT_A5= sum(PWEIGHT_A[RELATE==1&n==5]),
              WEIGHT_AM= sum(PWEIGHT_A[RELATE==1&HV104==1]),
              WEIGHT_AF= sum(PWEIGHT_A[RELATE==1&HV104==2]),
              
              WEIGHT_FT10= sum(PWEIGHT_A[RELATE==1&family_type==10]),
              WEIGHT_FT20= sum(PWEIGHT_A[RELATE==1&family_type==20]),
              WEIGHT_FT31= sum(PWEIGHT_A[RELATE==1&family_type==31]),
              WEIGHT_FT32= sum(PWEIGHT_A[RELATE==1&family_type==32]),
              WEIGHT_FT33= sum(PWEIGHT_A[RELATE==1&family_type==33]),
              WEIGHT_FT34= sum(PWEIGHT_A[RELATE==1&family_type==34]),
              WEIGHT_FT35= sum(PWEIGHT_A[RELATE==1&family_type==35]),
              WEIGHT_FT36= sum(PWEIGHT_A[RELATE==1&family_type==36]),
              WEIGHT_FT00= sum(PWEIGHT_A[RELATE==1&family_type==0]),
              
              WEIGHT_FT210= sum(PWEIGHT_A[RELATE==1&family_type2==10]),
              WEIGHT_FT220= sum(PWEIGHT_A[RELATE==1&family_type2==20]),
              WEIGHT_FT230= sum(PWEIGHT_A[RELATE==1&family_type2==30]),
              WEIGHT_FT240= sum(PWEIGHT_A[RELATE==1&family_type2==40]),
              WEIGHT_FT250= sum(PWEIGHT_A[RELATE==1&family_type2==50]),
              WEIGHT_FT299= sum(PWEIGHT_A[RELATE==1&family_type2==9999]))
  df
})

# STEP 5 LIST #####
PASO4<-lapply(PASO3, function(df) {
  df<-df %>% ##### STEP 4 #######
  group_by(S3H, SAMPLE,country_control,YEAR,HHID,CONTINENT,SOURCE,CENSUS_ROUND,DECADE,FIVE_YEAR,CONT2)%>%
    summarise(hh=1,
              hh_per1=n(),
              hh_m=sum(M_headed),
              hh_f=sum(F_headed),
              hh_per=sum(hh*PWEIGHT_A),
              hh_child5=sum(child5*PWEIGHT_A),
              hh_adult18=sum(adult18*PWEIGHT_A),
              
              hh_00_09=sum(age00_09*PWEIGHT_A),
              hh_10_19=sum(age10_19*PWEIGHT_A),
              hh_20_29=sum(age20_29*PWEIGHT_A),
              hh_30_39=sum(age30_39*PWEIGHT_A),
              hh_40_49=sum(age40_49*PWEIGHT_A),
              hh_50_59=sum(age50_59*PWEIGHT_A),
              hh_60_69=sum(age60_69*PWEIGHT_A),
              hh_70_79=sum(age70_79*PWEIGHT_A),
              hh_80=sum(age80*PWEIGHT_A),
              
              hh_adult18M=sum(adult18M*PWEIGHT_A),
              hh_adult18F=sum(adult18F*PWEIGHT_A),
              
              hh_child18=sum(child18*PWEIGHT_A),
              hh_elderly65=sum(elderly65*PWEIGHT_A),
              
              hh_heads=sum(heads*PWEIGHT_A),
              hh_spouses=sum(spouses*PWEIGHT_A),
              hh_childs=sum(childs*PWEIGHT_A),
              hh_other_relatives=sum(other_relatives*PWEIGHT_A),
              hh_non_relatives=sum(non_relatives*PWEIGHT_A),
              hh_other_rel_or_non_rel=sum(other_rel_or_non_rel*PWEIGHT_A),
              
              hh_grandchilds=sum(grandchilds*PWEIGHT_A),
              hh_parents=sum(parents*PWEIGHT_A),
              hh_siblings=sum(siblings*PWEIGHT_A), 
              hh_M_headed=sum(M_headed*PWEIGHT_A),
              
              hh_F_headed=sum(F_headed*PWEIGHT_A))%>%
    mutate(family_type= ifelse(hh_per1==1, 10,
                               ifelse(hh_spouses>0 & hh_childs>0 & hh_other_relatives ==0 & hh_non_relatives==0,20,
                                      ifelse(hh_spouses>0 & hh_childs==0 & hh_other_relatives ==0 & hh_non_relatives==0,20,
                                             ifelse(hh_spouses==0 & hh_childs>0 & hh_other_relatives ==0 & hh_non_relatives==0,20,
                                                    
                                                    ifelse(hh_spouses>0 & hh_childs>0 &  hh_other_relatives >0 & hh_non_relatives==0,31,
                                                           ifelse(hh_spouses>0 & hh_childs==0 & hh_other_relatives >0 & hh_non_relatives==0,31,
                                                                  ifelse(hh_spouses==0 & hh_childs>0 & hh_other_relatives >0 & hh_non_relatives==0,31,
                                                                         
                                                                         ifelse(hh_spouses>0 & hh_childs>0 &  hh_other_relatives ==0 & hh_non_relatives>0,32,
                                                                                ifelse(hh_spouses>0 & hh_childs==0 & hh_other_relatives ==0 & hh_non_relatives>0,32,
                                                                                       ifelse(hh_spouses==0 & hh_childs>0 & hh_other_relatives ==0 & hh_non_relatives>0,32,       
                                                                                              
                                                                                              ifelse(hh_spouses>0 & hh_childs>0 &  hh_other_relatives >0 & hh_non_relatives>0,33,
                                                                                                     ifelse(hh_spouses>0 & hh_childs==0 & hh_other_relatives >0 & hh_non_relatives>0,33,
                                                                                                            ifelse(hh_spouses==0 & hh_childs>0 & hh_other_relatives >0 & hh_non_relatives>0,33,        
                                                                                                                   
                                                                                                                   ifelse(hh_spouses==0 & hh_childs==0 &  hh_other_relatives >0 & hh_non_relatives==0,34,
                                                                                                                          ifelse(hh_spouses==0 & hh_childs==0 &  hh_other_relatives ==0 & hh_non_relatives>0,35,
                                                                                                                                 ifelse(hh_spouses==0 & hh_childs==0 &  hh_other_relatives >0 & hh_non_relatives>0,36,
                                                                                                                                        0)))))))))))))))))%>%
    mutate(family_type2=ifelse(hh_per1==1, 10,
                               ifelse(hh_spouses>0 & hh_childs>0 & hh_other_relatives ==0 & hh_non_relatives==0,20,
                                      ifelse(hh_spouses>0 & hh_childs==0 & hh_other_relatives ==0 & hh_non_relatives==0,20,
                                             ifelse(hh_spouses==0 & hh_childs>0 & hh_other_relatives ==0 & hh_non_relatives==0,20,       
                                                    ifelse(hh_non_relatives>0,30, # "Other non family",
                                                           ifelse((hh_parents>0 | hh_grandchilds>0),40,# "Stem family",
                                                                  ifelse(hh_other_relatives>0,50, 9999))))))))
  
  
  
  df
})

# STEP 6 LIST  #####
final<-Map(left_join, PASO4,auxlist, by="S3H")
gc()

# STEP 7 LIST ######
agrr_database_list<-lapply(final, function(df) {
  
  df<-df %>%
    group_by(S3H,country_control,YEAR,SOURCE,SAMPLE,CONTINENT,CONT2,CENSUS_ROUND,DECADE,FIVE_YEAR)%>%
    summarise(WPOP=labelled(x=round(unique(WEIGHT_POP),0),label="Weighted population in sample"),
              
              HS01=labelled(x=sum(ifelse(hh_per1==1,hh,0))/sum(hh),label="Proportion of 1-person households"),
              HS02=labelled(x=sum(ifelse(hh_per1==2,hh,0))/sum(hh),label="Proportion of 2-person households"),
              HS03=labelled(x=sum(ifelse(hh_per1==3,hh,0))/sum(hh),label="Proportion of 3-person households"),
              HS04=labelled(x=sum(ifelse(hh_per1==4,hh,0))/sum(hh),label="Proportion of 4-person households"),
              HS05=labelled(x=sum(ifelse(hh_per1==5,hh,0))/sum(hh),label="Proportion of 5-person households"),
              HS06=labelled(x=sum(ifelse(hh_per1==6,hh,0))/sum(hh),label="Proportion of 6-person households"),
              HS07=labelled(x=sum(ifelse(hh_per1==7,hh,0))/sum(hh),label="Proportion of 7-person households"),
              HS08=labelled(x=sum(ifelse(hh_per1==8,hh,0))/sum(hh),label="Proportion of 8-person households"),
              HS09=labelled(x=sum(ifelse(hh_per1==9,hh,0))/sum(hh),label="Proportion of 9-person households"),
              HS10=labelled(x=sum(ifelse(hh_per1==10,hh,0))/sum(hh),label="Proportion of 10-person households"),
              HS11=labelled(x=sum(ifelse(hh_per1>10,hh,0))/sum(hh),label="Proportion of 11+person households"),
              
              HS12=labelled(x=sum(hh_child5 != 0)/sum(hh),label="Proportion of households with at least one person below 0-4 years old"),
              HS13=labelled(x=sum(hh_elderly65 != 0)/sum(hh),label="Proportion of households with at least one person 65+"),
              
              HS14=labelled(x=sum(hh_per)/unique(WEIGHT_A),label="Average household size"),
              HS15=labelled(x=sum(hh_child5)/unique(WEIGHT_A),label="Average number of 0-4 children in the household"),
              HS16=labelled(x=sum(hh_adult18)/unique(WEIGHT_A),label="Average number of adults in the household (aged 18+)"),
              HS17=labelled(x=sum(hh_child18)/unique(WEIGHT_A),label="Average number of children in the household (aged < 18)"),
              HS18=labelled(x=sum(hh_elderly65)/unique(WEIGHT_A), label="Average number of elderly people in the household (aged >65)"),
              
              HS19=labelled(x=sum(hh_00_09)/unique(WEIGHT_A),label="Average number of 0-9 individuals in the household"),
              HS20=labelled(x=sum(hh_10_19)/unique(WEIGHT_A),label="Average number of 10-19 individuals in the household"),
              HS21=labelled(x=sum(hh_20_29)/unique(WEIGHT_A),label="Average number of 20-29 individuals in the household"),
              HS22=labelled(x=sum(hh_30_39)/unique(WEIGHT_A),label="Average number of 30-39 individuals in the household"),
              HS23=labelled(x=sum(hh_40_49)/unique(WEIGHT_A),label="Average number of 40-49 individuals in the household"),
              HS24=labelled(x=sum(hh_50_59)/unique(WEIGHT_A),label="Average number of 50-59 individuals in the household"),
              HS25=labelled(x=sum(hh_60_69)/unique(WEIGHT_A),label="Average number of 60-69 individuals in the household"),
              HS26=labelled(x=sum(hh_70_79)/unique(WEIGHT_A),label="Average number of 70-79 individuals in the household"),
              HS27=labelled(x=sum(hh_80)/unique(WEIGHT_A),label="Average number of 80+ individuals in the household"),
              
              HS28=labelled(x=sum(ifelse((hh_per1==2|hh_per1==3),hh,0))/sum(hh),label="Proportion of 2-3 persons households"),
              HS29=labelled(x=sum(ifelse((hh_per1==4|hh_per1==5),hh,0))/sum(hh),label="Proportion of 4-5 persons households"),
              HS30=labelled(x=sum(ifelse(hh_per1>6,hh,0))/sum(hh),label="Proportion of 6+ person households"),
              
              HR01=labelled(x=sum(hh_heads)/unique(WEIGHT_A),label="Average number of Heads in the household"),
              HR02=labelled(x=sum(hh_spouses)/unique(WEIGHT_A),label="Average number of Spouses in the household"),
              HR03=labelled(x=sum(hh_childs)/unique(WEIGHT_A),label="Average number of Children in the household"),
              HR04=labelled(x=sum(hh_other_relatives)/unique(WEIGHT_A),label="Average number of other relatives in the household"),
              HR05=labelled(x=sum(hh_non_relatives)/unique(WEIGHT_A),label="Average number of non relatives in the household"),
              HR06=labelled(x=sum(hh_other_rel_or_non_rel)/unique(WEIGHT_A),label="Average number of relative or non relatives in the household"),
              
              HR07=labelled(x=sum(hh_heads[hh_per1==2])/unique(WEIGHT_A2) ,label="Average number of Heads in 2 persons household"),
              HR08=labelled(x=sum(hh_spouses[hh_per1==2])/unique(WEIGHT_A2) ,label="Average number of Spouses in 2 persons household"),
              HR09=labelled(x=sum(hh_childs[hh_per1==2])/unique(WEIGHT_A2) ,label="Average number of Children in 2 persons household"),
              HR10=labelled(x=sum(hh_other_relatives[hh_per1==2])/unique(WEIGHT_A2) ,label="Average number of Other relatives in 2 persons household"),
              HR11=labelled(x=sum(hh_non_relatives[hh_per1==2])/unique(WEIGHT_A2) ,label="Average number of non relatives in 2 persons household"),
              HR12=labelled(x=sum(hh_other_rel_or_non_rel[hh_per1==2])/unique(WEIGHT_A2) ,label="Average number of of relative or non relatives in 2 persons household"),
              
              HR13=labelled(x=sum(hh_heads[hh_per1==3])/unique(WEIGHT_A3) ,label="Average number of Heads in 3 persons household"),
              HR14=labelled(x=sum(hh_spouses[hh_per1==3])/unique(WEIGHT_A3) ,label="Average number of Spouses in 3 persons household"),
              HR15=labelled(x=sum(hh_childs[hh_per1==3])/unique(WEIGHT_A3) ,label="Average number of Children in 3 persons household"),
              HR16=labelled(x=sum(hh_other_relatives[hh_per1==3])/unique(WEIGHT_A3) ,label="Average number of Other relatives in 3 persons household"),
              HR17=labelled(x=sum(hh_non_relatives[hh_per1==3])/unique(WEIGHT_A3) ,label="Average number of non relatives in 3 persons household"),
              HR18=labelled(x=sum(hh_other_rel_or_non_rel[hh_per1==3])/unique(WEIGHT_A3) ,label="Average number of of relative or non relatives in 3 persons household"),
              
              HR19=labelled(x=sum(hh_heads[hh_per1==4])/unique(WEIGHT_A4) ,label="Average number of Heads in 4 persons household"),
              HR20=labelled(x=sum(hh_spouses[hh_per1==4])/unique(WEIGHT_A4) ,label="Average number of Spouses in 4 persons household"),
              HR21=labelled(x=sum(hh_childs[hh_per1==4])/unique(WEIGHT_A4) ,label="Average number of Children in 4 persons household"),
              HR22=labelled(x=sum(hh_other_relatives[hh_per1==4])/unique(WEIGHT_A4) ,label="Average number of Other relatives in 4 persons household"),
              HR23=labelled(x=sum(hh_non_relatives[hh_per1==4])/unique(WEIGHT_A4) ,label="Average number of non relatives in 4 persons household"),
              HR24=labelled(x=sum(hh_other_rel_or_non_rel[hh_per1==4])/unique(WEIGHT_A4) ,label="Average number of of relative or non relatives in 4 persons household"),
              
              HR25=labelled(x=sum(hh_heads[hh_per1==5])/unique(WEIGHT_A5) ,label="Average number of Heads in 5 persons household"),
              HR26=labelled(x=sum(hh_spouses[hh_per1==5])/unique(WEIGHT_A5) ,label="Average number of Spouses in 5 persons household"),
              HR27=labelled(x=sum(hh_childs[hh_per1==5])/unique(WEIGHT_A5) ,label="Average number of Children in 5 persons household"),
              HR28=labelled(x=sum(hh_other_relatives[hh_per1==5])/unique(WEIGHT_A5) ,label="Average number of Other relatives in 5 persons household"),
              HR29=labelled(x=sum(hh_non_relatives[hh_per1==5])/unique(WEIGHT_A5) ,label="Average number of non relatives in 5 persons household"),
              HR30=labelled(x=sum(hh_other_rel_or_non_rel[hh_per1==5])/unique(WEIGHT_A5) ,label="Average number of of relative or non relatives in 5 persons household"),
              
              HT01=labelled(x=sum(ifelse(family_type==10,1,0)/sum(hh)),label="Proportion of unipersonal households based on relationship to the head"),
              HT02=labelled(x=sum(ifelse(family_type==20,1,0)/sum(hh)),label="Proportion of nuclear households based on relationship to the head"),
              HT03=labelled(x=sum(ifelse(family_type==31,1,0)/sum(hh)),label="Proportion of nuclear plus other relative households based on relationship to the head"),
              HT04=labelled(x=sum(ifelse(family_type==32,1,0)/sum(hh)),label="Proportion of nuclear plus other non relative households based on relationship to the head"),
              HT05=labelled(x=sum(ifelse(family_type==33,1,0)/sum(hh)),label="Proportion of nuclear plus other relative plus other non relative households based on relationship to the head"),
              HT06=labelled(x=sum(ifelse(family_type==34,1,0)/sum(hh)),label="Proportion of other relative households based on relationship to the head"),
              HT07=labelled(x=sum(ifelse(family_type==35,1,0)/sum(hh)),label="Proportion of other non relative households based on relationship to the head"),
              HT08=labelled(x=sum(ifelse(family_type==36,1,0)/sum(hh)),label="Proportion of other relative plus other non relative households based on relationship to the head"),
              HT09=labelled(x=sum(ifelse(family_type==0,1,0)/sum(hh)),label="Proportion of other relative or non relative households based on relationship to the head"),
              
              HT10=labelled(x=sum(hh_per[family_type==10])/unique(WEIGHT_FT10),label="Average size of unipersonal households based on relationship to the head"),
              HT11=labelled(x=sum(hh_per[family_type==20])/unique(WEIGHT_FT20),label="Average size of nuclear households based on relationship to the head"),
              HT12=labelled(x=sum(hh_per[family_type==31])/unique(WEIGHT_FT31),label="Average size of nuclear plus other relative households based on relationship to the head"),
              HT13=labelled(x=sum(hh_per[family_type==32])/unique(WEIGHT_FT32),label="Average size of nuclear plus other non relative households based on relationship to the head"),
              HT14=labelled(x=sum(hh_per[family_type==33])/unique(WEIGHT_FT33),label="Average size of nuclear plus other relative plus other non relative households based on relationship to the head"),
              HT15=labelled(x=sum(hh_per[family_type==34])/unique(WEIGHT_FT34),label="Average size of other relative households based on relationship to the head"),
              HT16=labelled(x=sum(hh_per[family_type==35])/unique(WEIGHT_FT35),label="Average size of other non relative households based on relationship to the head"),
              HT17=labelled(x=sum(hh_per[family_type==36])/unique(WEIGHT_FT36),label="Average size of other relative plus other non relative households based on relationship to the head"),
              HT18=labelled(x=sum(hh_per[family_type==0])/unique(WEIGHT_FT00),label="Average size of other relative or non relative households based on relationship to the head"),
              
              HT20=labelled(x=sum(ifelse(family_type2==10,1,0)/sum(hh)),label="Proportion of unipersonal households based on relationship to the head"),
              HT21=labelled(x=sum(ifelse(family_type2==20,1,0)/sum(hh)),label="Proportion of nuclear households based on relationship to the head"),
              HT22=labelled(x=sum(ifelse(family_type2==40,1,0)/sum(hh)),label="Proportion of stem-family households based on relationship to the head"),
              HT23=labelled(x=sum(ifelse(family_type2==50,1,0)/sum(hh)),label="Proportion of Other family households based on relationship to the head"),
              HT24=labelled(x=sum(ifelse(family_type2==30,1,0)/sum(hh)),label="Proportion of Other non family households based on relationship to the head"),
              HT25=labelled(x=sum(ifelse(family_type2==9999,1,0)/sum(hh)),label="Proportion of other relative or non relative households based on relationship to the head"),
              
              HT26=labelled(x=sum(hh_per[family_type2==10])/unique(WEIGHT_FT210),label="Average household size of unipersonal based on relationship to the head"),
              HT27=labelled(x=sum(hh_per[family_type2==20])/unique(WEIGHT_FT220),label="Average household size of nuclear family based on relationship to the head"),
              HT28=labelled(x=sum(hh_per[family_type2==40])/unique(WEIGHT_FT240),label="Average household size of Stem family based on relationship to the head"),
              HT29=labelled(x=sum(hh_per[family_type2==50])/unique(WEIGHT_FT250),label="Average household size of Other family based on relationship to the head"),
              HT30=labelled(x=sum(hh_per[family_type2==30])/unique(WEIGHT_FT230),label="Average household size of Other non family based on relationship to the head"),
              HT31=labelled(x=sum(hh_per[family_type2==9999])/unique(WEIGHT_FT299),label="Average household size of other relative or non relative based on relationship to the head"),
              
              HH01=labelled(x=sum(hh_M_headed[hh_M_headed!=0])/unique(WEIGHT_A),label="Proportion of male-headed households"),
              HH02=labelled(x=sum(hh_F_headed[hh_F_headed!=0])/unique(WEIGHT_A),label="Proportion of female-headed households"),
              
              HH03=labelled(x=sum(ifelse((hh_m==1 &hh_per1==1),hh,0))/sum(hh_m),label="Proportion of 1-person households of male-headed households"),
              HH04=labelled(x=sum(ifelse((hh_m==1 &hh_per1==2),hh,0))/sum(hh_m),label="Proportion of 2-person households of male-headed households"),
              HH05=labelled(x=sum(ifelse((hh_m==1 &hh_per1==3),hh,0))/sum(hh_m),label="Proportion of 3-person households of male-headed households"),
              HH06=labelled(x=sum(ifelse((hh_m==1 &hh_per1==4),hh,0))/sum(hh_m),label="Proportion of 4-person households of male-headed households"),
              HH07=labelled(x=sum(ifelse((hh_m==1 &hh_per1==5),hh,0))/sum(hh_m),label="Proportion of 5-person households of male-headed households"),
              HH08=labelled(x=sum(ifelse((hh_m==1 &hh_per1==6),hh,0))/sum(hh_m),label="Proportion of 6-person households of male-headed households"),
              HH09=labelled(x=sum(ifelse((hh_m==1 &hh_per1==7),hh,0))/sum(hh_m),label="Proportion of 7-person households of male-headed households"),
              HH10=labelled(x=sum(ifelse((hh_m==1 &hh_per1==8),hh,0))/sum(hh_m),label="Proportion of 8-person households of male-headed households"),
              HH11=labelled(x=sum(ifelse((hh_m==1 &hh_per1==9),hh,0))/sum(hh_m),label="Proportion of 9-person households of male-headed households"),
              HH12=labelled(x=sum(ifelse((hh_m==1 &hh_per1==10),hh,0))/sum(hh_m),label="Proportion of 10-person households of male-headed households"),
              HH13=labelled(x=sum(ifelse((hh_m==1 &hh_per1>10),hh,0))/sum(hh_m),label="Proportion of 11+person households of male-headed households"),
              
              HH14=labelled(x=sum(ifelse((hh_f==1 &hh_per1==1),hh,0))/sum(hh_f),label="Proportion of 1-person households of female-headed households"),
              HH15=labelled(x=sum(ifelse((hh_f==1 &hh_per1==2),hh,0))/sum(hh_f),label="Proportion of 2-person households of female-headed households"),
              HH16=labelled(x=sum(ifelse((hh_f==1 &hh_per1==3),hh,0))/sum(hh_f),label="Proportion of 3-person households of female-headed households"),
              HH17=labelled(x=sum(ifelse((hh_f==1 &hh_per1==4),hh,0))/sum(hh_f),label="Proportion of 4-person households of female-headed households"),
              HH18=labelled(x=sum(ifelse((hh_f==1 &hh_per1==5),hh,0))/sum(hh_f),label="Proportion of 5-person households of female-headed households"),
              HH19=labelled(x=sum(ifelse((hh_f==1 &hh_per1==6),hh,0))/sum(hh_f),label="Proportion of 6-person households of female-headed households"),
              HH20=labelled(x=sum(ifelse((hh_f==1 &hh_per1==7),hh,0))/sum(hh_f),label="Proportion of 7-person households of female-headed households"),
              HH21=labelled(x=sum(ifelse((hh_f==1 &hh_per1==8),hh,0))/sum(hh_f),label="Proportion of 8-person households of female-headed households"),
              HH22=labelled(x=sum(ifelse((hh_f==1 &hh_per1==9),hh,0))/sum(hh_f),label="Proportion of 9-person households of female-headed households"),
              HH23=labelled(x=sum(ifelse((hh_f==1 &hh_per1==10),hh,0))/sum(hh_f),label="Proportion of 10-person households of female-headed households"),
              HH24=labelled(x=sum(ifelse((hh_f==1 &hh_per1>10),hh,0))/sum(hh_f),label="Proportion of 11+person households of female-headed households"),
              
              HH25=labelled(x=sum(ifelse((hh_m==1 &family_type2==10),1,0)/sum(hh_m)),label="Proportion of unipersonal households based on relationship to the head of male-headed households"),
              HH26=labelled(x=sum(ifelse((hh_m==1 &family_type2==20),1,0)/sum(hh_m)),label="Proportion of nuclear households based on relationship to the head of male-headed households"),
              HH27=labelled(x=sum(ifelse((hh_m==1 &family_type2==40),1,0)/sum(hh_m)),label="Proportion of stem-family households based on relationship to the head of male-headed households"),
              HH28=labelled(x=sum(ifelse((hh_m==1 &family_type2==50),1,0)/sum(hh_m)),label="Proportion of Other family households based on relationship to the head of male-headed households"),
              HH29=labelled(x=sum(ifelse((hh_m==1 &family_type2==30),1,0)/sum(hh_m)),label="Proportion of Other non family households based on relationship to the head of male-headed households"),
              HH30=labelled(x=sum(ifelse((hh_m==1 &family_type2==9999),1,0)/sum(hh_m)),label="Proportion of other relative or non relative households based on relationship to the head of male-headed households"),
              
              HH31=labelled(x=sum(ifelse((hh_f==1 &family_type2==10),1,0)/sum(hh_f)),label="Proportion of unipersonal households based on relationship to the head of female-headed households"),
              HH32=labelled(x=sum(ifelse((hh_f==1 &family_type2==20),1,0)/sum(hh_f)),label="Proportion of nuclear households based on relationship to the head of frmale-headed households"),
              HH33=labelled(x=sum(ifelse((hh_f==1 &family_type2==40),1,0)/sum(hh_f)),label="Proportion of stem-family households based on relationship to the head of female-headed households"),
              HH34=labelled(x=sum(ifelse((hh_f==1 &family_type2==50),1,0)/sum(hh_f)),label="Proportion of Other family households based on relationship to the head of female-headed households"),
              HH35=labelled(x=sum(ifelse((hh_f==1 &family_type2==30),1,0)/sum(hh_f)),label="Proportion of Other non family households based on relationship to the head of female-headed households"),
              HH36=labelled(x=sum(ifelse((hh_f==1 &family_type2==9999),1,0)/sum(hh_f)),label="Proportion of other relative or non relative households based on relationship to the head of female-headed households"),
              
              HH37=labelled(x=sum(hh_per[hh_M_headed!=0])/unique(WEIGHT_AM),label="Average household size of male-headed households"),
              HH38=labelled(x=sum(hh_per[hh_F_headed!=0])/unique(WEIGHT_AF),label="Average household size of female-headed households"),
              HH39=labelled(x=sum(hh_child18[hh_M_headed!=0])/unique(WEIGHT_AM),label="Average number of children of male-headed households"),
              HH40=labelled(x=sum(hh_child18[hh_F_headed!=0])/unique(WEIGHT_AF),label="Average number of children of female-headed households"),
              HH41=labelled(x=sum(hh_adult18[hh_M_headed!=0])/unique(WEIGHT_AM),label="Average number of adults of male-headed households"),
              HH42=labelled(x=sum(hh_adult18[hh_F_headed!=0])/unique(WEIGHT_AF),label="Average number of adults of female-headed households"),
              HH43=labelled(x=sum(hh_elderly65[hh_M_headed!=0])/unique(WEIGHT_AM),label="Average number of elderly people (aged >65) of male-headed households"),
              HH44=labelled(x=sum(hh_elderly65[hh_F_headed!=0])/unique(WEIGHT_AF),label="Average number of elderly people (aged >65) of female-headed households"),
              
              HH45=labelled(x=sum(hh_spouses[hh_M_headed!=0])/unique(WEIGHT_AM),label="Average number of spouses of male-headed households"),
              HH46=labelled(x=sum(hh_spouses[hh_F_headed!=0])/unique(WEIGHT_AF),label="Average number of spouses of female-headed households"),
              HH47=labelled(x=sum(hh_childs[hh_M_headed!=0])/unique(WEIGHT_AM),label="Average number of childs of male-headed households"),
              HH48=labelled(x=sum(hh_childs[hh_F_headed!=0])/unique(WEIGHT_AF),label="Average number of childs of female-headed households"),
              HH49=labelled(x=sum(hh_other_relatives[hh_M_headed!=0])/unique(WEIGHT_AM),label="Average number of other relatives of male-headed households"),
              HH50=labelled(x=sum(hh_other_relatives[hh_F_headed!=0])/unique(WEIGHT_AF),label="Average number of other relatives of female-headed households"),
              HH51=labelled(x=sum(hh_non_relatives[hh_M_headed!=0])/unique(WEIGHT_AM),label="Average number of other non relatives of male-headed households"),
              HH52=labelled(x=sum(hh_non_relatives[hh_F_headed!=0])/unique(WEIGHT_AF),label="Average number of other non relatives of female-headed households"),
              
              HH53=labelled(x=sum(hh_adult18M[hh_M_headed!=0])/unique(WEIGHT_AM),label="Average number of males adults in male-headed households"),
              HH54=labelled(x=sum(hh_adult18M[hh_F_headed!=0])/unique(WEIGHT_AF),label="Average number of males adults in female-headed households"),
              HH55=labelled(x=sum(hh_adult18F[hh_M_headed!=0])/unique(WEIGHT_AM),label="Average number of female adults in male-headed households"),
              HH56=labelled(x=sum(hh_adult18F[hh_F_headed!=0])/unique(WEIGHT_AF),label="Average number of female adults in female-headed households")
    )
  
})



# STEP 8  #####

agrr_database_dhs <- do.call("rbind", agrr_database_list)
as_factor(agrr_database_dhs$REGION_NAME)

regionw<-as.data.frame(regionw)
agrr_database_dhs<-as.data.frame(agrr_database_dhs)
agrr_database_dhs<- data.frame(agrr_database_dhs,
                               regionw[match(agrr_database_dhs[,"country_control"],
                                             regionw[,"ISO3"]),c("EU Member States")])

colnames(agrr_database_dhs)[grep(names(agrr_database_dhs %>% select(last_col())), colnames(agrr_database_dhs))]<-"CNTRY"

clipr::write_clip(agrr_database_dhs)


save(agrr_database_dhs, file="G:\\Shared drives\\CORESIDENCE\\TEAM FOLDERS\\Juan Galeano\\AGGR_INDICATORS_DHS_SUBNATIONAL_HARMONIZED.RData")

agrr_database_dhs_long<-agrr_database_dhs%>%
  pivot_longer(c(WPOP:HH56), names_to = "indicator", values_to = "value") 


library(writexl)
write_xlsx(agrr_database_dhs_long,"G:\\Shared drives\\CORESIDENCE\\TEAM FOLDERS\\Juan Galeano\\AGGR_INDICATORS_DHS_SUBNATIONAL_HARMONIZED_long.xlsx")

gc()





##### LFS #######
# READ CODES REGIONS #####
library(readxl)
regionw <- read_excel("C:/Users/jgaleano/Downloads/regionw.xlsx")

# CORE LIST LFS #####

setwd("G:\\Shared drives\\CORESIDENCE\\WP2_DATA\\2_4_CORESIDENCE_DATABASE\\CORESIDENCE_LFS_HARMO\\")

patt.shp <- ".*RData*"

data.shp <-list.files(pattern=patt.shp)

CORE_LFS_LIST <- sapply(data.shp, function(x) mget(load(x)), simplify = TRUE)  

# STEP 1 LIST ######
PASO1<-lapply(CORE_LFS_LIST, function(df) {
  df$SEX[is.na(df$SEX)] <- 3
  VALID_GQ<-1
  df<-df %>%
    filter(HHTYPE %in% VALID_GQ) %>% #filtrem només els private households
    mutate(hhid=paste(HHNUM,REFWEEK, sep="-"))%>%
    add_count(hhid)%>%
    
    mutate(#hhid=paste(HHNUM,REFWEEK, sep="-"),
      child5=ifelse(AGE<7,1,0),
      adult18=ifelse(AGE>17,1,0),
      adult18M=ifelse((AGE>17&SEX==1),1,0),
      adult18F=ifelse((AGE>17&SEX==2),1,0),
      child18=ifelse(AGE<=17,1,0),
      elderly65=ifelse(AGE>62,1,0),
      
      age00_09=ifelse((AGE==2|AGE==7),1,0),
      age10_19=ifelse((AGE==12|AGE==17),1,0),
      age20_29=ifelse((AGE==22|AGE==27),1,0),
      age30_39=ifelse((AGE==32|AGE==37),1,0),
      age40_49=ifelse((AGE==42|AGE==47),1,0),
      age50_59=ifelse((AGE==52|AGE==57),1,0),
      age60_69=ifelse((AGE==62|AGE==67),1,0),
      age70_79=ifelse((AGE==72|AGE==77),1,0),
      age80=ifelse(AGE>=82,1,0),
      
      heads=ifelse(RELATE==1,1,0), 
      spouses=ifelse(RELATE==2,1,0),
      childs=ifelse(RELATE==3,1,0),
      other_relatives=ifelse(RELATE==4,1,0),
      non_relatives=ifelse(RELATE==5,1,0),
      other_rel_or_non_rel=ifelse(RELATE==6,1,0),
      
      grandchilds=ifelse(RELATED==4100,1,0),
      parents=ifelse(RELATED==4200,1,0),
      siblings=ifelse(RELATED==4400,1,0),
      
      WEIGHT_POP= sum(COEFF,na.rm = TRUE)*1000,
      #WEIGHT_A= sum(df[df$RELATE==1,"PWEIGHT_A"]),
      WEIGHT_A= sum(COEFF[RELATE==1],na.rm = TRUE),
      WEIGHT_A2= sum(COEFF[RELATE==1&n==2]),
      WEIGHT_A3= sum(COEFF[RELATE==1&n==3]),
      WEIGHT_A4= sum(COEFF[RELATE==1&n==4]),
      WEIGHT_A5= sum(COEFF[RELATE==1&n==5]),
      WEIGHT_AM= sum(COEFF[RELATE==1&SEX==1]),
      WEIGHT_AF= sum(COEFF[RELATE==1&SEX==2]),
      
      M_headed=ifelse((RELATE==1 & SEX==1),1,0),
      F_headed=ifelse((RELATE==1 & SEX==2),1,0))  %>%
    drop_na(COEFF)
})
gc()

# STEP 2 LIST #######  
PASO2<-lapply(PASO1, function(df) {  
  df<-df%>%
    group_by(hhid)%>%
    summarise(hh_per1=n(),
              hh_parents=sum(parents*COEFF),
              hh_grandchilds=sum(grandchilds*COEFF),
              hh_heads=sum(heads*COEFF),
              hh_spouses=sum(spouses*COEFF),
              hh_childs=sum(childs*COEFF),
              hh_other_relatives=sum(other_relatives*COEFF),
              hh_non_relatives=sum(non_relatives*COEFF),
              hh_other_rel_or_non_rel=sum(other_rel_or_non_rel*COEFF))%>%
    mutate(family_type= ifelse(hh_per1==1, 10,
                               ifelse(hh_spouses>0 & hh_childs>0 & hh_other_relatives ==0 & hh_non_relatives==0,20,
                                      ifelse(hh_spouses>0 & hh_childs==0 & hh_other_relatives ==0 & hh_non_relatives==0,20,
                                             ifelse(hh_spouses==0 & hh_childs>0 & hh_other_relatives ==0 & hh_non_relatives==0,20,
                                                    
                                                    ifelse(hh_spouses>0 & hh_childs>0 &  hh_other_relatives >0 & hh_non_relatives==0,31,
                                                           ifelse(hh_spouses>0 & hh_childs==0 & hh_other_relatives >0 & hh_non_relatives==0,31,
                                                                  ifelse(hh_spouses==0 & hh_childs>0 & hh_other_relatives >0 & hh_non_relatives==0,31,
                                                                         
                                                                         ifelse(hh_spouses>0 & hh_childs>0 &  hh_other_relatives ==0 & hh_non_relatives>0,32,
                                                                                ifelse(hh_spouses>0 & hh_childs==0 & hh_other_relatives ==0 & hh_non_relatives>0,32,
                                                                                       ifelse(hh_spouses==0 & hh_childs>0 & hh_other_relatives ==0 & hh_non_relatives>0,32,       
                                                                                              
                                                                                              ifelse(hh_spouses>0 & hh_childs>0 &  hh_other_relatives >0 & hh_non_relatives>0,33,
                                                                                                     ifelse(hh_spouses>0 & hh_childs==0 & hh_other_relatives >0 & hh_non_relatives>0,33,
                                                                                                            ifelse(hh_spouses==0 & hh_childs>0 & hh_other_relatives >0 & hh_non_relatives>0,33,        
                                                                                                                   
                                                                                                                   ifelse(hh_spouses==0 & hh_childs==0 &  hh_other_relatives >0 & hh_non_relatives==0,34,
                                                                                                                          ifelse(hh_spouses==0 & hh_childs==0 &  hh_other_relatives ==0 & hh_non_relatives>0,35,
                                                                                                                                 ifelse(hh_spouses==0 & hh_childs==0 &  hh_other_relatives >0 & hh_non_relatives>0,36,
                                                                                                                                        0)))))))))))))))))%>%
    mutate(family_type2=ifelse(hh_per1==1, 10,
                               ifelse(hh_spouses>0 & hh_childs>0 & hh_other_relatives ==0 & hh_non_relatives==0,20,
                                      ifelse(hh_spouses>0 & hh_childs==0 & hh_other_relatives ==0 & hh_non_relatives==0,20,
                                             ifelse(hh_spouses==0 & hh_childs>0 & hh_other_relatives ==0 & hh_non_relatives==0,20,       
                                                    ifelse(hh_non_relatives>0,30, # "Other non family",
                                                           ifelse((hh_parents>0 | hh_grandchilds>0),40,# "Stem family",
                                                                  ifelse(hh_other_relatives>0,50, 9999)))))))) #"Other family", 
  
  
  df<-df%>%select(hhid,family_type,family_type2)             
  df
})

# STEP 3 LIST ####
PASO3<- map2(PASO1, PASO2, ~left_join(.x, .y, by = 'hhid'))

# STEP 4 LIST ####
auxlist<-lapply(PASO3, function(df) {
  
  df<-df %>%
    group_by(S3H)%>%
    summarise(WEIGHT_POP= sum(COEFF,na.rm = TRUE)*1,
              #WEIGHT_A= sum(df[df$RELATE==1,"PWEIGHT_A"]),
              WEIGHT_A= sum(COEFF[RELATE==1]),
              WEIGHT_A2= sum(COEFF[RELATE==1&n==2]),
              WEIGHT_A3= sum(COEFF[RELATE==1&n==3]),
              WEIGHT_A4= sum(COEFF[RELATE==1&n==4]),
              WEIGHT_A5= sum(COEFF[RELATE==1&n==5]),
              WEIGHT_AM= sum(COEFF[RELATE==1&SEX==1]),
              WEIGHT_AF= sum(COEFF[RELATE==1&SEX==2]),
              
              WEIGHT_FT10= sum(COEFF[RELATE==1&family_type==10]),
              WEIGHT_FT20= sum(COEFF[RELATE==1&family_type==20]),
              WEIGHT_FT31= sum(COEFF[RELATE==1&family_type==31]),
              WEIGHT_FT32= sum(COEFF[RELATE==1&family_type==32]),
              WEIGHT_FT33= sum(COEFF[RELATE==1&family_type==33]),
              WEIGHT_FT34= sum(COEFF[RELATE==1&family_type==34]),
              WEIGHT_FT35= sum(COEFF[RELATE==1&family_type==35]),
              WEIGHT_FT36= sum(COEFF[RELATE==1&family_type==36]),
              WEIGHT_FT00= sum(COEFF[RELATE==1&family_type==0]),
              
              WEIGHT_FT210= sum(COEFF[RELATE==1&family_type2==10]),
              WEIGHT_FT220= sum(COEFF[RELATE==1&family_type2==20]),
              WEIGHT_FT230= sum(COEFF[RELATE==1&family_type2==30]),
              WEIGHT_FT240= sum(COEFF[RELATE==1&family_type2==40]),
              WEIGHT_FT250= sum(COEFF[RELATE==1&family_type2==50]),
              WEIGHT_FT299= sum(COEFF[RELATE==1&family_type2==9999]))
  df
})

# STEP 5 LIST #####
PASO4<-lapply(PASO3, function(df) {
  df<-df %>% ##### STEP 4 #######
  group_by(S3H,SAMPLE,ISO3,YEAR,hhid,CONTINENT,SOURCE,CENSUS_ROUND,DECADE,FIVE_YEAR,CONT2)%>%
    summarise(hh=1,
              hh_per1=n(),
              hh_m=sum(M_headed),
              hh_f=sum(F_headed),
              hh_per=sum(hh*COEFF),
              hh_child5=sum(child5*COEFF),
              hh_adult18=sum(adult18*COEFF),
              
              hh_00_09=sum(age00_09*COEFF),
              hh_10_19=sum(age10_19*COEFF),
              hh_20_29=sum(age20_29*COEFF),
              hh_30_39=sum(age30_39*COEFF),
              hh_40_49=sum(age40_49*COEFF),
              hh_50_59=sum(age50_59*COEFF),
              hh_60_69=sum(age60_69*COEFF),
              hh_70_79=sum(age70_79*COEFF),
              hh_80=sum(age80*COEFF),
              
              hh_adult18M=sum(adult18M*COEFF),
              hh_adult18F=sum(adult18F*COEFF),
              
              hh_child18=sum(child18*COEFF),
              hh_elderly65=sum(elderly65*COEFF),
              
              hh_heads=sum(heads*COEFF),
              hh_spouses=sum(spouses*COEFF),
              hh_childs=sum(childs*COEFF),
              hh_other_relatives=sum(other_relatives*COEFF),
              hh_non_relatives=sum(non_relatives*COEFF),
              hh_other_rel_or_non_rel=sum(other_rel_or_non_rel*COEFF),
              
              hh_grandchilds=sum(grandchilds*COEFF),
              hh_parents=sum(parents*COEFF),
              hh_siblings=sum(siblings*COEFF), 
              hh_M_headed=sum(M_headed*COEFF),
              
              hh_F_headed=sum(F_headed*COEFF))%>%
    mutate(family_type= ifelse(hh_per1==1, 10,
                               ifelse(hh_spouses>0 & hh_childs>0 & hh_other_relatives ==0 & hh_non_relatives==0,20,
                                      ifelse(hh_spouses>0 & hh_childs==0 & hh_other_relatives ==0 & hh_non_relatives==0,20,
                                             ifelse(hh_spouses==0 & hh_childs>0 & hh_other_relatives ==0 & hh_non_relatives==0,20,
                                                    
                                                    ifelse(hh_spouses>0 & hh_childs>0 &  hh_other_relatives >0 & hh_non_relatives==0,31,
                                                           ifelse(hh_spouses>0 & hh_childs==0 & hh_other_relatives >0 & hh_non_relatives==0,31,
                                                                  ifelse(hh_spouses==0 & hh_childs>0 & hh_other_relatives >0 & hh_non_relatives==0,31,
                                                                         
                                                                         ifelse(hh_spouses>0 & hh_childs>0 &  hh_other_relatives ==0 & hh_non_relatives>0,32,
                                                                                ifelse(hh_spouses>0 & hh_childs==0 & hh_other_relatives ==0 & hh_non_relatives>0,32,
                                                                                       ifelse(hh_spouses==0 & hh_childs>0 & hh_other_relatives ==0 & hh_non_relatives>0,32,       
                                                                                              
                                                                                              ifelse(hh_spouses>0 & hh_childs>0 &  hh_other_relatives >0 & hh_non_relatives>0,33,
                                                                                                     ifelse(hh_spouses>0 & hh_childs==0 & hh_other_relatives >0 & hh_non_relatives>0,33,
                                                                                                            ifelse(hh_spouses==0 & hh_childs>0 & hh_other_relatives >0 & hh_non_relatives>0,33,        
                                                                                                                   
                                                                                                                   ifelse(hh_spouses==0 & hh_childs==0 &  hh_other_relatives >0 & hh_non_relatives==0,34,
                                                                                                                          ifelse(hh_spouses==0 & hh_childs==0 &  hh_other_relatives ==0 & hh_non_relatives>0,35,
                                                                                                                                 ifelse(hh_spouses==0 & hh_childs==0 &  hh_other_relatives >0 & hh_non_relatives>0,36,
                                                                                                                                        0)))))))))))))))))%>%
    mutate(family_type2=ifelse(hh_per1==1, 10,
                               ifelse(hh_spouses>0 & hh_childs>0 & hh_other_relatives ==0 & hh_non_relatives==0,20,
                                      ifelse(hh_spouses>0 & hh_childs==0 & hh_other_relatives ==0 & hh_non_relatives==0,20,
                                             ifelse(hh_spouses==0 & hh_childs>0 & hh_other_relatives ==0 & hh_non_relatives==0,20,       
                                                    ifelse(hh_non_relatives>0,30, # "Other non family",
                                                           ifelse((hh_parents>0 | hh_grandchilds>0),40,# "Stem family",
                                                                  ifelse(hh_other_relatives>0,50, 9999))))))))
  
  
  
  df
})

# STEP 6 LIST  #####
final<-Map(left_join, PASO4,auxlist, by="S3H")
gc()

# STEP 7 LIST ######
agrr_database_list_lfs<-lapply(final, function(df) {
  #df$REGION<-as.character(df$REGION)
  df<-df %>%
    group_by(S3H,ISO3,YEAR,SOURCE,SAMPLE,CONTINENT,CONT2,CENSUS_ROUND,DECADE,FIVE_YEAR)%>%
    summarise(WPOP=labelled(x=round(unique(WEIGHT_POP),0),label="Weighted population in sample"),
              
              HS01=labelled(x=sum(ifelse(hh_per1==1,hh,0))/sum(hh),label="Proportion of 1-person households"),
              HS02=labelled(x=sum(ifelse(hh_per1==2,hh,0))/sum(hh),label="Proportion of 2-person households"),
              HS03=labelled(x=sum(ifelse(hh_per1==3,hh,0))/sum(hh),label="Proportion of 3-person households"),
              HS04=labelled(x=sum(ifelse(hh_per1==4,hh,0))/sum(hh),label="Proportion of 4-person households"),
              HS05=labelled(x=sum(ifelse(hh_per1==5,hh,0))/sum(hh),label="Proportion of 5-person households"),
              HS06=labelled(x=sum(ifelse(hh_per1==6,hh,0))/sum(hh),label="Proportion of 6-person households"),
              HS07=labelled(x=sum(ifelse(hh_per1==7,hh,0))/sum(hh),label="Proportion of 7-person households"),
              HS08=labelled(x=sum(ifelse(hh_per1==8,hh,0))/sum(hh),label="Proportion of 8-person households"),
              HS09=labelled(x=sum(ifelse(hh_per1==9,hh,0))/sum(hh),label="Proportion of 9-person households"),
              HS10=labelled(x=sum(ifelse(hh_per1==10,hh,0))/sum(hh),label="Proportion of 10-person households"),
              HS11=labelled(x=sum(ifelse(hh_per1>10,hh,0))/sum(hh),label="Proportion of 11+person households"),
              
              HS12=labelled(x=sum(hh_child5 != 0)/sum(hh),label="Proportion of households with at least one person below 0-4 years old"),
              HS13=labelled(x=sum(hh_elderly65 != 0)/sum(hh),label="Proportion of households with at least one person 65+"),
              
              HS14=labelled(x=sum(hh_per)/unique(WEIGHT_A),label="Average household size"),
              HS15=labelled(x=sum(hh_child5)/unique(WEIGHT_A),label="Average number of 0-4 children in the household"),
              HS16=labelled(x=sum(hh_adult18)/unique(WEIGHT_A),label="Average number of adults in the household (aged 18+)"),
              HS17=labelled(x=sum(hh_child18)/unique(WEIGHT_A),label="Average number of children in the household (aged < 18)"),
              HS18=labelled(x=sum(hh_elderly65)/unique(WEIGHT_A), label="Average number of elderly people in the household (aged >65)"),
              
              HS19=labelled(x=sum(hh_00_09)/unique(WEIGHT_A),label="Average number of 0-9 individuals in the household"),
              HS20=labelled(x=sum(hh_10_19)/unique(WEIGHT_A),label="Average number of 10-19 individuals in the household"),
              HS21=labelled(x=sum(hh_20_29)/unique(WEIGHT_A),label="Average number of 20-29 individuals in the household"),
              HS22=labelled(x=sum(hh_30_39)/unique(WEIGHT_A),label="Average number of 30-39 individuals in the household"),
              HS23=labelled(x=sum(hh_40_49)/unique(WEIGHT_A),label="Average number of 40-49 individuals in the household"),
              HS24=labelled(x=sum(hh_50_59)/unique(WEIGHT_A),label="Average number of 50-59 individuals in the household"),
              HS25=labelled(x=sum(hh_60_69)/unique(WEIGHT_A),label="Average number of 60-69 individuals in the household"),
              HS26=labelled(x=sum(hh_70_79)/unique(WEIGHT_A),label="Average number of 70-79 individuals in the household"),
              HS27=labelled(x=sum(hh_80)/unique(WEIGHT_A),label="Average number of 80+ individuals in the household"),
              
              HS28=labelled(x=sum(ifelse((hh_per1==2|hh_per1==3),hh,0))/sum(hh),label="Proportion of 2-3 persons households"),
              HS29=labelled(x=sum(ifelse((hh_per1==4|hh_per1==5),hh,0))/sum(hh),label="Proportion of 4-5 persons households"),
              HS30=labelled(x=sum(ifelse(hh_per1>6,hh,0))/sum(hh),label="Proportion of 6+ person households"),
              
              HR01=labelled(x=sum(hh_heads)/unique(WEIGHT_A),label="Average number of Heads in the household"),
              HR02=labelled(x=sum(hh_spouses)/unique(WEIGHT_A),label="Average number of Spouses in the household"),
              HR03=labelled(x=sum(hh_childs)/unique(WEIGHT_A),label="Average number of Children in the household"),
              HR04=labelled(x=sum(hh_other_relatives)/unique(WEIGHT_A),label="Average number of other relatives in the household"),
              HR05=labelled(x=sum(hh_non_relatives)/unique(WEIGHT_A),label="Average number of non relatives in the household"),
              HR06=labelled(x=sum(hh_other_rel_or_non_rel)/unique(WEIGHT_A),label="Average number of relative or non relatives in the household"),
              
              HR07=labelled(x=sum(hh_heads[hh_per1==2])/unique(WEIGHT_A2) ,label="Average number of Heads in 2 persons household"),
              HR08=labelled(x=sum(hh_spouses[hh_per1==2])/unique(WEIGHT_A2) ,label="Average number of Spouses in 2 persons household"),
              HR09=labelled(x=sum(hh_childs[hh_per1==2])/unique(WEIGHT_A2) ,label="Average number of Children in 2 persons household"),
              HR10=labelled(x=sum(hh_other_relatives[hh_per1==2])/unique(WEIGHT_A2) ,label="Average number of Other relatives in 2 persons household"),
              HR11=labelled(x=sum(hh_non_relatives[hh_per1==2])/unique(WEIGHT_A2) ,label="Average number of non relatives in 2 persons household"),
              HR12=labelled(x=sum(hh_other_rel_or_non_rel[hh_per1==2])/unique(WEIGHT_A2) ,label="Average number of of relative or non relatives in 2 persons household"),
              
              HR13=labelled(x=sum(hh_heads[hh_per1==3])/unique(WEIGHT_A3) ,label="Average number of Heads in 3 persons household"),
              HR14=labelled(x=sum(hh_spouses[hh_per1==3])/unique(WEIGHT_A3) ,label="Average number of Spouses in 3 persons household"),
              HR15=labelled(x=sum(hh_childs[hh_per1==3])/unique(WEIGHT_A3) ,label="Average number of Children in 3 persons household"),
              HR16=labelled(x=sum(hh_other_relatives[hh_per1==3])/unique(WEIGHT_A3) ,label="Average number of Other relatives in 3 persons household"),
              HR17=labelled(x=sum(hh_non_relatives[hh_per1==3])/unique(WEIGHT_A3) ,label="Average number of non relatives in 3 persons household"),
              HR18=labelled(x=sum(hh_other_rel_or_non_rel[hh_per1==3])/unique(WEIGHT_A3) ,label="Average number of of relative or non relatives in 3 persons household"),
              
              HR19=labelled(x=sum(hh_heads[hh_per1==4])/unique(WEIGHT_A4) ,label="Average number of Heads in 4 persons household"),
              HR20=labelled(x=sum(hh_spouses[hh_per1==4])/unique(WEIGHT_A4) ,label="Average number of Spouses in 4 persons household"),
              HR21=labelled(x=sum(hh_childs[hh_per1==4])/unique(WEIGHT_A4) ,label="Average number of Children in 4 persons household"),
              HR22=labelled(x=sum(hh_other_relatives[hh_per1==4])/unique(WEIGHT_A4) ,label="Average number of Other relatives in 4 persons household"),
              HR23=labelled(x=sum(hh_non_relatives[hh_per1==4])/unique(WEIGHT_A4) ,label="Average number of non relatives in 4 persons household"),
              HR24=labelled(x=sum(hh_other_rel_or_non_rel[hh_per1==4])/unique(WEIGHT_A4) ,label="Average number of of relative or non relatives in 4 persons household"),
              
              HR25=labelled(x=sum(hh_heads[hh_per1==5])/unique(WEIGHT_A5) ,label="Average number of Heads in 5 persons household"),
              HR26=labelled(x=sum(hh_spouses[hh_per1==5])/unique(WEIGHT_A5) ,label="Average number of Spouses in 5 persons household"),
              HR27=labelled(x=sum(hh_childs[hh_per1==5])/unique(WEIGHT_A5) ,label="Average number of Children in 5 persons household"),
              HR28=labelled(x=sum(hh_other_relatives[hh_per1==5])/unique(WEIGHT_A5) ,label="Average number of Other relatives in 5 persons household"),
              HR29=labelled(x=sum(hh_non_relatives[hh_per1==5])/unique(WEIGHT_A5) ,label="Average number of non relatives in 5 persons household"),
              HR30=labelled(x=sum(hh_other_rel_or_non_rel[hh_per1==5])/unique(WEIGHT_A5) ,label="Average number of of relative or non relatives in 5 persons household"),
              
              HT01=labelled(x=sum(ifelse(family_type==10,1,0)/sum(hh)),label="Proportion of unipersonal households based on relationship to the head"),
              HT02=labelled(x=sum(ifelse(family_type==20,1,0)/sum(hh)),label="Proportion of nuclear households based on relationship to the head"),
              HT03=labelled(x=sum(ifelse(family_type==31,1,0)/sum(hh)),label="Proportion of nuclear plus other relative households based on relationship to the head"),
              HT04=labelled(x=sum(ifelse(family_type==32,1,0)/sum(hh)),label="Proportion of nuclear plus other non relative households based on relationship to the head"),
              HT05=labelled(x=sum(ifelse(family_type==33,1,0)/sum(hh)),label="Proportion of nuclear plus other relative plus other non relative households based on relationship to the head"),
              HT06=labelled(x=sum(ifelse(family_type==34,1,0)/sum(hh)),label="Proportion of other relative households based on relationship to the head"),
              HT07=labelled(x=sum(ifelse(family_type==35,1,0)/sum(hh)),label="Proportion of other non relative households based on relationship to the head"),
              HT08=labelled(x=sum(ifelse(family_type==36,1,0)/sum(hh)),label="Proportion of other relative plus other non relative households based on relationship to the head"),
              HT09=labelled(x=sum(ifelse(family_type==0,1,0)/sum(hh)),label="Proportion of other relative or non relative households based on relationship to the head"),
              
              HT10=labelled(x=sum(hh_per[family_type==10])/unique(WEIGHT_FT10),label="Average size of unipersonal households based on relationship to the head"),
              HT11=labelled(x=sum(hh_per[family_type==20])/unique(WEIGHT_FT20),label="Average size of nuclear households based on relationship to the head"),
              HT12=labelled(x=sum(hh_per[family_type==31])/unique(WEIGHT_FT31),label="Average size of nuclear plus other relative households based on relationship to the head"),
              HT13=labelled(x=sum(hh_per[family_type==32])/unique(WEIGHT_FT32),label="Average size of nuclear plus other non relative households based on relationship to the head"),
              HT14=labelled(x=sum(hh_per[family_type==33])/unique(WEIGHT_FT33),label="Average size of nuclear plus other relative plus other non relative households based on relationship to the head"),
              HT15=labelled(x=sum(hh_per[family_type==34])/unique(WEIGHT_FT34),label="Average size of other relative households based on relationship to the head"),
              HT16=labelled(x=sum(hh_per[family_type==35])/unique(WEIGHT_FT35),label="Average size of other non relative households based on relationship to the head"),
              HT17=labelled(x=sum(hh_per[family_type==36])/unique(WEIGHT_FT36),label="Average size of other relative plus other non relative households based on relationship to the head"),
              HT18=labelled(x=sum(hh_per[family_type==0])/unique(WEIGHT_FT00),label="Average size of other relative or non relative households based on relationship to the head"),
              
              HT20=labelled(x=sum(ifelse(family_type2==10,1,0)/sum(hh)),label="Proportion of unipersonal households based on relationship to the head"),
              HT21=labelled(x=sum(ifelse(family_type2==20,1,0)/sum(hh)),label="Proportion of nuclear households based on relationship to the head"),
              HT22=labelled(x=sum(ifelse(family_type2==40,1,0)/sum(hh)),label="Proportion of stem-family households based on relationship to the head"),
              HT23=labelled(x=sum(ifelse(family_type2==50,1,0)/sum(hh)),label="Proportion of Other family households based on relationship to the head"),
              HT24=labelled(x=sum(ifelse(family_type2==30,1,0)/sum(hh)),label="Proportion of Other non family households based on relationship to the head"),
              HT25=labelled(x=sum(ifelse(family_type2==9999,1,0)/sum(hh)),label="Proportion of other relative or non relative households based on relationship to the head"),
              
              HT26=labelled(x=sum(hh_per[family_type2==10])/unique(WEIGHT_FT210),label="Average household size of unipersonal based on relationship to the head"),
              HT27=labelled(x=sum(hh_per[family_type2==20])/unique(WEIGHT_FT220),label="Average household size of nuclear family based on relationship to the head"),
              HT28=labelled(x=sum(hh_per[family_type2==40])/unique(WEIGHT_FT240),label="Average household size of Stem family based on relationship to the head"),
              HT29=labelled(x=sum(hh_per[family_type2==50])/unique(WEIGHT_FT250),label="Average household size of Other family based on relationship to the head"),
              HT30=labelled(x=sum(hh_per[family_type2==30])/unique(WEIGHT_FT230),label="Average household size of Other non family based on relationship to the head"),
              HT31=labelled(x=sum(hh_per[family_type2==9999])/unique(WEIGHT_FT299),label="Average household size of other relative or non relative based on relationship to the head"),
              
              HH01=labelled(x=sum(hh_M_headed[hh_M_headed!=0])/unique(WEIGHT_A),label="Proportion of male-headed households"),
              HH02=labelled(x=sum(hh_F_headed[hh_F_headed!=0])/unique(WEIGHT_A),label="Proportion of female-headed households"),
              
              HH03=labelled(x=sum(ifelse((hh_m==1 &hh_per1==1),hh,0))/sum(hh_m),label="Proportion of 1-person households of male-headed households"),
              HH04=labelled(x=sum(ifelse((hh_m==1 &hh_per1==2),hh,0))/sum(hh_m),label="Proportion of 2-person households of male-headed households"),
              HH05=labelled(x=sum(ifelse((hh_m==1 &hh_per1==3),hh,0))/sum(hh_m),label="Proportion of 3-person households of male-headed households"),
              HH06=labelled(x=sum(ifelse((hh_m==1 &hh_per1==4),hh,0))/sum(hh_m),label="Proportion of 4-person households of male-headed households"),
              HH07=labelled(x=sum(ifelse((hh_m==1 &hh_per1==5),hh,0))/sum(hh_m),label="Proportion of 5-person households of male-headed households"),
              HH08=labelled(x=sum(ifelse((hh_m==1 &hh_per1==6),hh,0))/sum(hh_m),label="Proportion of 6-person households of male-headed households"),
              HH09=labelled(x=sum(ifelse((hh_m==1 &hh_per1==7),hh,0))/sum(hh_m),label="Proportion of 7-person households of male-headed households"),
              HH10=labelled(x=sum(ifelse((hh_m==1 &hh_per1==8),hh,0))/sum(hh_m),label="Proportion of 8-person households of male-headed households"),
              HH11=labelled(x=sum(ifelse((hh_m==1 &hh_per1==9),hh,0))/sum(hh_m),label="Proportion of 9-person households of male-headed households"),
              HH12=labelled(x=sum(ifelse((hh_m==1 &hh_per1==10),hh,0))/sum(hh_m),label="Proportion of 10-person households of male-headed households"),
              HH13=labelled(x=sum(ifelse((hh_m==1 &hh_per1>10),hh,0))/sum(hh_m),label="Proportion of 11+person households of male-headed households"),
              
              HH14=labelled(x=sum(ifelse((hh_f==1 &hh_per1==1),hh,0))/sum(hh_f),label="Proportion of 1-person households of female-headed households"),
              HH15=labelled(x=sum(ifelse((hh_f==1 &hh_per1==2),hh,0))/sum(hh_f),label="Proportion of 2-person households of female-headed households"),
              HH16=labelled(x=sum(ifelse((hh_f==1 &hh_per1==3),hh,0))/sum(hh_f),label="Proportion of 3-person households of female-headed households"),
              HH17=labelled(x=sum(ifelse((hh_f==1 &hh_per1==4),hh,0))/sum(hh_f),label="Proportion of 4-person households of female-headed households"),
              HH18=labelled(x=sum(ifelse((hh_f==1 &hh_per1==5),hh,0))/sum(hh_f),label="Proportion of 5-person households of female-headed households"),
              HH19=labelled(x=sum(ifelse((hh_f==1 &hh_per1==6),hh,0))/sum(hh_f),label="Proportion of 6-person households of female-headed households"),
              HH20=labelled(x=sum(ifelse((hh_f==1 &hh_per1==7),hh,0))/sum(hh_f),label="Proportion of 7-person households of female-headed households"),
              HH21=labelled(x=sum(ifelse((hh_f==1 &hh_per1==8),hh,0))/sum(hh_f),label="Proportion of 8-person households of female-headed households"),
              HH22=labelled(x=sum(ifelse((hh_f==1 &hh_per1==9),hh,0))/sum(hh_f),label="Proportion of 9-person households of female-headed households"),
              HH23=labelled(x=sum(ifelse((hh_f==1 &hh_per1==10),hh,0))/sum(hh_f),label="Proportion of 10-person households of female-headed households"),
              HH24=labelled(x=sum(ifelse((hh_f==1 &hh_per1>10),hh,0))/sum(hh_f),label="Proportion of 11+person households of female-headed households"),
              
              HH25=labelled(x=sum(ifelse((hh_m==1 &family_type2==10),1,0)/sum(hh_m)),label="Proportion of unipersonal households based on relationship to the head of male-headed households"),
              HH26=labelled(x=sum(ifelse((hh_m==1 &family_type2==20),1,0)/sum(hh_m)),label="Proportion of nuclear households based on relationship to the head of male-headed households"),
              HH27=labelled(x=sum(ifelse((hh_m==1 &family_type2==40),1,0)/sum(hh_m)),label="Proportion of stem-family households based on relationship to the head of male-headed households"),
              HH28=labelled(x=sum(ifelse((hh_m==1 &family_type2==50),1,0)/sum(hh_m)),label="Proportion of Other family households based on relationship to the head of male-headed households"),
              HH29=labelled(x=sum(ifelse((hh_m==1 &family_type2==30),1,0)/sum(hh_m)),label="Proportion of Other non family households based on relationship to the head of male-headed households"),
              HH30=labelled(x=sum(ifelse((hh_m==1 &family_type2==9999),1,0)/sum(hh_m)),label="Proportion of other relative or non relative households based on relationship to the head of male-headed households"),
              
              HH31=labelled(x=sum(ifelse((hh_f==1 &family_type2==10),1,0)/sum(hh_f)),label="Proportion of unipersonal households based on relationship to the head of female-headed households"),
              HH32=labelled(x=sum(ifelse((hh_f==1 &family_type2==20),1,0)/sum(hh_f)),label="Proportion of nuclear households based on relationship to the head of frmale-headed households"),
              HH33=labelled(x=sum(ifelse((hh_f==1 &family_type2==40),1,0)/sum(hh_f)),label="Proportion of stem-family households based on relationship to the head of female-headed households"),
              HH34=labelled(x=sum(ifelse((hh_f==1 &family_type2==50),1,0)/sum(hh_f)),label="Proportion of Other family households based on relationship to the head of female-headed households"),
              HH35=labelled(x=sum(ifelse((hh_f==1 &family_type2==30),1,0)/sum(hh_f)),label="Proportion of Other non family households based on relationship to the head of female-headed households"),
              HH36=labelled(x=sum(ifelse((hh_f==1 &family_type2==9999),1,0)/sum(hh_f)),label="Proportion of other relative or non relative households based on relationship to the head of female-headed households"),
              
              HH37=labelled(x=sum(hh_per[hh_M_headed!=0])/unique(WEIGHT_AM),label="Average household size of male-headed households"),
              HH38=labelled(x=sum(hh_per[hh_F_headed!=0])/unique(WEIGHT_AF),label="Average household size of female-headed households"),
              HH39=labelled(x=sum(hh_child18[hh_M_headed!=0])/unique(WEIGHT_AM),label="Average number of children of male-headed households"),
              HH40=labelled(x=sum(hh_child18[hh_F_headed!=0])/unique(WEIGHT_AF),label="Average number of children of female-headed households"),
              HH41=labelled(x=sum(hh_adult18[hh_M_headed!=0])/unique(WEIGHT_AM),label="Average number of adults of male-headed households"),
              HH42=labelled(x=sum(hh_adult18[hh_F_headed!=0])/unique(WEIGHT_AF),label="Average number of adults of female-headed households"),
              HH43=labelled(x=sum(hh_elderly65[hh_M_headed!=0])/unique(WEIGHT_AM),label="Average number of elderly people (aged >65) of male-headed households"),
              HH44=labelled(x=sum(hh_elderly65[hh_F_headed!=0])/unique(WEIGHT_AF),label="Average number of elderly people (aged >65) of female-headed households"),
              
              HH45=labelled(x=sum(hh_spouses[hh_M_headed!=0])/unique(WEIGHT_AM),label="Average number of spouses of male-headed households"),
              HH46=labelled(x=sum(hh_spouses[hh_F_headed!=0])/unique(WEIGHT_AF),label="Average number of spouses of female-headed households"),
              HH47=labelled(x=sum(hh_childs[hh_M_headed!=0])/unique(WEIGHT_AM),label="Average number of childs of male-headed households"),
              HH48=labelled(x=sum(hh_childs[hh_F_headed!=0])/unique(WEIGHT_AF),label="Average number of childs of female-headed households"),
              HH49=labelled(x=sum(hh_other_relatives[hh_M_headed!=0])/unique(WEIGHT_AM),label="Average number of other relatives of male-headed households"),
              HH50=labelled(x=sum(hh_other_relatives[hh_F_headed!=0])/unique(WEIGHT_AF),label="Average number of other relatives of female-headed households"),
              HH51=labelled(x=sum(hh_non_relatives[hh_M_headed!=0])/unique(WEIGHT_AM),label="Average number of other non relatives of male-headed households"),
              HH52=labelled(x=sum(hh_non_relatives[hh_F_headed!=0])/unique(WEIGHT_AF),label="Average number of other non relatives of female-headed households"),
              
              HH53=labelled(x=sum(hh_adult18M[hh_M_headed!=0])/unique(WEIGHT_AM),label="Average number of males adults in male-headed households"),
              HH54=labelled(x=sum(hh_adult18M[hh_F_headed!=0])/unique(WEIGHT_AF),label="Average number of males adults in female-headed households"),
              HH55=labelled(x=sum(hh_adult18F[hh_M_headed!=0])/unique(WEIGHT_AM),label="Average number of female adults in male-headed households"),
              HH56=labelled(x=sum(hh_adult18F[hh_F_headed!=0])/unique(WEIGHT_AF),label="Average number of female adults in female-headed households")
    )
  
})




# SETP 8  #####
l1 <- agrr_database_list_lfs[sapply(agrr_database_list_lfs, function(x) length(x$REGION) == 1)]

agrr_database_lfs <- do.call("rbind", agrr_database_list_lfs)


regionw<-as.data.frame(regionw)
agrr_database_lfs<-as.data.frame(agrr_database_lfs)
agrr_database_lfs<- data.frame(agrr_database_lfs,
                               regionw[match(agrr_database_lfs[,"ISO3"],
                                             regionw[,"ISO3"]),c("EU Member States")])

colnames(agrr_database_lfs)[grep(names(agrr_database_lfs %>% select(last_col())), colnames(agrr_database_lfs))]<-"CNTRY"

clipr::write_clip(agrr_database_lfs)

save(agrr_database_lfs, file="G:\\Shared drives\\CORESIDENCE\\TEAM FOLDERS\\Juan Galeano\\AGGR_INDICATORS_LFS_SUBNATIONAL_HARMONIZED.RData")

gc()


##### SILC #######
# READ CODES REGIONS #####
library(readxl)
regionw <- read_excel("C:/Users/jgaleano/Downloads/regionw.xlsx")



# CORE LIST SILC #####


setwd("G:\\Shared drives\\CORESIDENCE\\WP2_DATA\\2_4_CORESIDENCE_DATABASE\\CORESIDENCE_SILC_HARMO\\")

patt.shp <- ".*RData*"

data.shp <-list.files(pattern=patt.shp)

CORE_SILC_LIST <- sapply(data.shp, function(x) mget(load(x)), simplify = TRUE)  


# STEP 1 LIST ######

PASO1<-lapply(CORE_SILC_LIST, function(df) {
  
  df$SOURCE<-"SILC"
  df$SAMPLE<-with(df,paste(ISO3, YEAR, SOURCE, sep="_"))
  
  
  df<-df %>%
    #filter(GQ %in% VALID_GQ) %>% #filtrem només els private households
    add_count(HHID)%>%
    mutate(child5=ifelse(AGE<=4,1,0),
           adult18=ifelse(AGE>=18,1,0),
           adult18M=ifelse((AGE>=18&RB090==1),1,0),
           adult18F=ifelse((AGE>=18&RB090==2),1,0),
           child18=ifelse(AGE<18,1,0),
           elderly65=ifelse(AGE>=65,1,0),
           
           age00_09=ifelse((AGE2=="0-4"|AGE2=="5-9"),1,0),
           age10_19=ifelse((AGE2=="10-14"|AGE2=="15-19"),1,0),
           age20_29=ifelse((AGE2=="20-24"|AGE2=="25-29"),1,0),
           age30_39=ifelse((AGE2=="30-34"|AGE2=="35-39"),1,0),
           age40_49=ifelse((AGE2=="40-44"|AGE2=="45-49"),1,0),
           age50_59=ifelse((AGE2=="50-54"|AGE2=="55-59"),1,0),
           age60_69=ifelse((AGE2=="60-64"|AGE2=="65-69"),1,0),
           age70_79=ifelse((AGE2=="70-74"|AGE2=="75-79"),1,0),
           age80=ifelse(AGE>=80,1,0),
           
           heads=ifelse(RELATE==1,1,0), 
           spouses=ifelse(RELATE==2,1,0),
           childs=ifelse(RELATE==3,1,0),
           other_relatives=ifelse(RELATE==4,1,0),
           non_relatives=ifelse(RELATE==5,1,0),
           other_rel_or_non_rel=ifelse(RELATE==6,1,0),
           
           grandchilds=ifelse(RELATED==4100,1,0),
           parents=ifelse(RELATED==4200,1,0),
           siblings=ifelse(RELATED==4400,1,0),
           
           M_headed=ifelse((RELATE==1 & RB090==1),1,0),
           F_headed=ifelse((RELATE==1 & RB090==2),1,0),
           
           WEIGHT_POP= sum(RB050,na.rm = TRUE)*1,
           #WEIGHT_A= sum(df[df$RELATE==1,"PWEIGHT_A"]),
           WEIGHT_A= sum(RB050[RELATE==1],na.rm = TRUE),
           WEIGHT_A2= sum(RB050[RELATE==1&n==2]),
           WEIGHT_A3= sum(RB050[RELATE==1&n==3]),
           WEIGHT_A4= sum(RB050[RELATE==1&n==4]),
           WEIGHT_A5= sum(RB050[RELATE==1&n==5]),
           WEIGHT_AM= sum(RB050[RELATE==1&RB090==1]),
           WEIGHT_AF= sum(RB050[RELATE==1&RB090==2]))  %>%
    drop_na(RB050)
})
gc()

# STEP 2 LIST #######  
PASO2<-lapply(PASO1, function(df) {  
  df<-df%>%
    group_by(HHID)%>%
    summarise(hh_per1=n(),
              hh_parents=sum(parents*RB050),
              hh_grandchilds=sum(grandchilds*RB050),
              hh_heads=sum(heads*RB050),
              hh_spouses=sum(spouses*RB050),
              hh_childs=sum(childs*RB050),
              hh_other_relatives=sum(other_relatives*RB050),
              hh_non_relatives=sum(non_relatives*RB050),
              hh_other_rel_or_non_rel=sum(other_rel_or_non_rel*RB050))%>%
    mutate(family_type= ifelse(hh_per1==1, 10,
                               ifelse(hh_spouses>0 & hh_childs>0 & hh_other_relatives ==0 & hh_non_relatives==0,20,
                                      ifelse(hh_spouses>0 & hh_childs==0 & hh_other_relatives ==0 & hh_non_relatives==0,20,
                                             ifelse(hh_spouses==0 & hh_childs>0 & hh_other_relatives ==0 & hh_non_relatives==0,20,
                                                    
                                                    ifelse(hh_spouses>0 & hh_childs>0 &  hh_other_relatives >0 & hh_non_relatives==0,31,
                                                           ifelse(hh_spouses>0 & hh_childs==0 & hh_other_relatives >0 & hh_non_relatives==0,31,
                                                                  ifelse(hh_spouses==0 & hh_childs>0 & hh_other_relatives >0 & hh_non_relatives==0,31,
                                                                         
                                                                         ifelse(hh_spouses>0 & hh_childs>0 &  hh_other_relatives ==0 & hh_non_relatives>0,32,
                                                                                ifelse(hh_spouses>0 & hh_childs==0 & hh_other_relatives ==0 & hh_non_relatives>0,32,
                                                                                       ifelse(hh_spouses==0 & hh_childs>0 & hh_other_relatives ==0 & hh_non_relatives>0,32,       
                                                                                              
                                                                                              ifelse(hh_spouses>0 & hh_childs>0 &  hh_other_relatives >0 & hh_non_relatives>0,33,
                                                                                                     ifelse(hh_spouses>0 & hh_childs==0 & hh_other_relatives >0 & hh_non_relatives>0,33,
                                                                                                            ifelse(hh_spouses==0 & hh_childs>0 & hh_other_relatives >0 & hh_non_relatives>0,33,        
                                                                                                                   
                                                                                                                   ifelse(hh_spouses==0 & hh_childs==0 &  hh_other_relatives >0 & hh_non_relatives==0,34,
                                                                                                                          ifelse(hh_spouses==0 & hh_childs==0 &  hh_other_relatives ==0 & hh_non_relatives>0,35,
                                                                                                                                 ifelse(hh_spouses==0 & hh_childs==0 &  hh_other_relatives >0 & hh_non_relatives>0,36,
                                                                                                                                        0)))))))))))))))))%>%
    mutate(family_type2=ifelse(hh_per1==1, 10,
                               ifelse(hh_spouses>0 & hh_childs>0 & hh_other_relatives ==0 & hh_non_relatives==0,20,
                                      ifelse(hh_spouses>0 & hh_childs==0 & hh_other_relatives ==0 & hh_non_relatives==0,20,
                                             ifelse(hh_spouses==0 & hh_childs>0 & hh_other_relatives ==0 & hh_non_relatives==0,20,       
                                                    ifelse(hh_non_relatives>0,30, # "Other non family",
                                                           ifelse((hh_parents>0 | hh_grandchilds>0),40,# "Stem family",
                                                                  ifelse(hh_other_relatives>0,50, 9999)))))))) #"Other family", 
  
  
  df<-df%>%select(HHID,family_type,family_type2)             
  df
})

# STEP 3 LIST ####
PASO3<- map2(PASO1, PASO2, ~left_join(.x, .y, by = 'HHID'))

# STEP 4 LIST ####
auxlist<-lapply(PASO3, function(df) {
  
  df<-df %>%
    group_by(S3H)%>%
    summarise(WEIGHT_POP= sum(RB050,na.rm = TRUE)*1,
              #WEIGHT_A= sum(df[df$RELATE==1,"PWEIGHT_A"]),
              WEIGHT_A= sum(RB050[RELATE==1]),
              WEIGHT_A2= sum(RB050[RELATE==1&n==2]),
              WEIGHT_A3= sum(RB050[RELATE==1&n==3]),
              WEIGHT_A4= sum(RB050[RELATE==1&n==4]),
              WEIGHT_A5= sum(RB050[RELATE==1&n==5]),
              WEIGHT_AM= sum(RB050[RELATE==1&RB090==1]),
              WEIGHT_AF= sum(RB050[RELATE==1&RB090==2]),
              
              WEIGHT_FT10= sum(RB050[RELATE==1&family_type==10]),
              WEIGHT_FT20= sum(RB050[RELATE==1&family_type==20]),
              WEIGHT_FT31= sum(RB050[RELATE==1&family_type==31]),
              WEIGHT_FT32= sum(RB050[RELATE==1&family_type==32]),
              WEIGHT_FT33= sum(RB050[RELATE==1&family_type==33]),
              WEIGHT_FT34= sum(RB050[RELATE==1&family_type==34]),
              WEIGHT_FT35= sum(RB050[RELATE==1&family_type==35]),
              WEIGHT_FT36= sum(RB050[RELATE==1&family_type==36]),
              WEIGHT_FT00= sum(RB050[RELATE==1&family_type==0]),
              
              WEIGHT_FT210= sum(RB050[RELATE==1&family_type2==10]),
              WEIGHT_FT220= sum(RB050[RELATE==1&family_type2==20]),
              WEIGHT_FT230= sum(RB050[RELATE==1&family_type2==30]),
              WEIGHT_FT240= sum(RB050[RELATE==1&family_type2==40]),
              WEIGHT_FT250= sum(RB050[RELATE==1&family_type2==50]),
              WEIGHT_FT299= sum(RB050[RELATE==1&family_type2==9999]))
  df
})

# STEP 5 LIST #####
PASO4<-lapply(PASO3, function(df) {
  df<-df %>% ##### STEP 4 #######
  group_by(S3H,SAMPLE,ISO3,YEAR,HHID,CONTINENT,SOURCE,CENSUS_ROUND,DECADE,FIVE_YEAR,CONT2)%>%
    summarise(hh=1,
              hh_per1=n(),
              hh_m=sum(M_headed),
              hh_f=sum(F_headed),
              hh_per=sum(hh*RB050),
              hh_child5=sum(child5*RB050),
              hh_adult18=sum(adult18*RB050),
              
              hh_00_09=sum(age00_09*RB050),
              hh_10_19=sum(age10_19*RB050),
              hh_20_29=sum(age20_29*RB050),
              hh_30_39=sum(age30_39*RB050),
              hh_40_49=sum(age40_49*RB050),
              hh_50_59=sum(age50_59*RB050),
              hh_60_69=sum(age60_69*RB050),
              hh_70_79=sum(age70_79*RB050),
              hh_80=sum(age80*RB050),
              
              hh_adult18M=sum(adult18M*RB050),
              hh_adult18F=sum(adult18F*RB050),
              
              hh_child18=sum(child18*RB050),
              hh_elderly65=sum(elderly65*RB050),
              
              hh_heads=sum(heads*RB050),
              hh_spouses=sum(spouses*RB050),
              hh_childs=sum(childs*RB050),
              hh_other_relatives=sum(other_relatives*RB050),
              hh_non_relatives=sum(non_relatives*RB050),
              hh_other_rel_or_non_rel=sum(other_rel_or_non_rel*RB050),
              
              hh_grandchilds=sum(grandchilds*RB050),
              hh_parents=sum(parents*RB050),
              hh_siblings=sum(siblings*RB050), 
              hh_M_headed=sum(M_headed*RB050),
              
              hh_F_headed=sum(F_headed*RB050))%>%
    mutate(family_type= ifelse(hh_per1==1, 10,
                               ifelse(hh_spouses>0 & hh_childs>0 & hh_other_relatives ==0 & hh_non_relatives==0,20,
                                      ifelse(hh_spouses>0 & hh_childs==0 & hh_other_relatives ==0 & hh_non_relatives==0,20,
                                             ifelse(hh_spouses==0 & hh_childs>0 & hh_other_relatives ==0 & hh_non_relatives==0,20,
                                                    
                                                    ifelse(hh_spouses>0 & hh_childs>0 &  hh_other_relatives >0 & hh_non_relatives==0,31,
                                                           ifelse(hh_spouses>0 & hh_childs==0 & hh_other_relatives >0 & hh_non_relatives==0,31,
                                                                  ifelse(hh_spouses==0 & hh_childs>0 & hh_other_relatives >0 & hh_non_relatives==0,31,
                                                                         
                                                                         ifelse(hh_spouses>0 & hh_childs>0 &  hh_other_relatives ==0 & hh_non_relatives>0,32,
                                                                                ifelse(hh_spouses>0 & hh_childs==0 & hh_other_relatives ==0 & hh_non_relatives>0,32,
                                                                                       ifelse(hh_spouses==0 & hh_childs>0 & hh_other_relatives ==0 & hh_non_relatives>0,32,       
                                                                                              
                                                                                              ifelse(hh_spouses>0 & hh_childs>0 &  hh_other_relatives >0 & hh_non_relatives>0,33,
                                                                                                     ifelse(hh_spouses>0 & hh_childs==0 & hh_other_relatives >0 & hh_non_relatives>0,33,
                                                                                                            ifelse(hh_spouses==0 & hh_childs>0 & hh_other_relatives >0 & hh_non_relatives>0,33,        
                                                                                                                   
                                                                                                                   ifelse(hh_spouses==0 & hh_childs==0 &  hh_other_relatives >0 & hh_non_relatives==0,34,
                                                                                                                          ifelse(hh_spouses==0 & hh_childs==0 &  hh_other_relatives ==0 & hh_non_relatives>0,35,
                                                                                                                                 ifelse(hh_spouses==0 & hh_childs==0 &  hh_other_relatives >0 & hh_non_relatives>0,36,
                                                                                                                                        0)))))))))))))))))%>%
    mutate(family_type2=ifelse(hh_per1==1, 10,
                               ifelse(hh_spouses>0 & hh_childs>0 & hh_other_relatives ==0 & hh_non_relatives==0,20,
                                      ifelse(hh_spouses>0 & hh_childs==0 & hh_other_relatives ==0 & hh_non_relatives==0,20,
                                             ifelse(hh_spouses==0 & hh_childs>0 & hh_other_relatives ==0 & hh_non_relatives==0,20,       
                                                    ifelse(hh_non_relatives>0,30, # "Other non family",
                                                           ifelse((hh_parents>0 | hh_grandchilds>0),40,# "Stem family",
                                                                  ifelse(hh_other_relatives>0,50, 9999))))))))
  
  
  
  df
})


# STEP 6 LIST  #####
final<-Map(left_join, PASO4,auxlist, by="S3H")
gc()


# STEP 7 LIST ######
agrr_database_list_silc<-lapply(final, function(df) {
  # df$REGION<-as.character(df$REGION)
  df<-df %>%
    group_by(S3H,ISO3,YEAR,SOURCE,SAMPLE,CONTINENT,CONT2,CENSUS_ROUND,DECADE,FIVE_YEAR)%>%
    summarise(WPOP=labelled(x=round(unique(WEIGHT_POP),0),label="Weighted population in sample"),
              
              HS01=labelled(x=sum(ifelse(hh_per1==1,hh,0))/sum(hh),label="Proportion of 1-person households"),
              HS02=labelled(x=sum(ifelse(hh_per1==2,hh,0))/sum(hh),label="Proportion of 2-person households"),
              HS03=labelled(x=sum(ifelse(hh_per1==3,hh,0))/sum(hh),label="Proportion of 3-person households"),
              HS04=labelled(x=sum(ifelse(hh_per1==4,hh,0))/sum(hh),label="Proportion of 4-person households"),
              HS05=labelled(x=sum(ifelse(hh_per1==5,hh,0))/sum(hh),label="Proportion of 5-person households"),
              HS06=labelled(x=sum(ifelse(hh_per1==6,hh,0))/sum(hh),label="Proportion of 6-person households"),
              HS07=labelled(x=sum(ifelse(hh_per1==7,hh,0))/sum(hh),label="Proportion of 7-person households"),
              HS08=labelled(x=sum(ifelse(hh_per1==8,hh,0))/sum(hh),label="Proportion of 8-person households"),
              HS09=labelled(x=sum(ifelse(hh_per1==9,hh,0))/sum(hh),label="Proportion of 9-person households"),
              HS10=labelled(x=sum(ifelse(hh_per1==10,hh,0))/sum(hh),label="Proportion of 10-person households"),
              HS11=labelled(x=sum(ifelse(hh_per1>10,hh,0))/sum(hh),label="Proportion of 11+person households"),
              
              HS12=labelled(x=sum(hh_child5 != 0)/sum(hh),label="Proportion of households with at least one person below 0-4 years old"),
              HS13=labelled(x=sum(hh_elderly65 != 0)/sum(hh),label="Proportion of households with at least one person 65+"),
              
              HS14=labelled(x=sum(hh_per)/unique(WEIGHT_A),label="Average household size"),
              HS15=labelled(x=sum(hh_child5)/unique(WEIGHT_A),label="Average number of 0-4 children in the household"),
              HS16=labelled(x=sum(hh_adult18)/unique(WEIGHT_A),label="Average number of adults in the household (aged 18+)"),
              HS17=labelled(x=sum(hh_child18)/unique(WEIGHT_A),label="Average number of children in the household (aged < 18)"),
              HS18=labelled(x=sum(hh_elderly65)/unique(WEIGHT_A), label="Average number of elderly people in the household (aged >65)"),
              
              HS19=labelled(x=sum(hh_00_09)/unique(WEIGHT_A),label="Average number of 0-9 individuals in the household"),
              HS20=labelled(x=sum(hh_10_19)/unique(WEIGHT_A),label="Average number of 10-19 individuals in the household"),
              HS21=labelled(x=sum(hh_20_29)/unique(WEIGHT_A),label="Average number of 20-29 individuals in the household"),
              HS22=labelled(x=sum(hh_30_39)/unique(WEIGHT_A),label="Average number of 30-39 individuals in the household"),
              HS23=labelled(x=sum(hh_40_49)/unique(WEIGHT_A),label="Average number of 40-49 individuals in the household"),
              HS24=labelled(x=sum(hh_50_59)/unique(WEIGHT_A),label="Average number of 50-59 individuals in the household"),
              HS25=labelled(x=sum(hh_60_69)/unique(WEIGHT_A),label="Average number of 60-69 individuals in the household"),
              HS26=labelled(x=sum(hh_70_79)/unique(WEIGHT_A),label="Average number of 70-79 individuals in the household"),
              HS27=labelled(x=sum(hh_80)/unique(WEIGHT_A),label="Average number of 80+ individuals in the household"),
              
              HS28=labelled(x=sum(ifelse((hh_per1==2|hh_per1==3),hh,0))/sum(hh),label="Proportion of 2-3 persons households"),
              HS29=labelled(x=sum(ifelse((hh_per1==4|hh_per1==5),hh,0))/sum(hh),label="Proportion of 4-5 persons households"),
              HS30=labelled(x=sum(ifelse(hh_per1>6,hh,0))/sum(hh),label="Proportion of 6+ person households"),
              
              HR01=labelled(x=sum(hh_heads)/unique(WEIGHT_A),label="Average number of Heads in the household"),
              HR02=labelled(x=sum(hh_spouses)/unique(WEIGHT_A),label="Average number of Spouses in the household"),
              HR03=labelled(x=sum(hh_childs)/unique(WEIGHT_A),label="Average number of Children in the household"),
              HR04=labelled(x=sum(hh_other_relatives)/unique(WEIGHT_A),label="Average number of other relatives in the household"),
              HR05=labelled(x=sum(hh_non_relatives)/unique(WEIGHT_A),label="Average number of non relatives in the household"),
              HR06=labelled(x=sum(hh_other_rel_or_non_rel)/unique(WEIGHT_A),label="Average number of relative or non relatives in the household"),
              
              HR07=labelled(x=sum(hh_heads[hh_per1==2])/unique(WEIGHT_A2) ,label="Average number of Heads in 2 persons household"),
              HR08=labelled(x=sum(hh_spouses[hh_per1==2])/unique(WEIGHT_A2) ,label="Average number of Spouses in 2 persons household"),
              HR09=labelled(x=sum(hh_childs[hh_per1==2])/unique(WEIGHT_A2) ,label="Average number of Children in 2 persons household"),
              HR10=labelled(x=sum(hh_other_relatives[hh_per1==2])/unique(WEIGHT_A2) ,label="Average number of Other relatives in 2 persons household"),
              HR11=labelled(x=sum(hh_non_relatives[hh_per1==2])/unique(WEIGHT_A2) ,label="Average number of non relatives in 2 persons household"),
              HR12=labelled(x=sum(hh_other_rel_or_non_rel[hh_per1==2])/unique(WEIGHT_A2) ,label="Average number of of relative or non relatives in 2 persons household"),
              
              HR13=labelled(x=sum(hh_heads[hh_per1==3])/unique(WEIGHT_A3) ,label="Average number of Heads in 3 persons household"),
              HR14=labelled(x=sum(hh_spouses[hh_per1==3])/unique(WEIGHT_A3) ,label="Average number of Spouses in 3 persons household"),
              HR15=labelled(x=sum(hh_childs[hh_per1==3])/unique(WEIGHT_A3) ,label="Average number of Children in 3 persons household"),
              HR16=labelled(x=sum(hh_other_relatives[hh_per1==3])/unique(WEIGHT_A3) ,label="Average number of Other relatives in 3 persons household"),
              HR17=labelled(x=sum(hh_non_relatives[hh_per1==3])/unique(WEIGHT_A3) ,label="Average number of non relatives in 3 persons household"),
              HR18=labelled(x=sum(hh_other_rel_or_non_rel[hh_per1==3])/unique(WEIGHT_A3) ,label="Average number of of relative or non relatives in 3 persons household"),
              
              HR19=labelled(x=sum(hh_heads[hh_per1==4])/unique(WEIGHT_A4) ,label="Average number of Heads in 4 persons household"),
              HR20=labelled(x=sum(hh_spouses[hh_per1==4])/unique(WEIGHT_A4) ,label="Average number of Spouses in 4 persons household"),
              HR21=labelled(x=sum(hh_childs[hh_per1==4])/unique(WEIGHT_A4) ,label="Average number of Children in 4 persons household"),
              HR22=labelled(x=sum(hh_other_relatives[hh_per1==4])/unique(WEIGHT_A4) ,label="Average number of Other relatives in 4 persons household"),
              HR23=labelled(x=sum(hh_non_relatives[hh_per1==4])/unique(WEIGHT_A4) ,label="Average number of non relatives in 4 persons household"),
              HR24=labelled(x=sum(hh_other_rel_or_non_rel[hh_per1==4])/unique(WEIGHT_A4) ,label="Average number of of relative or non relatives in 4 persons household"),
              
              HR25=labelled(x=sum(hh_heads[hh_per1==5])/unique(WEIGHT_A5) ,label="Average number of Heads in 5 persons household"),
              HR26=labelled(x=sum(hh_spouses[hh_per1==5])/unique(WEIGHT_A5) ,label="Average number of Spouses in 5 persons household"),
              HR27=labelled(x=sum(hh_childs[hh_per1==5])/unique(WEIGHT_A5) ,label="Average number of Children in 5 persons household"),
              HR28=labelled(x=sum(hh_other_relatives[hh_per1==5])/unique(WEIGHT_A5) ,label="Average number of Other relatives in 5 persons household"),
              HR29=labelled(x=sum(hh_non_relatives[hh_per1==5])/unique(WEIGHT_A5) ,label="Average number of non relatives in 5 persons household"),
              HR30=labelled(x=sum(hh_other_rel_or_non_rel[hh_per1==5])/unique(WEIGHT_A5) ,label="Average number of of relative or non relatives in 5 persons household"),
              
              HT01=labelled(x=sum(ifelse(family_type==10,1,0)/sum(hh)),label="Proportion of unipersonal households based on relationship to the head"),
              HT02=labelled(x=sum(ifelse(family_type==20,1,0)/sum(hh)),label="Proportion of nuclear households based on relationship to the head"),
              HT03=labelled(x=sum(ifelse(family_type==31,1,0)/sum(hh)),label="Proportion of nuclear plus other relative households based on relationship to the head"),
              HT04=labelled(x=sum(ifelse(family_type==32,1,0)/sum(hh)),label="Proportion of nuclear plus other non relative households based on relationship to the head"),
              HT05=labelled(x=sum(ifelse(family_type==33,1,0)/sum(hh)),label="Proportion of nuclear plus other relative plus other non relative households based on relationship to the head"),
              HT06=labelled(x=sum(ifelse(family_type==34,1,0)/sum(hh)),label="Proportion of other relative households based on relationship to the head"),
              HT07=labelled(x=sum(ifelse(family_type==35,1,0)/sum(hh)),label="Proportion of other non relative households based on relationship to the head"),
              HT08=labelled(x=sum(ifelse(family_type==36,1,0)/sum(hh)),label="Proportion of other relative plus other non relative households based on relationship to the head"),
              HT09=labelled(x=sum(ifelse(family_type==0,1,0)/sum(hh)),label="Proportion of other relative or non relative households based on relationship to the head"),
              
              HT10=labelled(x=sum(hh_per[family_type==10])/unique(WEIGHT_FT10),label="Average size of unipersonal households based on relationship to the head"),
              HT11=labelled(x=sum(hh_per[family_type==20])/unique(WEIGHT_FT20),label="Average size of nuclear households based on relationship to the head"),
              HT12=labelled(x=sum(hh_per[family_type==31])/unique(WEIGHT_FT31),label="Average size of nuclear plus other relative households based on relationship to the head"),
              HT13=labelled(x=sum(hh_per[family_type==32])/unique(WEIGHT_FT32),label="Average size of nuclear plus other non relative households based on relationship to the head"),
              HT14=labelled(x=sum(hh_per[family_type==33])/unique(WEIGHT_FT33),label="Average size of nuclear plus other relative plus other non relative households based on relationship to the head"),
              HT15=labelled(x=sum(hh_per[family_type==34])/unique(WEIGHT_FT34),label="Average size of other relative households based on relationship to the head"),
              HT16=labelled(x=sum(hh_per[family_type==35])/unique(WEIGHT_FT35),label="Average size of other non relative households based on relationship to the head"),
              HT17=labelled(x=sum(hh_per[family_type==36])/unique(WEIGHT_FT36),label="Average size of other relative plus other non relative households based on relationship to the head"),
              HT18=labelled(x=sum(hh_per[family_type==0])/unique(WEIGHT_FT00),label="Average size of other relative or non relative households based on relationship to the head"),
              
              HT20=labelled(x=sum(ifelse(family_type2==10,1,0)/sum(hh)),label="Proportion of unipersonal households based on relationship to the head"),
              HT21=labelled(x=sum(ifelse(family_type2==20,1,0)/sum(hh)),label="Proportion of nuclear households based on relationship to the head"),
              HT22=labelled(x=sum(ifelse(family_type2==40,1,0)/sum(hh)),label="Proportion of stem-family households based on relationship to the head"),
              HT23=labelled(x=sum(ifelse(family_type2==50,1,0)/sum(hh)),label="Proportion of Other family households based on relationship to the head"),
              HT24=labelled(x=sum(ifelse(family_type2==30,1,0)/sum(hh)),label="Proportion of Other non family households based on relationship to the head"),
              HT25=labelled(x=sum(ifelse(family_type2==9999,1,0)/sum(hh)),label="Proportion of other relative or non relative households based on relationship to the head"),
              
              HT26=labelled(x=sum(hh_per[family_type2==10])/unique(WEIGHT_FT210),label="Average household size of unipersonal based on relationship to the head"),
              HT27=labelled(x=sum(hh_per[family_type2==20])/unique(WEIGHT_FT220),label="Average household size of nuclear family based on relationship to the head"),
              HT28=labelled(x=sum(hh_per[family_type2==40])/unique(WEIGHT_FT240),label="Average household size of Stem family based on relationship to the head"),
              HT29=labelled(x=sum(hh_per[family_type2==50])/unique(WEIGHT_FT250),label="Average household size of Other family based on relationship to the head"),
              HT30=labelled(x=sum(hh_per[family_type2==30])/unique(WEIGHT_FT230),label="Average household size of Other non family based on relationship to the head"),
              HT31=labelled(x=sum(hh_per[family_type2==9999])/unique(WEIGHT_FT299),label="Average household size of other relative or non relative based on relationship to the head"),
              
              HH01=labelled(x=sum(hh_M_headed[hh_M_headed!=0])/unique(WEIGHT_A),label="Proportion of male-headed households"),
              HH02=labelled(x=sum(hh_F_headed[hh_F_headed!=0])/unique(WEIGHT_A),label="Proportion of female-headed households"),
              
              HH03=labelled(x=sum(ifelse((hh_m==1 &hh_per1==1),hh,0))/sum(hh_m),label="Proportion of 1-person households of male-headed households"),
              HH04=labelled(x=sum(ifelse((hh_m==1 &hh_per1==2),hh,0))/sum(hh_m),label="Proportion of 2-person households of male-headed households"),
              HH05=labelled(x=sum(ifelse((hh_m==1 &hh_per1==3),hh,0))/sum(hh_m),label="Proportion of 3-person households of male-headed households"),
              HH06=labelled(x=sum(ifelse((hh_m==1 &hh_per1==4),hh,0))/sum(hh_m),label="Proportion of 4-person households of male-headed households"),
              HH07=labelled(x=sum(ifelse((hh_m==1 &hh_per1==5),hh,0))/sum(hh_m),label="Proportion of 5-person households of male-headed households"),
              HH08=labelled(x=sum(ifelse((hh_m==1 &hh_per1==6),hh,0))/sum(hh_m),label="Proportion of 6-person households of male-headed households"),
              HH09=labelled(x=sum(ifelse((hh_m==1 &hh_per1==7),hh,0))/sum(hh_m),label="Proportion of 7-person households of male-headed households"),
              HH10=labelled(x=sum(ifelse((hh_m==1 &hh_per1==8),hh,0))/sum(hh_m),label="Proportion of 8-person households of male-headed households"),
              HH11=labelled(x=sum(ifelse((hh_m==1 &hh_per1==9),hh,0))/sum(hh_m),label="Proportion of 9-person households of male-headed households"),
              HH12=labelled(x=sum(ifelse((hh_m==1 &hh_per1==10),hh,0))/sum(hh_m),label="Proportion of 10-person households of male-headed households"),
              HH13=labelled(x=sum(ifelse((hh_m==1 &hh_per1>10),hh,0))/sum(hh_m),label="Proportion of 11+person households of male-headed households"),
              
              HH14=labelled(x=sum(ifelse((hh_f==1 &hh_per1==1),hh,0))/sum(hh_f),label="Proportion of 1-person households of female-headed households"),
              HH15=labelled(x=sum(ifelse((hh_f==1 &hh_per1==2),hh,0))/sum(hh_f),label="Proportion of 2-person households of female-headed households"),
              HH16=labelled(x=sum(ifelse((hh_f==1 &hh_per1==3),hh,0))/sum(hh_f),label="Proportion of 3-person households of female-headed households"),
              HH17=labelled(x=sum(ifelse((hh_f==1 &hh_per1==4),hh,0))/sum(hh_f),label="Proportion of 4-person households of female-headed households"),
              HH18=labelled(x=sum(ifelse((hh_f==1 &hh_per1==5),hh,0))/sum(hh_f),label="Proportion of 5-person households of female-headed households"),
              HH19=labelled(x=sum(ifelse((hh_f==1 &hh_per1==6),hh,0))/sum(hh_f),label="Proportion of 6-person households of female-headed households"),
              HH20=labelled(x=sum(ifelse((hh_f==1 &hh_per1==7),hh,0))/sum(hh_f),label="Proportion of 7-person households of female-headed households"),
              HH21=labelled(x=sum(ifelse((hh_f==1 &hh_per1==8),hh,0))/sum(hh_f),label="Proportion of 8-person households of female-headed households"),
              HH22=labelled(x=sum(ifelse((hh_f==1 &hh_per1==9),hh,0))/sum(hh_f),label="Proportion of 9-person households of female-headed households"),
              HH23=labelled(x=sum(ifelse((hh_f==1 &hh_per1==10),hh,0))/sum(hh_f),label="Proportion of 10-person households of female-headed households"),
              HH24=labelled(x=sum(ifelse((hh_f==1 &hh_per1>10),hh,0))/sum(hh_f),label="Proportion of 11+person households of female-headed households"),
              
              HH25=labelled(x=sum(ifelse((hh_m==1 &family_type2==10),1,0)/sum(hh_m)),label="Proportion of unipersonal households based on relationship to the head of male-headed households"),
              HH26=labelled(x=sum(ifelse((hh_m==1 &family_type2==20),1,0)/sum(hh_m)),label="Proportion of nuclear households based on relationship to the head of male-headed households"),
              HH27=labelled(x=sum(ifelse((hh_m==1 &family_type2==40),1,0)/sum(hh_m)),label="Proportion of stem-family households based on relationship to the head of male-headed households"),
              HH28=labelled(x=sum(ifelse((hh_m==1 &family_type2==50),1,0)/sum(hh_m)),label="Proportion of Other family households based on relationship to the head of male-headed households"),
              HH29=labelled(x=sum(ifelse((hh_m==1 &family_type2==30),1,0)/sum(hh_m)),label="Proportion of Other non family households based on relationship to the head of male-headed households"),
              HH30=labelled(x=sum(ifelse((hh_m==1 &family_type2==9999),1,0)/sum(hh_m)),label="Proportion of other relative or non relative households based on relationship to the head of male-headed households"),
              
              HH31=labelled(x=sum(ifelse((hh_f==1 &family_type2==10),1,0)/sum(hh_f)),label="Proportion of unipersonal households based on relationship to the head of female-headed households"),
              HH32=labelled(x=sum(ifelse((hh_f==1 &family_type2==20),1,0)/sum(hh_f)),label="Proportion of nuclear households based on relationship to the head of frmale-headed households"),
              HH33=labelled(x=sum(ifelse((hh_f==1 &family_type2==40),1,0)/sum(hh_f)),label="Proportion of stem-family households based on relationship to the head of female-headed households"),
              HH34=labelled(x=sum(ifelse((hh_f==1 &family_type2==50),1,0)/sum(hh_f)),label="Proportion of Other family households based on relationship to the head of female-headed households"),
              HH35=labelled(x=sum(ifelse((hh_f==1 &family_type2==30),1,0)/sum(hh_f)),label="Proportion of Other non family households based on relationship to the head of female-headed households"),
              HH36=labelled(x=sum(ifelse((hh_f==1 &family_type2==9999),1,0)/sum(hh_f)),label="Proportion of other relative or non relative households based on relationship to the head of female-headed households"),
              
              HH37=labelled(x=sum(hh_per[hh_M_headed!=0])/unique(WEIGHT_AM),label="Average household size of male-headed households"),
              HH38=labelled(x=sum(hh_per[hh_F_headed!=0])/unique(WEIGHT_AF),label="Average household size of female-headed households"),
              HH39=labelled(x=sum(hh_child18[hh_M_headed!=0])/unique(WEIGHT_AM),label="Average number of children of male-headed households"),
              HH40=labelled(x=sum(hh_child18[hh_F_headed!=0])/unique(WEIGHT_AF),label="Average number of children of female-headed households"),
              HH41=labelled(x=sum(hh_adult18[hh_M_headed!=0])/unique(WEIGHT_AM),label="Average number of adults of male-headed households"),
              HH42=labelled(x=sum(hh_adult18[hh_F_headed!=0])/unique(WEIGHT_AF),label="Average number of adults of female-headed households"),
              HH43=labelled(x=sum(hh_elderly65[hh_M_headed!=0])/unique(WEIGHT_AM),label="Average number of elderly people (aged >65) of male-headed households"),
              HH44=labelled(x=sum(hh_elderly65[hh_F_headed!=0])/unique(WEIGHT_AF),label="Average number of elderly people (aged >65) of female-headed households"),
              
              HH45=labelled(x=sum(hh_spouses[hh_M_headed!=0])/unique(WEIGHT_AM),label="Average number of spouses of male-headed households"),
              HH46=labelled(x=sum(hh_spouses[hh_F_headed!=0])/unique(WEIGHT_AF),label="Average number of spouses of female-headed households"),
              HH47=labelled(x=sum(hh_childs[hh_M_headed!=0])/unique(WEIGHT_AM),label="Average number of childs of male-headed households"),
              HH48=labelled(x=sum(hh_childs[hh_F_headed!=0])/unique(WEIGHT_AF),label="Average number of childs of female-headed households"),
              HH49=labelled(x=sum(hh_other_relatives[hh_M_headed!=0])/unique(WEIGHT_AM),label="Average number of other relatives of male-headed households"),
              HH50=labelled(x=sum(hh_other_relatives[hh_F_headed!=0])/unique(WEIGHT_AF),label="Average number of other relatives of female-headed households"),
              HH51=labelled(x=sum(hh_non_relatives[hh_M_headed!=0])/unique(WEIGHT_AM),label="Average number of other non relatives of male-headed households"),
              HH52=labelled(x=sum(hh_non_relatives[hh_F_headed!=0])/unique(WEIGHT_AF),label="Average number of other non relatives of female-headed households"),
              
              HH53=labelled(x=sum(hh_adult18M[hh_M_headed!=0])/unique(WEIGHT_AM),label="Average number of males adults in male-headed households"),
              HH54=labelled(x=sum(hh_adult18M[hh_F_headed!=0])/unique(WEIGHT_AF),label="Average number of males adults in female-headed households"),
              HH55=labelled(x=sum(hh_adult18F[hh_M_headed!=0])/unique(WEIGHT_AM),label="Average number of female adults in male-headed households"),
              HH56=labelled(x=sum(hh_adult18F[hh_F_headed!=0])/unique(WEIGHT_AF),label="Average number of female adults in female-headed households")
    )
  
})




# STEP 8 LIST  #####
l1 <- agrr_database_list_silc[sapply(agrr_database_list_silc, function(x) length(x$DB040) == 1)]
names(l1)
agrr_database_silc <- do.call("rbind", agrr_database_list_silc)

regionw<-as.data.frame(regionw)
agrr_database_silc<-as.data.frame(agrr_database_silc)
agrr_database_silc<- data.frame(agrr_database_silc,
                                regionw[match(agrr_database_silc[,"ISO3"],
                                              regionw[,"ISO3"]),c("EU Member States")])

colnames(agrr_database_silc)[grep(names(agrr_database_silc %>% select(last_col())), colnames(agrr_database_silc))]<-"CNTRY"


clipr::write_clip(agrr_database_silc)
save(agrr_database_silc, file="G:\\Shared drives\\CORESIDENCE\\TEAM FOLDERS\\Juan Galeano\\AGGR_INDICATORS_SILC_SUBNATIONAL_HARMONIZED.RData")





# AUSTRALIA #####
# READ AUSTRALIAN DATA #####

df01 <- read_sav("G:/Shared drives/CORESIDENCE/WP2_DATA/2_1_Input Data/OTHERS/HILDA/Eperson a210c.sav")
df02 <- read_sav("G:/Shared drives/CORESIDENCE/WP2_DATA/2_1_Input Data/OTHERS/HILDA/Eperson b210c.sav")
df03 <- read_sav("G:/Shared drives/CORESIDENCE/WP2_DATA/2_1_Input Data/OTHERS/HILDA/Eperson c210c.sav")
df04 <- read_sav("G:/Shared drives/CORESIDENCE/WP2_DATA/2_1_Input Data/OTHERS/HILDA/Eperson d210c.sav")
df05 <- read_sav("G:/Shared drives/CORESIDENCE/WP2_DATA/2_1_Input Data/OTHERS/HILDA/Eperson e210c.sav")
df06 <- read_sav("G:/Shared drives/CORESIDENCE/WP2_DATA/2_1_Input Data/OTHERS/HILDA/Eperson f210c.sav")
df07 <- read_sav("G:/Shared drives/CORESIDENCE/WP2_DATA/2_1_Input Data/OTHERS/HILDA/Eperson g210c.sav")
df08 <- read_sav("G:/Shared drives/CORESIDENCE/WP2_DATA/2_1_Input Data/OTHERS/HILDA/Eperson h210c.sav")
df09 <- read_sav("G:/Shared drives/CORESIDENCE/WP2_DATA/2_1_Input Data/OTHERS/HILDA/Eperson i210c.sav")
df10 <- read_sav("G:/Shared drives/CORESIDENCE/WP2_DATA/2_1_Input Data/OTHERS/HILDA/Eperson j210c.sav")
df11 <- read_sav("G:/Shared drives/CORESIDENCE/WP2_DATA/2_1_Input Data/OTHERS/HILDA/Eperson k210c.sav")
df12 <- read_sav("G:/Shared drives/CORESIDENCE/WP2_DATA/2_1_Input Data/OTHERS/HILDA/Eperson l210c.sav")
df13 <- read_sav("G:/Shared drives/CORESIDENCE/WP2_DATA/2_1_Input Data/OTHERS/HILDA/Eperson m210c.sav")
df14 <- read_sav("G:/Shared drives/CORESIDENCE/WP2_DATA/2_1_Input Data/OTHERS/HILDA/Eperson n210c.sav")
df15 <- read_sav("G:/Shared drives/CORESIDENCE/WP2_DATA/2_1_Input Data/OTHERS/HILDA/Eperson o210c.sav")
df16 <- read_sav("G:/Shared drives/CORESIDENCE/WP2_DATA/2_1_Input Data/OTHERS/HILDA/Eperson p210c.sav")
df17 <- read_sav("G:/Shared drives/CORESIDENCE/WP2_DATA/2_1_Input Data/OTHERS/HILDA/Eperson q210c.sav")
df18 <- read_sav("G:/Shared drives/CORESIDENCE/WP2_DATA/2_1_Input Data/OTHERS/HILDA/Eperson r210c.sav")
df19 <- read_sav("G:/Shared drives/CORESIDENCE/WP2_DATA/2_1_Input Data/OTHERS/HILDA/Eperson s210c.sav")
df20 <- read_sav("G:/Shared drives/CORESIDENCE/WP2_DATA/2_1_Input Data/OTHERS/HILDA/Eperson t210c.sav")
df21 <- read_sav("G:/Shared drives/CORESIDENCE/WP2_DATA/2_1_Input Data/OTHERS/HILDA/Eperson u210c.sav")


df01<-df01%>%select(ahhrhid,ahhrpid,ahhpno,ahhstate,ahgsex,ahgage,arg01,ahhwte, ahhwtes,ahhtype,ahhrih,ahhprtid,ahhfid,ahhmid)      #  ahghhm,# alnwte,
df02<-df02%>%select(bhhrhid,bhhrpid,bhhpno,bhhstate,bhgsex,bhgage,brg01,bhhwte, bhhwtes,bhhtype,bhhrih,bhhprtid,bhhfid,bhhmid)      #  ahghhm,# alnwte,
df03<-df03%>%select(chhrhid,chhrpid,chhpno,chhstate,chgsex,chgage,crg01,chhwte, chhwtes,chhtype,chhrih,chhprtid,chhfid,chhmid)      #  ahghhm,# alnwte,
df04<-df04%>%select(dhhrhid,dhhrpid,dhhpno,dhhstate,dhgsex,dhgage,drg01,dhhwte, dhhwtes,dhhtype,dhhrih,dhhprtid,dhhfid,dhhmid)      #  ahghhm,# alnwte,
df05<-df05%>%select(ehhrhid,ehhrpid,ehhpno,ehhstate,ehgsex,ehgage,erg01,ehhwte, ehhwtes,ehhtype,ehhrih,ehhprtid,ehhfid,ehhmid)      #  ahghhm,# alnwte,
df06<-df06%>%select(fhhrhid,fhhrpid,fhhpno,fhhstate,fhgsex,fhgage,frg01,fhhwte, fhhwtes,fhhtype,fhhrih,fhhprtid,fhhfid,fhhmid)      #  ahghhm,# alnwte,
df07<-df07%>%select(ghhrhid,ghhrpid,ghhpno,ghhstate,ghgsex,ghgage,grg01,ghhwte, ghhwtes,ghhtype,ghhrih,ghhprtid,ghhfid,ghhmid)      #  ahghhm,# alnwte,
df08<-df08%>%select(hhhrhid,hhhrpid,hhhpno,hhhstate,hhgsex,hhgage,hrg01,hhhwte, hhhwtes,hhhtype,hhhrih,hhhprtid,hhhfid,hhhmid)      #  ahghhm,# alnwte,
df09<-df09%>%select(ihhrhid,ihhrpid,ihhpno,ihhstate,ihgsex,ihgage,irg01,ihhwte, ihhwtes,ihhtype,ihhrih,ihhprtid,ihhfid,ihhmid)      #  ahghhm,# alnwte,
df10<-df10%>%select(jhhrhid,jhhrpid,jhhpno,jhhstate,jhgsex,jhgage,jrg01,jhhwte, jhhwtes,jhhtype,jhhrih,jhhprtid,jhhfid,jhhmid)      #  ahghhm,# alnwte,
df11<-df11%>%select(khhrhid,khhrpid,khhpno,khhstate,khgsex,khgage,krg01,khhwte, khhwtes,khhtype,khhrih,khhprtid,khhfid,khhmid)      #  ahghhm,# alnwte,
df12<-df12%>%select(lhhrhid,lhhrpid,lhhpno,lhhstate,lhgsex,lhgage,lrg01,lhhwte, lhhwtes,lhhtype,lhhrih,lhhprtid,lhhfid,lhhmid)      #  ahghhm,# alnwte,
df13<-df13%>%select(mhhrhid,mhhrpid,mhhpno,mhhstate,mhgsex,mhgage,mrg01,mhhwte, mhhwtes,mhhtype,mhhrih,mhhprtid,mhhfid,mhhmid)      #  ahghhm,# alnwte,
df14<-df14%>%select(nhhrhid,nhhrpid,nhhpno,nhhstate,nhgsex,nhgage,nrg01,nhhwte, nhhwtes,nhhtype,nhhrih,nhhprtid,nhhfid,nhhmid)      #  ahghhm,# alnwte,
df15<-df15%>%select(ohhrhid,ohhrpid,ohhpno,ohhstate,ohgsex,ohgage,org01,ohhwte, ohhwtes,ohhtype,ohhrih,ohhprtid,ohhfid,ohhmid)      #  ahghhm,# alnwte,
df16<-df16%>%select(phhrhid,phhrpid,phhpno,phhstate,phgsex,phgage,prg01,phhwte, phhwtes,phhtype,phhrih,phhprtid,phhfid,phhmid)      #  ahghhm,# alnwte,
df17<-df17%>%select(qhhrhid,qhhrpid,qhhpno,qhhstate,qhgsex,qhgage,qrg01,qhhwte, qhhwtes,qhhtype,qhhrih,qhhprtid,qhhfid,qhhmid)      #  ahghhm,# alnwte,
df18<-df18%>%select(rhhrhid,rhhrpid,rhhpno,rhhstate,rhgsex,rhgage,rrg01,rhhwte, rhhwtes,rhhtype,rhhrih,rhhprtid,rhhfid,rhhmid)      #  ahghhm,# alnwte,
df19<-df19%>%select(shhrhid,shhrpid,shhpno,shhstate,shgsex,shgage,srg01,shhwte, shhwtes,shhtype,shhrih,shhprtid,shhfid,shhmid)      #  ahghhm,# alnwte,
df20<-df20%>%select(thhrhid,thhrpid,thhpno,thhstate,thgsex,thgage,trg01,thhwte, thhwtes,thhtype,thhrih,thhprtid,thhfid,thhmid)      #  ahghhm,# alnwte,
df21<-df21%>%select(uhhrhid,uhhrpid,uhhpno,uhhstate,uhgsex,uhgage,urg01,uhhwte, uhhwtes,uhhtype,uhhrih,uhhprtid,uhhfid,uhhmid)      #  ahghhm,# alnwte,


df01$YEAR<-2001
df02$YEAR<-2002
df03$YEAR<-2003
df04$YEAR<-2004
df05$YEAR<-2005
df06$YEAR<-2006
df07$YEAR<-2007
df08$YEAR<-2008
df09$YEAR<-2009
df10$YEAR<-2010
df11$YEAR<-2011
df12$YEAR<-2012
df13$YEAR<-2013
df14$YEAR<-2014
df15$YEAR<-2015
df16$YEAR<-2016
df17$YEAR<-2017
df18$YEAR<-2018
df19$YEAR<-2019
df20$YEAR<-2020
df21$YEAR<-2021





# CREATE A LIST #####

sample_AUS_LIST<-list(df01,df02,df03,df04,df05,df06,df07,df08,df09,df10,df11,df12,df13,df14,df15,df16,df17,df18,df19,df20,df21)

# CORE LIST #####

CORE_AUS_LIST<-lapply(sample_AUS_LIST, function(df) {
  
  colnames(df)<-c("hhrhid","hhrpid","hhpno","hhstate","hgsex","hgage","rg01","hhwte","hhwtes","hhtype","hhrih","hhprtid","hhfid","hhmid","YEAR")  
  
  #df<-df %>% drop_na(rg01)
  df$rg01[is.na(df$rg01)] <- 6000
  df$rg01 <- ifelse(df$rg01==-1,6000,df$rg01 ) 
  
  
  df$AGE <- df$hgage 
  df$CNTRY <-"Australia"
  df$CNTRY_ISO <-"AUS"
  df$CONTINENT<-"Australia and New Zealand"
  df$CONTINENT2<-"OCEANIA"
  df$SOURCE<-"HILDA"
  df$SAMPLE<-with(df,paste(CNTRY_ISO, YEAR, SOURCE, sep="_")) 
  
  df$PWEIGHT_A<-df$hhwte
  
  df<-df%>%filter(hhtype!=25)
  
  df$AGE2<-with(df,  ifelse(AGE==999, "999",
                            ifelse(AGE<=4, "0-4",
                                   ifelse(AGE<=9, "5-9", 
                                          ifelse(AGE<=14, "10-14",
                                                 ifelse(AGE<=19, "15-19",
                                                        ifelse(AGE<=24, "20-24",
                                                               ifelse(AGE<=29, "25-29",
                                                                      ifelse(AGE<=34, "30-34",
                                                                             ifelse(AGE<=39, "35-39",
                                                                                    ifelse(AGE<=44, "40-44",
                                                                                           ifelse(AGE<=49, "45-49",
                                                                                                  ifelse(AGE<=54, "50-54",
                                                                                                         ifelse(AGE<=59, "55-59",
                                                                                                                ifelse(AGE<=64, "60-64",
                                                                                                                       ifelse(AGE<=69, "65-69",
                                                                                                                              ifelse(AGE<=74, "70-74",
                                                                                                                                     ifelse(AGE<=79, "75-79",
                                                                                                                                            ifelse(AGE<=84, "80-84",
                                                                                                                                                   ifelse(AGE<=89, "85-89",
                                                                                                                                                          ifelse(AGE<=94, "90-94","95+")))))))))))))))))))))
  
  
  df$CENSUS_ROUND<-with(df,ifelse((YEAR>=1956 & YEAR <=1965), 1960,
                                  ifelse((YEAR>=1966 & YEAR <=1975), 1970,
                                         ifelse((YEAR>=1976 & YEAR <=1985), 1980,
                                                ifelse((YEAR>=1986 & YEAR <=1995), 1990,
                                                       ifelse((YEAR>=1996 & YEAR <=2005), 2000,
                                                              ifelse((YEAR>=2006 & YEAR <=2015), 2010,
                                                                     ifelse((YEAR>=2016 & YEAR <=2025), 2020,0))))))))
  
  df$DECADE<-with(df,ifelse((YEAR>=1951 & YEAR <=1960), 1950,
                            ifelse((YEAR>=1961 & YEAR <=1970), 1960,
                                   ifelse((YEAR>=1971 & YEAR <=1980), 1970,
                                          ifelse((YEAR>=1981 & YEAR <=1990), 1980,
                                                 ifelse((YEAR>=1991 & YEAR <=2000), 1990,
                                                        ifelse((YEAR>=2001 & YEAR <=2010), 2000,
                                                               ifelse((YEAR>=2011 & YEAR <=2020), 2010,
                                                                      ifelse((YEAR>=2021 & YEAR <=2030), 2020,0)))))))))
  
  df$FIVE_YEAR<-with(df,ifelse((YEAR>=1951 & YEAR <=1955), 1951,
                               ifelse((YEAR>=1956 & YEAR <=1960), 1956,
                                      ifelse((YEAR>=1961 & YEAR <=1965), 1961,
                                             ifelse((YEAR>=1966 & YEAR <=1970), 1966,
                                                    ifelse((YEAR>=1971 & YEAR <=1975), 1971,
                                                           ifelse((YEAR>=1976 & YEAR <=1980), 1976,
                                                                  ifelse((YEAR>=1981 & YEAR <=1985), 1981,
                                                                         ifelse((YEAR>=1986 & YEAR <=1990), 1986,
                                                                                ifelse((YEAR>=1991 & YEAR <=1995), 1991,
                                                                                       ifelse((YEAR>=1996 & YEAR <=2000), 1996,
                                                                                              ifelse((YEAR>=2001 & YEAR <=2005), 2001,
                                                                                                     ifelse((YEAR>=2006 & YEAR <=2010), 2006,
                                                                                                            ifelse((YEAR>=2011 & YEAR <=2015), 2011,
                                                                                                                   ifelse((YEAR>=2016 & YEAR <=2020), 2016,
                                                                                                                          ifelse((YEAR>=2021 & YEAR <=2025), 2021,0))))))))))))))))
  
  
  
  
  
  
  
  df<-df%>%
    mutate(RELATE=case_when(rg01 == 0 ~ 1, #
                            rg01 == 1 ~ 2, #
                            rg01 == 2 ~ 2, #
                            rg01 == 3 ~ 5, #
                            rg01 == 4 ~ 3, #
                            rg01 == 5 ~ 3, #
                            rg01 == 6 ~ 3, #
                            rg01 == 7 ~ 4, #
                            rg01 == 8 ~ 4, #
                            rg01 == 9 ~ 4, #
                            rg01 == 10 ~ 4, #
                            rg01 == 11 ~ 3, #
                            rg01 == 12 ~ 4, #
                            rg01 == 13 ~ 4, #
                            rg01 == 14 ~ 4, #
                            rg01 == 15 ~ 4, #
                            rg01 == 16 ~ 4, #
                            rg01 == 17 ~ 4, #
                            rg01 == 18 ~ 4, #
                            rg01 == 19 ~ 5, #
                            rg01 == 6000 ~ 6)) #
  
  
  df$RELATE<-labelled(x=df$RELATE,
                      labels=c("Head"=1, 
                               "Spouse/partner"=2,
                               "Child"=3, 
                               "Other relative"=4,
                               "Non-relative"=5,
                               "Other relative or non-relative"=6,
                               "Missing" = 8,
                               "Unknown"=9),
                      label="Relationship to household head [general version]")         
  
  df<-df%>%
    mutate(RELATED=case_when(rg01 == 0 ~ 1000, #
                             rg01 == 1 ~ 2100, #
                             rg01 == 2 ~ 2200, #
                             rg01 == 3 ~ 5130, #
                             rg01 == 4 ~ 3100, #
                             rg01 == 5 ~ 3300, #
                             rg01 == 6 ~ 3200, #
                             rg01 == 7 ~ 4210, #
                             rg01 == 8 ~ 4211, #
                             rg01 == 9 ~ 4211, #
                             rg01 == 10 ~ 4220, #
                             rg01 == 11 ~ 3400, #
                             rg01 == 12 ~ 4100, #
                             rg01 == 13 ~ 4500, #
                             rg01 == 14 ~ 4410, #
                             rg01 == 15 ~ 4420, #
                             rg01 == 16 ~ 4420, #
                             rg01 == 17 ~ 4420, #
                             rg01 == 18 ~ 4000, #
                             rg01 == 19 ~ 5000, #
                             rg01 == 6000 ~ 6000)) #
  
  df  
})


CORE_AUS_LIST<-lapply(CORE_AUS_LIST, function(df) {
  
  df<-df%>%mutate(S3H=case_when(
    hhstate == 1~ 036001,
    hhstate == 2~ 036002,
    hhstate == 3~ 036003,
    hhstate == 4~ 036004,
    hhstate == 5~ 036005,
    hhstate == 6~ 036006,
    hhstate == 7~ 036007,
    hhstate == 8~ 036008,
    hhstate == 9~ 036009,
    hhstate == 10~ 036010))
  df$S3H<- sprintf("%.6d",df$S3H)  
  
  df
})

# STEP 1 LIST ######

PASO1<-lapply(CORE_AUS_LIST, function(df) {
  
  df<-df %>%
    #filter(GQ %in% VALID_GQ) %>% #filtrem només els private households
    add_count(hhrhid)%>%
    mutate(child5=ifelse(AGE<=4,1,0),
           adult18=ifelse(AGE>=18,1,0),
           adult18M=ifelse((AGE>=18&hgsex==1),1,0),
           adult18F=ifelse((AGE>=18&hgsex==2),1,0),
           child18=ifelse(AGE<18,1,0),
           elderly65=ifelse(AGE>=65,1,0),
           
           age00_09=ifelse((AGE2=="0-4"|AGE2=="5-9"),1,0),
           age10_19=ifelse((AGE2=="10-14"|AGE2=="15-19"),1,0),
           age20_29=ifelse((AGE2=="20-24"|AGE2=="25-29"),1,0),
           age30_39=ifelse((AGE2=="30-34"|AGE2=="35-39"),1,0),
           age40_49=ifelse((AGE2=="40-44"|AGE2=="45-49"),1,0),
           age50_59=ifelse((AGE2=="50-54"|AGE2=="55-59"),1,0),
           age60_69=ifelse((AGE2=="60-64"|AGE2=="65-69"),1,0),
           age70_79=ifelse((AGE2=="70-74"|AGE2=="75-79"),1,0),
           age80=ifelse(AGE>=80,1,0),
           
           heads=ifelse(RELATE==1,1,0), 
           spouses=ifelse(RELATE==2,1,0),
           childs=ifelse(RELATE==3,1,0),
           other_relatives=ifelse(RELATE==4,1,0),
           non_relatives=ifelse(RELATE==5,1,0),
           other_rel_or_non_rel=ifelse(RELATE==6,1,0),
           
           grandchilds=ifelse(RELATED==4100,1,0),
           parents=ifelse(RELATED==4200,1,0),
           siblings=ifelse(RELATED==4400,1,0),
           
           M_headed=ifelse((RELATE==1 & hgsex==1),1,0),
           F_headed=ifelse((RELATE==1 & hgsex==2),1,0),
           
           WEIGHT_POP= sum(PWEIGHT_A,na.rm = TRUE)*1,
           #WEIGHT_A= sum(df[df$RELATE==1,"PWEIGHT_A"]),
           WEIGHT_A= sum(PWEIGHT_A[RELATE==1],na.rm = TRUE),
           WEIGHT_A2= sum(PWEIGHT_A[RELATE==1&n==2]),
           WEIGHT_A3= sum(PWEIGHT_A[RELATE==1&n==3]),
           WEIGHT_A4= sum(PWEIGHT_A[RELATE==1&n==4]),
           WEIGHT_A5= sum(PWEIGHT_A[RELATE==1&n==5]),
           WEIGHT_AM= sum(PWEIGHT_A[RELATE==1&hgsex==1]),
           WEIGHT_AF= sum(PWEIGHT_A[RELATE==1&hgsex==2]))  %>%
    drop_na(PWEIGHT_A)
})
gc()

# STEP 2 LIST #######  
PASO2<-lapply(PASO1, function(df) {  
  df<-df%>%
    group_by(hhrhid)%>%
    summarise(hh_per1=n(),
              hh_parents=sum(parents*PWEIGHT_A),
              hh_grandchilds=sum(grandchilds*PWEIGHT_A),
              hh_heads=sum(heads*PWEIGHT_A),
              hh_spouses=sum(spouses*PWEIGHT_A),
              hh_childs=sum(childs*PWEIGHT_A),
              hh_other_relatives=sum(other_relatives*PWEIGHT_A),
              hh_non_relatives=sum(non_relatives*PWEIGHT_A),
              hh_other_rel_or_non_rel=sum(other_rel_or_non_rel*PWEIGHT_A))%>%
    mutate(family_type= ifelse(hh_per1==1, 10,
                               ifelse(hh_spouses>0 & hh_childs>0 & hh_other_relatives ==0 & hh_non_relatives==0,20,
                                      ifelse(hh_spouses>0 & hh_childs==0 & hh_other_relatives ==0 & hh_non_relatives==0,20,
                                             ifelse(hh_spouses==0 & hh_childs>0 & hh_other_relatives ==0 & hh_non_relatives==0,20,
                                                    
                                                    ifelse(hh_spouses>0 & hh_childs>0 &  hh_other_relatives >0 & hh_non_relatives==0,31,
                                                           ifelse(hh_spouses>0 & hh_childs==0 & hh_other_relatives >0 & hh_non_relatives==0,31,
                                                                  ifelse(hh_spouses==0 & hh_childs>0 & hh_other_relatives >0 & hh_non_relatives==0,31,
                                                                         
                                                                         ifelse(hh_spouses>0 & hh_childs>0 &  hh_other_relatives ==0 & hh_non_relatives>0,32,
                                                                                ifelse(hh_spouses>0 & hh_childs==0 & hh_other_relatives ==0 & hh_non_relatives>0,32,
                                                                                       ifelse(hh_spouses==0 & hh_childs>0 & hh_other_relatives ==0 & hh_non_relatives>0,32,       
                                                                                              
                                                                                              ifelse(hh_spouses>0 & hh_childs>0 &  hh_other_relatives >0 & hh_non_relatives>0,33,
                                                                                                     ifelse(hh_spouses>0 & hh_childs==0 & hh_other_relatives >0 & hh_non_relatives>0,33,
                                                                                                            ifelse(hh_spouses==0 & hh_childs>0 & hh_other_relatives >0 & hh_non_relatives>0,33,        
                                                                                                                   
                                                                                                                   ifelse(hh_spouses==0 & hh_childs==0 &  hh_other_relatives >0 & hh_non_relatives==0,34,
                                                                                                                          ifelse(hh_spouses==0 & hh_childs==0 &  hh_other_relatives ==0 & hh_non_relatives>0,35,
                                                                                                                                 ifelse(hh_spouses==0 & hh_childs==0 &  hh_other_relatives >0 & hh_non_relatives>0,36,
                                                                                                                                        0)))))))))))))))))%>%
    mutate(family_type2=ifelse(hh_per1==1, 10,
                               ifelse(hh_spouses>0 & hh_childs>0 & hh_other_relatives ==0 & hh_non_relatives==0,20,
                                      ifelse(hh_spouses>0 & hh_childs==0 & hh_other_relatives ==0 & hh_non_relatives==0,20,
                                             ifelse(hh_spouses==0 & hh_childs>0 & hh_other_relatives ==0 & hh_non_relatives==0,20,       
                                                    ifelse(hh_non_relatives>0,30, # "Other non family",
                                                           ifelse((hh_parents>0 | hh_grandchilds>0),40,# "Stem family",
                                                                  ifelse(hh_other_relatives>0,50, 9999)))))))) #"Other family", 
  
  
  df<-df%>%select(hhrhid,family_type,family_type2)             
  df
})
# STEP 3 LIST ####
PASO3<- map2(PASO1, PASO2, ~left_join(.x, .y, by = 'hhrhid'))

# STEP 4 LIST ####
auxlist<-lapply(PASO3, function(df) {
  
  df<-df %>%
    group_by(S3H)%>%
    summarise(WEIGHT_POP= sum(PWEIGHT_A,na.rm = TRUE)*1,
              #WEIGHT_A= sum(df[df$RELATE==1,"PWEIGHT_A"]),
              WEIGHT_A= sum(PWEIGHT_A[RELATE==1]),
              WEIGHT_A2= sum(PWEIGHT_A[RELATE==1&n==2]),
              WEIGHT_A3= sum(PWEIGHT_A[RELATE==1&n==3]),
              WEIGHT_A4= sum(PWEIGHT_A[RELATE==1&n==4]),
              WEIGHT_A5= sum(PWEIGHT_A[RELATE==1&n==5]),
              WEIGHT_AM= sum(PWEIGHT_A[RELATE==1&hgsex==1]),
              WEIGHT_AF= sum(PWEIGHT_A[RELATE==1&hgsex==2]),
              
              WEIGHT_FT10= sum(PWEIGHT_A[RELATE==1&family_type==10]),
              WEIGHT_FT20= sum(PWEIGHT_A[RELATE==1&family_type==20]),
              WEIGHT_FT31= sum(PWEIGHT_A[RELATE==1&family_type==31]),
              WEIGHT_FT32= sum(PWEIGHT_A[RELATE==1&family_type==32]),
              WEIGHT_FT33= sum(PWEIGHT_A[RELATE==1&family_type==33]),
              WEIGHT_FT34= sum(PWEIGHT_A[RELATE==1&family_type==34]),
              WEIGHT_FT35= sum(PWEIGHT_A[RELATE==1&family_type==35]),
              WEIGHT_FT36= sum(PWEIGHT_A[RELATE==1&family_type==36]),
              WEIGHT_FT00= sum(PWEIGHT_A[RELATE==1&family_type==0]),
              
              WEIGHT_FT210= sum(PWEIGHT_A[RELATE==1&family_type2==10]),
              WEIGHT_FT220= sum(PWEIGHT_A[RELATE==1&family_type2==20]),
              WEIGHT_FT230= sum(PWEIGHT_A[RELATE==1&family_type2==30]),
              WEIGHT_FT240= sum(PWEIGHT_A[RELATE==1&family_type2==40]),
              WEIGHT_FT250= sum(PWEIGHT_A[RELATE==1&family_type2==50]),
              WEIGHT_FT299= sum(PWEIGHT_A[RELATE==1&family_type2==9999]))
  df
})
# STEP 5 LIST #####
PASO4<-lapply(PASO3, function(df) {
  df<-df %>% ##### STEP 4 #######
  group_by(S3H,SAMPLE,CNTRY,CNTRY_ISO,YEAR,hhrhid,CONTINENT,SOURCE,CENSUS_ROUND,DECADE,FIVE_YEAR,CONTINENT2)%>%
    summarise(hh=1,
              hh_per1=n(),
              hh_m=sum(M_headed),
              hh_f=sum(F_headed),
              hh_per=sum(hh*PWEIGHT_A),
              hh_child5=sum(child5*PWEIGHT_A),
              hh_adult18=sum(adult18*PWEIGHT_A),
              
              hh_00_09=sum(age00_09*PWEIGHT_A),
              hh_10_19=sum(age10_19*PWEIGHT_A),
              hh_20_29=sum(age20_29*PWEIGHT_A),
              hh_30_39=sum(age30_39*PWEIGHT_A),
              hh_40_49=sum(age40_49*PWEIGHT_A),
              hh_50_59=sum(age50_59*PWEIGHT_A),
              hh_60_69=sum(age60_69*PWEIGHT_A),
              hh_70_79=sum(age70_79*PWEIGHT_A),
              hh_80=sum(age80*PWEIGHT_A),
              
              hh_adult18M=sum(adult18M*PWEIGHT_A),
              hh_adult18F=sum(adult18F*PWEIGHT_A),
              
              hh_child18=sum(child18*PWEIGHT_A),
              hh_elderly65=sum(elderly65*PWEIGHT_A),
              
              hh_heads=sum(heads*PWEIGHT_A),
              hh_spouses=sum(spouses*PWEIGHT_A),
              hh_childs=sum(childs*PWEIGHT_A),
              hh_other_relatives=sum(other_relatives*PWEIGHT_A),
              hh_non_relatives=sum(non_relatives*PWEIGHT_A),
              hh_other_rel_or_non_rel=sum(other_rel_or_non_rel*PWEIGHT_A),
              
              hh_grandchilds=sum(grandchilds*PWEIGHT_A),
              hh_parents=sum(parents*PWEIGHT_A),
              hh_siblings=sum(siblings*PWEIGHT_A), 
              hh_M_headed=sum(M_headed*PWEIGHT_A),
              
              hh_F_headed=sum(F_headed*PWEIGHT_A))%>%
    mutate(family_type= ifelse(hh_per1==1, 10,
                               ifelse(hh_spouses>0 & hh_childs>0 & hh_other_relatives ==0 & hh_non_relatives==0,20,
                                      ifelse(hh_spouses>0 & hh_childs==0 & hh_other_relatives ==0 & hh_non_relatives==0,20,
                                             ifelse(hh_spouses==0 & hh_childs>0 & hh_other_relatives ==0 & hh_non_relatives==0,20,
                                                    
                                                    ifelse(hh_spouses>0 & hh_childs>0 &  hh_other_relatives >0 & hh_non_relatives==0,31,
                                                           ifelse(hh_spouses>0 & hh_childs==0 & hh_other_relatives >0 & hh_non_relatives==0,31,
                                                                  ifelse(hh_spouses==0 & hh_childs>0 & hh_other_relatives >0 & hh_non_relatives==0,31,
                                                                         
                                                                         ifelse(hh_spouses>0 & hh_childs>0 &  hh_other_relatives ==0 & hh_non_relatives>0,32,
                                                                                ifelse(hh_spouses>0 & hh_childs==0 & hh_other_relatives ==0 & hh_non_relatives>0,32,
                                                                                       ifelse(hh_spouses==0 & hh_childs>0 & hh_other_relatives ==0 & hh_non_relatives>0,32,       
                                                                                              
                                                                                              ifelse(hh_spouses>0 & hh_childs>0 &  hh_other_relatives >0 & hh_non_relatives>0,33,
                                                                                                     ifelse(hh_spouses>0 & hh_childs==0 & hh_other_relatives >0 & hh_non_relatives>0,33,
                                                                                                            ifelse(hh_spouses==0 & hh_childs>0 & hh_other_relatives >0 & hh_non_relatives>0,33,        
                                                                                                                   
                                                                                                                   ifelse(hh_spouses==0 & hh_childs==0 &  hh_other_relatives >0 & hh_non_relatives==0,34,
                                                                                                                          ifelse(hh_spouses==0 & hh_childs==0 &  hh_other_relatives ==0 & hh_non_relatives>0,35,
                                                                                                                                 ifelse(hh_spouses==0 & hh_childs==0 &  hh_other_relatives >0 & hh_non_relatives>0,36,
                                                                                                                                        0)))))))))))))))))%>%
    mutate(family_type2=ifelse(hh_per1==1, 10,
                               ifelse(hh_spouses>0 & hh_childs>0 & hh_other_relatives ==0 & hh_non_relatives==0,20,
                                      ifelse(hh_spouses>0 & hh_childs==0 & hh_other_relatives ==0 & hh_non_relatives==0,20,
                                             ifelse(hh_spouses==0 & hh_childs>0 & hh_other_relatives ==0 & hh_non_relatives==0,20,       
                                                    ifelse(hh_non_relatives>0,30, # "Other non family",
                                                           ifelse((hh_parents>0 | hh_grandchilds>0),40,# "Stem family",
                                                                  ifelse(hh_other_relatives>0,50, 9999))))))))
  
  
  
  df
})

# STEP 6 LIST  #####
final<-Map(left_join, PASO4,auxlist, by="S3H")
gc()

# STEP 7 LIST ######
agrr_database_list_aus<-lapply(final, function(df) {
  # df$REGION<-as.character(df$REGION)
  df<-df %>%
    group_by(S3H,CNTRY_ISO,YEAR,SOURCE,SAMPLE,CONTINENT,CONTINENT2,CENSUS_ROUND,DECADE,FIVE_YEAR)%>%
    summarise(WPOP=labelled(x=round(unique(WEIGHT_POP),0),label="Weighted population in sample"),
              
              HS01=labelled(x=sum(ifelse(hh_per1==1,hh,0))/sum(hh),label="Proportion of 1-person households"),
              HS02=labelled(x=sum(ifelse(hh_per1==2,hh,0))/sum(hh),label="Proportion of 2-person households"),
              HS03=labelled(x=sum(ifelse(hh_per1==3,hh,0))/sum(hh),label="Proportion of 3-person households"),
              HS04=labelled(x=sum(ifelse(hh_per1==4,hh,0))/sum(hh),label="Proportion of 4-person households"),
              HS05=labelled(x=sum(ifelse(hh_per1==5,hh,0))/sum(hh),label="Proportion of 5-person households"),
              HS06=labelled(x=sum(ifelse(hh_per1==6,hh,0))/sum(hh),label="Proportion of 6-person households"),
              HS07=labelled(x=sum(ifelse(hh_per1==7,hh,0))/sum(hh),label="Proportion of 7-person households"),
              HS08=labelled(x=sum(ifelse(hh_per1==8,hh,0))/sum(hh),label="Proportion of 8-person households"),
              HS09=labelled(x=sum(ifelse(hh_per1==9,hh,0))/sum(hh),label="Proportion of 9-person households"),
              HS10=labelled(x=sum(ifelse(hh_per1==10,hh,0))/sum(hh),label="Proportion of 10-person households"),
              HS11=labelled(x=sum(ifelse(hh_per1>10,hh,0))/sum(hh),label="Proportion of 11+person households"),
              
              HS12=labelled(x=sum(hh_child5 != 0)/sum(hh),label="Proportion of households with at least one person below 0-4 years old"),
              HS13=labelled(x=sum(hh_elderly65 != 0)/sum(hh),label="Proportion of households with at least one person 65+"),
              
              HS14=labelled(x=sum(hh_per)/unique(WEIGHT_A),label="Average household size"),
              HS15=labelled(x=sum(hh_child5)/unique(WEIGHT_A),label="Average number of 0-4 children in the household"),
              HS16=labelled(x=sum(hh_adult18)/unique(WEIGHT_A),label="Average number of adults in the household (aged 18+)"),
              HS17=labelled(x=sum(hh_child18)/unique(WEIGHT_A),label="Average number of children in the household (aged < 18)"),
              HS18=labelled(x=sum(hh_elderly65)/unique(WEIGHT_A), label="Average number of elderly people in the household (aged >65)"),
              
              HS19=labelled(x=sum(hh_00_09)/unique(WEIGHT_A),label="Average number of 0-9 individuals in the household"),
              HS20=labelled(x=sum(hh_10_19)/unique(WEIGHT_A),label="Average number of 10-19 individuals in the household"),
              HS21=labelled(x=sum(hh_20_29)/unique(WEIGHT_A),label="Average number of 20-29 individuals in the household"),
              HS22=labelled(x=sum(hh_30_39)/unique(WEIGHT_A),label="Average number of 30-39 individuals in the household"),
              HS23=labelled(x=sum(hh_40_49)/unique(WEIGHT_A),label="Average number of 40-49 individuals in the household"),
              HS24=labelled(x=sum(hh_50_59)/unique(WEIGHT_A),label="Average number of 50-59 individuals in the household"),
              HS25=labelled(x=sum(hh_60_69)/unique(WEIGHT_A),label="Average number of 60-69 individuals in the household"),
              HS26=labelled(x=sum(hh_70_79)/unique(WEIGHT_A),label="Average number of 70-79 individuals in the household"),
              HS27=labelled(x=sum(hh_80)/unique(WEIGHT_A),label="Average number of 80+ individuals in the household"),
              
              HS28=labelled(x=sum(ifelse((hh_per1==2|hh_per1==3),hh,0))/sum(hh),label="Proportion of 2-3 persons households"),
              HS29=labelled(x=sum(ifelse((hh_per1==4|hh_per1==5),hh,0))/sum(hh),label="Proportion of 4-5 persons households"),
              HS30=labelled(x=sum(ifelse(hh_per1>6,hh,0))/sum(hh),label="Proportion of 6+ person households"),
              
              HR01=labelled(x=sum(hh_heads)/unique(WEIGHT_A),label="Average number of Heads in the household"),
              HR02=labelled(x=sum(hh_spouses)/unique(WEIGHT_A),label="Average number of Spouses in the household"),
              HR03=labelled(x=sum(hh_childs)/unique(WEIGHT_A),label="Average number of Children in the household"),
              HR04=labelled(x=sum(hh_other_relatives)/unique(WEIGHT_A),label="Average number of other relatives in the household"),
              HR05=labelled(x=sum(hh_non_relatives)/unique(WEIGHT_A),label="Average number of non relatives in the household"),
              HR06=labelled(x=sum(hh_other_rel_or_non_rel)/unique(WEIGHT_A),label="Average number of relative or non relatives in the household"),
              
              HR07=labelled(x=sum(hh_heads[hh_per1==2])/unique(WEIGHT_A2) ,label="Average number of Heads in 2 persons household"),
              HR08=labelled(x=sum(hh_spouses[hh_per1==2])/unique(WEIGHT_A2) ,label="Average number of Spouses in 2 persons household"),
              HR09=labelled(x=sum(hh_childs[hh_per1==2])/unique(WEIGHT_A2) ,label="Average number of Children in 2 persons household"),
              HR10=labelled(x=sum(hh_other_relatives[hh_per1==2])/unique(WEIGHT_A2) ,label="Average number of Other relatives in 2 persons household"),
              HR11=labelled(x=sum(hh_non_relatives[hh_per1==2])/unique(WEIGHT_A2) ,label="Average number of non relatives in 2 persons household"),
              HR12=labelled(x=sum(hh_other_rel_or_non_rel[hh_per1==2])/unique(WEIGHT_A2) ,label="Average number of of relative or non relatives in 2 persons household"),
              
              HR13=labelled(x=sum(hh_heads[hh_per1==3])/unique(WEIGHT_A3) ,label="Average number of Heads in 3 persons household"),
              HR14=labelled(x=sum(hh_spouses[hh_per1==3])/unique(WEIGHT_A3) ,label="Average number of Spouses in 3 persons household"),
              HR15=labelled(x=sum(hh_childs[hh_per1==3])/unique(WEIGHT_A3) ,label="Average number of Children in 3 persons household"),
              HR16=labelled(x=sum(hh_other_relatives[hh_per1==3])/unique(WEIGHT_A3) ,label="Average number of Other relatives in 3 persons household"),
              HR17=labelled(x=sum(hh_non_relatives[hh_per1==3])/unique(WEIGHT_A3) ,label="Average number of non relatives in 3 persons household"),
              HR18=labelled(x=sum(hh_other_rel_or_non_rel[hh_per1==3])/unique(WEIGHT_A3) ,label="Average number of of relative or non relatives in 3 persons household"),
              
              HR19=labelled(x=sum(hh_heads[hh_per1==4])/unique(WEIGHT_A4) ,label="Average number of Heads in 4 persons household"),
              HR20=labelled(x=sum(hh_spouses[hh_per1==4])/unique(WEIGHT_A4) ,label="Average number of Spouses in 4 persons household"),
              HR21=labelled(x=sum(hh_childs[hh_per1==4])/unique(WEIGHT_A4) ,label="Average number of Children in 4 persons household"),
              HR22=labelled(x=sum(hh_other_relatives[hh_per1==4])/unique(WEIGHT_A4) ,label="Average number of Other relatives in 4 persons household"),
              HR23=labelled(x=sum(hh_non_relatives[hh_per1==4])/unique(WEIGHT_A4) ,label="Average number of non relatives in 4 persons household"),
              HR24=labelled(x=sum(hh_other_rel_or_non_rel[hh_per1==4])/unique(WEIGHT_A4) ,label="Average number of of relative or non relatives in 4 persons household"),
              
              HR25=labelled(x=sum(hh_heads[hh_per1==5])/unique(WEIGHT_A5) ,label="Average number of Heads in 5 persons household"),
              HR26=labelled(x=sum(hh_spouses[hh_per1==5])/unique(WEIGHT_A5) ,label="Average number of Spouses in 5 persons household"),
              HR27=labelled(x=sum(hh_childs[hh_per1==5])/unique(WEIGHT_A5) ,label="Average number of Children in 5 persons household"),
              HR28=labelled(x=sum(hh_other_relatives[hh_per1==5])/unique(WEIGHT_A5) ,label="Average number of Other relatives in 5 persons household"),
              HR29=labelled(x=sum(hh_non_relatives[hh_per1==5])/unique(WEIGHT_A5) ,label="Average number of non relatives in 5 persons household"),
              HR30=labelled(x=sum(hh_other_rel_or_non_rel[hh_per1==5])/unique(WEIGHT_A5) ,label="Average number of of relative or non relatives in 5 persons household"),
              
              HT01=labelled(x=sum(ifelse(family_type==10,1,0)/sum(hh)),label="Proportion of unipersonal households based on relationship to the head"),
              HT02=labelled(x=sum(ifelse(family_type==20,1,0)/sum(hh)),label="Proportion of nuclear households based on relationship to the head"),
              HT03=labelled(x=sum(ifelse(family_type==31,1,0)/sum(hh)),label="Proportion of nuclear plus other relative households based on relationship to the head"),
              HT04=labelled(x=sum(ifelse(family_type==32,1,0)/sum(hh)),label="Proportion of nuclear plus other non relative households based on relationship to the head"),
              HT05=labelled(x=sum(ifelse(family_type==33,1,0)/sum(hh)),label="Proportion of nuclear plus other relative plus other non relative households based on relationship to the head"),
              HT06=labelled(x=sum(ifelse(family_type==34,1,0)/sum(hh)),label="Proportion of other relative households based on relationship to the head"),
              HT07=labelled(x=sum(ifelse(family_type==35,1,0)/sum(hh)),label="Proportion of other non relative households based on relationship to the head"),
              HT08=labelled(x=sum(ifelse(family_type==36,1,0)/sum(hh)),label="Proportion of other relative plus other non relative households based on relationship to the head"),
              HT09=labelled(x=sum(ifelse(family_type==0,1,0)/sum(hh)),label="Proportion of other relative or non relative households based on relationship to the head"),
              
              HT10=labelled(x=sum(hh_per[family_type==10])/unique(WEIGHT_FT10),label="Average size of unipersonal households based on relationship to the head"),
              HT11=labelled(x=sum(hh_per[family_type==20])/unique(WEIGHT_FT20),label="Average size of nuclear households based on relationship to the head"),
              HT12=labelled(x=sum(hh_per[family_type==31])/unique(WEIGHT_FT31),label="Average size of nuclear plus other relative households based on relationship to the head"),
              HT13=labelled(x=sum(hh_per[family_type==32])/unique(WEIGHT_FT32),label="Average size of nuclear plus other non relative households based on relationship to the head"),
              HT14=labelled(x=sum(hh_per[family_type==33])/unique(WEIGHT_FT33),label="Average size of nuclear plus other relative plus other non relative households based on relationship to the head"),
              HT15=labelled(x=sum(hh_per[family_type==34])/unique(WEIGHT_FT34),label="Average size of other relative households based on relationship to the head"),
              HT16=labelled(x=sum(hh_per[family_type==35])/unique(WEIGHT_FT35),label="Average size of other non relative households based on relationship to the head"),
              HT17=labelled(x=sum(hh_per[family_type==36])/unique(WEIGHT_FT36),label="Average size of other relative plus other non relative households based on relationship to the head"),
              HT18=labelled(x=sum(hh_per[family_type==0])/unique(WEIGHT_FT00),label="Average size of other relative or non relative households based on relationship to the head"),
              
              HT20=labelled(x=sum(ifelse(family_type2==10,1,0)/sum(hh)),label="Proportion of unipersonal households based on relationship to the head"),
              HT21=labelled(x=sum(ifelse(family_type2==20,1,0)/sum(hh)),label="Proportion of nuclear households based on relationship to the head"),
              HT22=labelled(x=sum(ifelse(family_type2==40,1,0)/sum(hh)),label="Proportion of stem-family households based on relationship to the head"),
              HT23=labelled(x=sum(ifelse(family_type2==50,1,0)/sum(hh)),label="Proportion of Other family households based on relationship to the head"),
              HT24=labelled(x=sum(ifelse(family_type2==30,1,0)/sum(hh)),label="Proportion of Other non family households based on relationship to the head"),
              HT25=labelled(x=sum(ifelse(family_type2==9999,1,0)/sum(hh)),label="Proportion of other relative or non relative households based on relationship to the head"),
              
              HT26=labelled(x=sum(hh_per[family_type2==10])/unique(WEIGHT_FT210),label="Average household size of unipersonal based on relationship to the head"),
              HT27=labelled(x=sum(hh_per[family_type2==20])/unique(WEIGHT_FT220),label="Average household size of nuclear family based on relationship to the head"),
              HT28=labelled(x=sum(hh_per[family_type2==40])/unique(WEIGHT_FT240),label="Average household size of Stem family based on relationship to the head"),
              HT29=labelled(x=sum(hh_per[family_type2==50])/unique(WEIGHT_FT250),label="Average household size of Other family based on relationship to the head"),
              HT30=labelled(x=sum(hh_per[family_type2==30])/unique(WEIGHT_FT230),label="Average household size of Other non family based on relationship to the head"),
              HT31=labelled(x=sum(hh_per[family_type2==9999])/unique(WEIGHT_FT299),label="Average household size of other relative or non relative based on relationship to the head"),
              
              HH01=labelled(x=sum(hh_M_headed[hh_M_headed!=0])/unique(WEIGHT_A),label="Proportion of male-headed households"),
              HH02=labelled(x=sum(hh_F_headed[hh_F_headed!=0])/unique(WEIGHT_A),label="Proportion of female-headed households"),
              
              HH03=labelled(x=sum(ifelse((hh_m==1 &hh_per1==1),hh,0))/sum(hh_m),label="Proportion of 1-person households of male-headed households"),
              HH04=labelled(x=sum(ifelse((hh_m==1 &hh_per1==2),hh,0))/sum(hh_m),label="Proportion of 2-person households of male-headed households"),
              HH05=labelled(x=sum(ifelse((hh_m==1 &hh_per1==3),hh,0))/sum(hh_m),label="Proportion of 3-person households of male-headed households"),
              HH06=labelled(x=sum(ifelse((hh_m==1 &hh_per1==4),hh,0))/sum(hh_m),label="Proportion of 4-person households of male-headed households"),
              HH07=labelled(x=sum(ifelse((hh_m==1 &hh_per1==5),hh,0))/sum(hh_m),label="Proportion of 5-person households of male-headed households"),
              HH08=labelled(x=sum(ifelse((hh_m==1 &hh_per1==6),hh,0))/sum(hh_m),label="Proportion of 6-person households of male-headed households"),
              HH09=labelled(x=sum(ifelse((hh_m==1 &hh_per1==7),hh,0))/sum(hh_m),label="Proportion of 7-person households of male-headed households"),
              HH10=labelled(x=sum(ifelse((hh_m==1 &hh_per1==8),hh,0))/sum(hh_m),label="Proportion of 8-person households of male-headed households"),
              HH11=labelled(x=sum(ifelse((hh_m==1 &hh_per1==9),hh,0))/sum(hh_m),label="Proportion of 9-person households of male-headed households"),
              HH12=labelled(x=sum(ifelse((hh_m==1 &hh_per1==10),hh,0))/sum(hh_m),label="Proportion of 10-person households of male-headed households"),
              HH13=labelled(x=sum(ifelse((hh_m==1 &hh_per1>10),hh,0))/sum(hh_m),label="Proportion of 11+person households of male-headed households"),
              
              HH14=labelled(x=sum(ifelse((hh_f==1 &hh_per1==1),hh,0))/sum(hh_f),label="Proportion of 1-person households of female-headed households"),
              HH15=labelled(x=sum(ifelse((hh_f==1 &hh_per1==2),hh,0))/sum(hh_f),label="Proportion of 2-person households of female-headed households"),
              HH16=labelled(x=sum(ifelse((hh_f==1 &hh_per1==3),hh,0))/sum(hh_f),label="Proportion of 3-person households of female-headed households"),
              HH17=labelled(x=sum(ifelse((hh_f==1 &hh_per1==4),hh,0))/sum(hh_f),label="Proportion of 4-person households of female-headed households"),
              HH18=labelled(x=sum(ifelse((hh_f==1 &hh_per1==5),hh,0))/sum(hh_f),label="Proportion of 5-person households of female-headed households"),
              HH19=labelled(x=sum(ifelse((hh_f==1 &hh_per1==6),hh,0))/sum(hh_f),label="Proportion of 6-person households of female-headed households"),
              HH20=labelled(x=sum(ifelse((hh_f==1 &hh_per1==7),hh,0))/sum(hh_f),label="Proportion of 7-person households of female-headed households"),
              HH21=labelled(x=sum(ifelse((hh_f==1 &hh_per1==8),hh,0))/sum(hh_f),label="Proportion of 8-person households of female-headed households"),
              HH22=labelled(x=sum(ifelse((hh_f==1 &hh_per1==9),hh,0))/sum(hh_f),label="Proportion of 9-person households of female-headed households"),
              HH23=labelled(x=sum(ifelse((hh_f==1 &hh_per1==10),hh,0))/sum(hh_f),label="Proportion of 10-person households of female-headed households"),
              HH24=labelled(x=sum(ifelse((hh_f==1 &hh_per1>10),hh,0))/sum(hh_f),label="Proportion of 11+person households of female-headed households"),
              
              HH25=labelled(x=sum(ifelse((hh_m==1 &family_type2==10),1,0)/sum(hh_m)),label="Proportion of unipersonal households based on relationship to the head of male-headed households"),
              HH26=labelled(x=sum(ifelse((hh_m==1 &family_type2==20),1,0)/sum(hh_m)),label="Proportion of nuclear households based on relationship to the head of male-headed households"),
              HH27=labelled(x=sum(ifelse((hh_m==1 &family_type2==40),1,0)/sum(hh_m)),label="Proportion of stem-family households based on relationship to the head of male-headed households"),
              HH28=labelled(x=sum(ifelse((hh_m==1 &family_type2==50),1,0)/sum(hh_m)),label="Proportion of Other family households based on relationship to the head of male-headed households"),
              HH29=labelled(x=sum(ifelse((hh_m==1 &family_type2==30),1,0)/sum(hh_m)),label="Proportion of Other non family households based on relationship to the head of male-headed households"),
              HH30=labelled(x=sum(ifelse((hh_m==1 &family_type2==9999),1,0)/sum(hh_m)),label="Proportion of other relative or non relative households based on relationship to the head of male-headed households"),
              
              HH31=labelled(x=sum(ifelse((hh_f==1 &family_type2==10),1,0)/sum(hh_f)),label="Proportion of unipersonal households based on relationship to the head of female-headed households"),
              HH32=labelled(x=sum(ifelse((hh_f==1 &family_type2==20),1,0)/sum(hh_f)),label="Proportion of nuclear households based on relationship to the head of frmale-headed households"),
              HH33=labelled(x=sum(ifelse((hh_f==1 &family_type2==40),1,0)/sum(hh_f)),label="Proportion of stem-family households based on relationship to the head of female-headed households"),
              HH34=labelled(x=sum(ifelse((hh_f==1 &family_type2==50),1,0)/sum(hh_f)),label="Proportion of Other family households based on relationship to the head of female-headed households"),
              HH35=labelled(x=sum(ifelse((hh_f==1 &family_type2==30),1,0)/sum(hh_f)),label="Proportion of Other non family households based on relationship to the head of female-headed households"),
              HH36=labelled(x=sum(ifelse((hh_f==1 &family_type2==9999),1,0)/sum(hh_f)),label="Proportion of other relative or non relative households based on relationship to the head of female-headed households"),
              
              HH37=labelled(x=sum(hh_per[hh_M_headed!=0])/unique(WEIGHT_AM),label="Average household size of male-headed households"),
              HH38=labelled(x=sum(hh_per[hh_F_headed!=0])/unique(WEIGHT_AF),label="Average household size of female-headed households"),
              HH39=labelled(x=sum(hh_child18[hh_M_headed!=0])/unique(WEIGHT_AM),label="Average number of children of male-headed households"),
              HH40=labelled(x=sum(hh_child18[hh_F_headed!=0])/unique(WEIGHT_AF),label="Average number of children of female-headed households"),
              HH41=labelled(x=sum(hh_adult18[hh_M_headed!=0])/unique(WEIGHT_AM),label="Average number of adults of male-headed households"),
              HH42=labelled(x=sum(hh_adult18[hh_F_headed!=0])/unique(WEIGHT_AF),label="Average number of adults of female-headed households"),
              HH43=labelled(x=sum(hh_elderly65[hh_M_headed!=0])/unique(WEIGHT_AM),label="Average number of elderly people (aged >65) of male-headed households"),
              HH44=labelled(x=sum(hh_elderly65[hh_F_headed!=0])/unique(WEIGHT_AF),label="Average number of elderly people (aged >65) of female-headed households"),
              
              HH45=labelled(x=sum(hh_spouses[hh_M_headed!=0])/unique(WEIGHT_AM),label="Average number of spouses of male-headed households"),
              HH46=labelled(x=sum(hh_spouses[hh_F_headed!=0])/unique(WEIGHT_AF),label="Average number of spouses of female-headed households"),
              HH47=labelled(x=sum(hh_childs[hh_M_headed!=0])/unique(WEIGHT_AM),label="Average number of childs of male-headed households"),
              HH48=labelled(x=sum(hh_childs[hh_F_headed!=0])/unique(WEIGHT_AF),label="Average number of childs of female-headed households"),
              HH49=labelled(x=sum(hh_other_relatives[hh_M_headed!=0])/unique(WEIGHT_AM),label="Average number of other relatives of male-headed households"),
              HH50=labelled(x=sum(hh_other_relatives[hh_F_headed!=0])/unique(WEIGHT_AF),label="Average number of other relatives of female-headed households"),
              HH51=labelled(x=sum(hh_non_relatives[hh_M_headed!=0])/unique(WEIGHT_AM),label="Average number of other non relatives of male-headed households"),
              HH52=labelled(x=sum(hh_non_relatives[hh_F_headed!=0])/unique(WEIGHT_AF),label="Average number of other non relatives of female-headed households"),
              
              HH53=labelled(x=sum(hh_adult18M[hh_M_headed!=0])/unique(WEIGHT_AM),label="Average number of males adults in male-headed households"),
              HH54=labelled(x=sum(hh_adult18M[hh_F_headed!=0])/unique(WEIGHT_AF),label="Average number of males adults in female-headed households"),
              HH55=labelled(x=sum(hh_adult18F[hh_M_headed!=0])/unique(WEIGHT_AM),label="Average number of female adults in male-headed households"),
              HH56=labelled(x=sum(hh_adult18F[hh_F_headed!=0])/unique(WEIGHT_AF),label="Average number of female adults in female-headed households")
    )
  
})




# STEP 8 LIST #####

agrr_database_aus <- do.call("rbind", agrr_database_list_aus)

agrr_database_aus$CNTRY<-"Australia"


save(agrr_database_aus, file="G:\\Shared drives\\CORESIDENCE\\TEAM FOLDERS\\Juan Galeano\\AGGR_INDICATORS_AUS_SUBNATIONAL_HARMONIZED.RData")

gc()








# READ ALL FIVE DATAFRAMES WITH AGGERGATED INDICATOR INDICATORS #####
load("G:/Shared drives/CORESIDENCE/TEAM FOLDERS/Juan Galeano/AGGR_INDICATORS_IPUMS_SUBNATIONAL_HARMONIZED.RData")
load("G:/Shared drives/CORESIDENCE/TEAM FOLDERS/Juan Galeano/AGGR_INDICATORS_DHS_SUBNATIONAL_HARMONIZED.RData")
load("G:/Shared drives/CORESIDENCE/TEAM FOLDERS/Juan Galeano/AGGR_INDICATORS_LFS_SUBNATIONAL_HARMONIZED.RData")
load("G:/Shared drives/CORESIDENCE/TEAM FOLDERS/Juan Galeano/AGGR_INDICATORS_SILC_SUBNATIONAL_HARMONIZED.RData")
load("G:/Shared drives/CORESIDENCE/TEAM FOLDERS/Juan Galeano/AGGR_INDICATORS_AUS_SUBNATIONAL_HARMONIZED.RData")


agrr_database_dhs<-agrr_database_dhs%>% filter(S3H!="NA")


# CHECK NAMES ######
names(agrr_database_ipums)
names(agrr_database_dhs)
names(agrr_database_lfs)
names(agrr_database_silc)
names(agrr_database_aus)


# ARRANGE COLUMNS ORDER #####
agrr_database_dhs<- agrr_database_dhs %>% relocate(CNTRY, .before = country_control)
agrr_database_lfs<- agrr_database_lfs %>% relocate(CNTRY, .before = ISO3)
agrr_database_silc<- agrr_database_silc %>% relocate(CNTRY, .before = ISO3)
agrr_database_aus<- agrr_database_aus %>% relocate(CNTRY, .before = CNTRY_ISO)

colnames(agrr_database_dhs)[3]<-"CNTRY_ISO"
colnames(agrr_database_dhs)[8]<-"CONTINENT2"
colnames(agrr_database_lfs)[3]<-"CNTRY_ISO"
colnames(agrr_database_lfs)[8]<-"CONTINENT2"
colnames(agrr_database_silc)[3]<-"CNTRY_ISO"
colnames(agrr_database_silc)[8]<-"CONTINENT2"


# GET LABELS OF COUNTRY NAMES ######
class(agrr_database_ipums$CNTRY)
class(agrr_database_dhs$CNTRY)

agrr_database_ipums$CNTRY<-as_factor(agrr_database_ipums$CNTRY)
agrr_database_ipums$CNTRY<-as.character(agrr_database_ipums$CNTRY)

ipums_sub<-agrr_database_ipums%>%  group_by(SAMPLE)%>%summarise(n=n())%>%mutate(n_rel = round(n / sum(n)*100,2))
clipr::write_clip(ipums_sub[1:2])

dhs_sub<-agrr_database_dhs%>%  group_by(SAMPLE)%>%summarise(n=n())%>%mutate(n_rel = round(n / sum(n)*100,2))
clipr::write_clip(dhs_sub[1:2])

lfs_sub<-agrr_database_lfs%>%  group_by(SAMPLE)%>%summarise(n=n())%>%mutate(n_rel = round(n / sum(n)*100,2))
clipr::write_clip(lfs_sub[1:2])

silc_sub<-agrr_database_silc%>%  group_by(SAMPLE)%>%summarise(n=n())%>%mutate(n_rel = round(n / sum(n)*100,2))
clipr::write_clip(silc_sub[1:2])

aus_sub<-agrr_database_aus%>%  group_by(SAMPLE)%>%summarise(n=n())%>%mutate(n_rel = round(n / sum(n)*100,2))
clipr::write_clip(aus_sub[1:2])


# CHECK THERE ARE NO DIFFERENCES IN COLUMNS BETWEE DATAFRAMES ######
setdiff(names(agrr_database_ipums), names(agrr_database_dhs))
setdiff(names(agrr_database_ipums), names(agrr_database_lfs))
setdiff(names(agrr_database_dhs), names(agrr_database_lfs))
setdiff(names(agrr_database_silc), names(agrr_database_ipums))
setdiff(names(agrr_database_aus), names(agrr_database_ipums))

# RBIND DATAFRAMES INTO CORESIDENCE_AGG
CORESIDENCE_AGG<-rbind(agrr_database_ipums,agrr_database_dhs,agrr_database_lfs,agrr_database_silc,agrr_database_aus)

# CORRECT ISO SURINAM ####
CORESIDENCE_AGG$CNTRY_ISO<-with(CORESIDENCE_AGG, ifelse(CNTRY=="Suriname","SUR",CNTRY_ISO))
CORESIDENCE_AGG$SAMPLE<-with(CORESIDENCE_AGG, ifelse(CNTRY=="Suriname","SUR_2012_IPUMS",SAMPLE))
CORESIDENCE_AGG$CNTRY_ISO<-with(CORESIDENCE_AGG, ifelse(CNTRY=="Greece","GRC",CNTRY_ISO))
CORESIDENCE_AGG$SAMPLE<-with(CORESIDENCE_AGG, ifelse((CNTRY=="Greece" & YEAR==2021),"GRC_2021_SILC",SAMPLE))

CORESIDENCE_AGG$SAMPLE_R<-with(CORESIDENCE_AGG, paste(SAMPLE,S3H, sep="_"))
CORESIDENCE_AGG<- CORESIDENCE_AGG %>% relocate(SAMPLE_R, .after = SAMPLE)
CORESIDENCE_AGG<- CORESIDENCE_AGG %>% relocate(S3H, .after = SAMPLE)
#CORESIDENCE_AGG<- CORESIDENCE_AGG %>% relocate(REGION_NAME, .after = REGION)

CORESIDENCE_AGG$WPOP<-NULL
names(CORESIDENCE_AGG)


library(sf)

WORLDCO <- st_read("G:\\Shared drives\\CORESIDENCE\\WP2_DATA\\2_6_Shapefiles\\CORESIDENCE_SHP_SUBNATIONAL", "A1_WORLD_SUBNATIONAL_CORESIDENCE")
colnames(WORLDCO)[4]<-"S3H"
CO_NAMES <- WORLDCO %>% st_drop_geometry()
CORESIDENCE_AGG<-as.data.frame(CORESIDENCE_AGG)
CO_NAMES<-as.data.frame(CO_NAMES)

CORESIDENCE_AGG <- data.frame(CORESIDENCE_AGG,
                              CO_NAMES[match(CORESIDENCE_AGG[,"S3H"],
                                             CO_NAMES[,"S3H"]),"ADMIN_NAME"])


colnames(CORESIDENCE_AGG)[159]<-"C6"
head(CORESIDENCE_AGG)

colnames(CORESIDENCE_AGG)[1]<-"C2"
colnames(CORESIDENCE_AGG)[2]<-"C1"
colnames(CORESIDENCE_AGG)[9]<-"C4"
colnames(CORESIDENCE_AGG)[8]<-"C3"

CORESIDENCE_AGG<- CORESIDENCE_AGG %>% relocate(C2, .after = C1)
CORESIDENCE_AGG<- CORESIDENCE_AGG %>% relocate(C3, .after = C2)
CORESIDENCE_AGG<- CORESIDENCE_AGG %>% relocate(C4, .after = C3)

colnames(CORESIDENCE_AGG)[5]<-"T1"
colnames(CORESIDENCE_AGG)[10]<-"T2"
colnames(CORESIDENCE_AGG)[12]<-"T5"
colnames(CORESIDENCE_AGG)[11]<-"T10"

CORESIDENCE_AGG<- CORESIDENCE_AGG %>% relocate(T2, .after = T1)
CORESIDENCE_AGG<- CORESIDENCE_AGG %>% relocate(T5, .after = T2)
CORESIDENCE_AGG<- CORESIDENCE_AGG %>% relocate(T10, .after = T5)

colnames(CORESIDENCE_AGG)[9]<-"S2"
colnames(CORESIDENCE_AGG)[10]<-"S1"
colnames(CORESIDENCE_AGG)[12]<-"S3"

CORESIDENCE_AGG<- CORESIDENCE_AGG %>% relocate(S2, .after = S1)
CORESIDENCE_AGG<- CORESIDENCE_AGG %>% relocate(S3, .after = S2)

colnames(CORESIDENCE_AGG)[12]<-"C5"

CORESIDENCE_AGG<- CORESIDENCE_AGG %>% relocate(C5, .after = C4)
CORESIDENCE_AGG<- CORESIDENCE_AGG %>% relocate(C6, .after = C5)


### CLEAN BASURA

CORESIDENCE_AGG<-CORESIDENCE_AGG%>% filter(C5!="616000")

CORESIDENCE_AGG$C6<-with(CORESIDENCE_AGG, ifelse(C5=="332011","Non-mappable camps",C6))
CORESIDENCE_AGG$C6<-with(CORESIDENCE_AGG, ifelse(C5=="356031","Lakshadweep",C6))
CORESIDENCE_AGG$C6<-with(CORESIDENCE_AGG, ifelse(C5=="356034","Pondicherry",C6))
CORESIDENCE_AGG$C6<-with(CORESIDENCE_AGG, ifelse(C5=="231017","Special region non-mappable",C6))
CORESIDENCE_AGG$C6<-with(CORESIDENCE_AGG, ifelse(C5=="466099","Unknown non-mappable",C6))
CORESIDENCE_AGG$C6<-with(CORESIDENCE_AGG, ifelse(C5=="566099","Unknown non-mappable",C6))
CORESIDENCE_AGG$C6<-with(CORESIDENCE_AGG, ifelse(C5=="705099","Unknown non-mappable",C6))

CORESIDENCE_AGG<-CORESIDENCE_AGG%>% filter(S1!="COL_1973_IPUMS")
CORESIDENCE_AGG<-CORESIDENCE_AGG%>% filter(S1!="COL_1985_IPUMS")
CORESIDENCE_AGG<-CORESIDENCE_AGG%>% filter(S1!="COL_1993_IPUMS")
CORESIDENCE_AGG<-CORESIDENCE_AGG%>% filter(S1!="COL_2005_IPUMS")
CORESIDENCE_AGG<-CORESIDENCE_AGG%>% filter(S1!="RWA_1991_IPUMS")
CORESIDENCE_AGG<-CORESIDENCE_AGG%>% filter(S1!="RWA_2002_IPUMS")

CORESIDENCE_AGG$C6<-with(CORESIDENCE_AGG, ifelse(C5=="686009","All other regions",C6))

# SAVE AS EXCEL AND RDATAFILE ######

CORESIDENCE_AGG_SUB_HARMO<-CORESIDENCE_AGG

library(writexl)
write_xlsx(CORESIDENCE_AGG_SUB_HARMO,"G:\\Shared drives\\CORESIDENCE\\WP2_DATA\\2_4_CORESIDENCE_DATABASE\\A2_CORESIDENCE_AGG_SUBNATIONAL_HARMONIZED_new.xlsx")
save(CORESIDENCE_AGG_SUB_HARMO,file="G:\\Shared drives\\CORESIDENCE\\WP2_DATA\\2_4_CORESIDENCE_DATABASE\\A2_CORESIDENCE_AGG_SUBNATIONAL_HARMONIZED_new.RData")


CORESIDENCE_AGG_long<-CORESIDENCE_AGG%>%
  pivot_longer(c(HS01:HH16), names_to = "indicator", values_to = "value") 


library(writexl)
write_xlsx(CORESIDENCE_AGG_long,"G:\\Shared drives\\CORESIDENCE\\WP2_DATA\\2_4_CORESIDENCE_DATABASE\\CORESIDENCE_AGG_SUBNATIONAL_long_HARMO_new.xlsx")
