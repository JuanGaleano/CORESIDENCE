###### BUILDING GLAD ########
load("./input_data/LFS_86_NEW.Rda")
load("./input_data/IPUMS_301_NEW.Rda")
regionw <- read_excel("./input_data/regionw.xlsx")

regionw<-as.data.frame(regionw)
DF_LA_IPUMS<-as.data.frame(DF_LA_IPUMS)
DF_LA_LFS<-as.data.frame(DF_LA_LFS)

# Add variable CONTINENT and SUBCONTINENT #######
DF_LA_IPUMS<- data.frame(DF_LA_IPUMS,
                         regionw[match(DF_LA_IPUMS[,"CNTRY_ISO"],
                                       regionw[,"ISO3"]),c("REGIONW")])
colnames(DF_LA_IPUMS)[grep(names(DF_LA_IPUMS %>% select(last_col())), colnames(DF_LA_IPUMS))]<-"REGIONW"

DF_LA_LFS<- data.frame(DF_LA_LFS,
                       regionw[match(DF_LA_LFS[,"CNTRY_ISO"],
                                     regionw[,"ISO3"]),c("REGIONW")])
colnames(DF_LA_LFS)[grep(names(DF_LA_LFS %>% select(last_col())), colnames(DF_LA_LFS))]<-"REGIONW"

DF_LA_IPUMS<- data.frame(DF_LA_IPUMS,
                         regionw[match(DF_LA_IPUMS[,"CNTRY_ISO"],
                                       regionw[,"ISO3"]),c("EU Member States")])
colnames(DF_LA_IPUMS)[grep(names(DF_LA_IPUMS %>% select(last_col())), colnames(DF_LA_IPUMS))]<-"CNTRY"

DF_LA_LFS<- data.frame(DF_LA_LFS,
                       regionw[match(DF_LA_LFS[,"CNTRY_ISO"],
                                     regionw[,"ISO3"]),c("EU Member States")])
colnames(DF_LA_LFS)[grep(names(DF_LA_LFS %>% select(last_col())), colnames(DF_LA_LFS))]<-"CNTRY"

DF_LA_IPUMS<-DF_LA_IPUMS%>%
  mutate(SUBCONTINENT=case_when(REGIONW == 11 ~ "Eastern Africa",
                                REGIONW == 12 ~ "Middle Africa",
                                REGIONW == 13 ~ "Northern Africa",
                                REGIONW == 14 ~ "Southern Africa",
                                REGIONW == 15 ~ "Western Africa",
                                REGIONW == 21 ~ "Caribbean" ,
                                REGIONW == 22 ~ "Central America" ,
                                REGIONW == 23 ~ "South America" ,
                                REGIONW == 24 ~ "North America",
                                REGIONW == 31 ~ "Central Asia",
                                REGIONW == 32 ~ "Eastern Asia",
                                REGIONW == 33 ~ "Southern Asia" ,
                                REGIONW == 34 ~ "South-Eastern Asia",
                                REGIONW == 35 ~ "Western Asia",
                                REGIONW == 41 ~ "Eastern Europe" ,
                                REGIONW == 42 ~ "Northern Europe" ,
                                REGIONW == 43 ~ "Southern Europe",
                                REGIONW == 44 ~ "Western Europe",
                                REGIONW == 51 ~ "Australia and New Zealand" ,
                                REGIONW == 52 ~ "Melanesia",
                                REGIONW == 53 ~ "Micronesia",
                                REGIONW == 54 ~ "Polynesia"))

DF_LA_IPUMS<-DF_LA_IPUMS%>%
  mutate(CONTINENT=case_when(SUBCONTINENT == "Eastern Africa" ~ "AFRICA",
                             SUBCONTINENT == "Middle Africa" ~ "AFRICA",
                             SUBCONTINENT == "Northern Africa" ~ "AFRICA",
                             SUBCONTINENT == "Southern Africa" ~ "AFRICA",
                             SUBCONTINENT == "Western Africa" ~ "AFRICA",
                             SUBCONTINENT == "Caribbean" ~ "LATIN-AMERICA",
                             SUBCONTINENT == "Central America" ~ "LATIN-AMERICA",
                             SUBCONTINENT == "South America" ~ "LATIN-AMERICA",
                             SUBCONTINENT == "North America" ~ "NORTH-AMERICA",
                             SUBCONTINENT == "Central Asia" ~ "ASIA",
                             SUBCONTINENT == "Eastern Asia" ~ "ASIA",
                             SUBCONTINENT == "Southern Asia" ~ "ASIA",
                             SUBCONTINENT == "South-Eastern Asia" ~ "ASIA",
                             SUBCONTINENT == "Western Asia" ~ "ASIA",
                             SUBCONTINENT == "Eastern Europe" ~ "EUROPE",
                             SUBCONTINENT == "Northern Europe" ~ "EUROPE",
                             SUBCONTINENT == "Southern Europe" ~ "EUROPE",
                             SUBCONTINENT == "Western Europe" ~ "EUROPE",
                             SUBCONTINENT == "Australia and New Zealand" ~ "OCEANIA",
                             SUBCONTINENT == "Melanesia" ~ "OCEANIA",
                             SUBCONTINENT == "Micronesia" ~ "OCEANIA",
                             SUBCONTINENT == "Polynesia" ~ "OCEANIA"))

DF_LA_LFS<-DF_LA_LFS%>%
  mutate(SUBCONTINENT=case_when(REGIONW == 11 ~ "Eastern Africa",
                                REGIONW == 12 ~ "Middle Africa",
                                REGIONW == 13 ~ "Northern Africa",
                                REGIONW == 14 ~ "Southern Africa",
                                REGIONW == 15 ~ "Western Africa",
                                REGIONW == 21 ~ "Caribbean" ,
                                REGIONW == 22 ~ "Central America" ,
                                REGIONW == 23 ~ "South America" ,
                                REGIONW == 24 ~ "North America",
                                REGIONW == 31 ~ "Central Asia",
                                REGIONW == 32 ~ "Eastern Asia",
                                REGIONW == 33 ~ "Southern Asia" ,
                                REGIONW == 34 ~ "South-Eastern Asia",
                                REGIONW == 35 ~ "Western Asia",
                                REGIONW == 41 ~ "Eastern Europe" ,
                                REGIONW == 42 ~ "Northern Europe" ,
                                REGIONW == 43 ~ "Southern Europe",
                                REGIONW == 44 ~ "Western Europe",
                                REGIONW == 51 ~ "Australia and New Zealand" ,
                                REGIONW == 52 ~ "Melanesia",
                                REGIONW == 53 ~ "Micronesia",
                                REGIONW == 54 ~ "Polynesia"))

DF_LA_LFS<-DF_LA_LFS%>%
  mutate(CONTINENT=case_when(SUBCONTINENT == "Eastern Africa" ~ "AFRICA",
                             SUBCONTINENT == "Middle Africa" ~ "AFRICA",
                             SUBCONTINENT == "Northern Africa" ~ "AFRICA",
                             SUBCONTINENT == "Southern Africa" ~ "AFRICA",
                             SUBCONTINENT == "Western Africa" ~ "AFRICA",
                             SUBCONTINENT == "Caribbean" ~ "LATIN-AMERICA",
                             SUBCONTINENT == "Central America" ~ "LATIN-AMERICA",
                             SUBCONTINENT == "South America" ~ "LATIN-AMERICA",
                             SUBCONTINENT == "North America" ~ "NORTH-AMERICA",
                             SUBCONTINENT == "Central Asia" ~ "ASIA",
                             SUBCONTINENT == "Eastern Asia" ~ "ASIA",
                             SUBCONTINENT == "Southern Asia" ~ "ASIA",
                             SUBCONTINENT == "South-Eastern Asia" ~ "ASIA",
                             SUBCONTINENT == "Western Asia" ~ "ASIA",
                             SUBCONTINENT == "Eastern Europe" ~ "EUROPE",
                             SUBCONTINENT == "Northern Europe" ~ "EUROPE",
                             SUBCONTINENT == "Southern Europe" ~ "EUROPE",
                             SUBCONTINENT == "Western Europe" ~ "EUROPE",
                             SUBCONTINENT == "Australia and New Zealand" ~ "OCEANIA",
                             SUBCONTINENT == "Melanesia" ~ "OCEANIA",
                             SUBCONTINENT == "Micronesia" ~ "OCEANIA",
                             SUBCONTINENT == "Polynesia" ~ "OCEANIA"))



DF_LA_IPUMS$REGIONW<-NULL
DF_LA_LFS$REGIONW<-NULL

# Add variable SOURCE ######

DF_LA_IPUMS$SOURCE<-"IPUMS"
DF_LA_LFS$SOURCE<-"LFS"

# Relocate variables ######
DF_LA_IPUMS<- DF_LA_IPUMS %>% relocate(CNTRY_ISO, .before = YEAR)
DF_LA_IPUMS<- DF_LA_IPUMS %>% relocate(SOURCE, .before = SEX)
DF_LA_IPUMS<- DF_LA_IPUMS %>% relocate(CNTRY, .before = SEX)
DF_LA_IPUMS<- DF_LA_IPUMS %>% relocate(SUBCONTINENT, .before = SEX)
DF_LA_IPUMS<- DF_LA_IPUMS %>% relocate(CONTINENT, .before = SEX)

DF_LA_LFS<- DF_LA_LFS %>% relocate(CNTRY_ISO, .before = YEAR)
DF_LA_LFS<- DF_LA_LFS %>% relocate(SOURCE, .before = SEX)
DF_LA_LFS<- DF_LA_LFS %>% relocate(CNTRY, .before = SEX)
DF_LA_LFS<- DF_LA_LFS %>% relocate(SUBCONTINENT, .before = SEX)
DF_LA_LFS<- DF_LA_LFS %>% relocate(CONTINENT, .before = SEX)

# Remove all labels ######
DF_LA_IPUMS<-DF_LA_IPUMS %>% 
  sjlabelled::remove_all_labels() %>% 
  tibble() 
# Check edattain lfs #####
DF_LA_LFS<-DF_LA_LFS|>mutate(EDATTAIN=as.character(EDATTAIN))

DF_LA_LFS<-DF_LA_LFS|>mutate(EDATTAIN=if_else(EDATTAIN=="",NA,EDATTAIN))

# Change colnames #####
colnames(DF_LA_IPUMS)[12]<-"LAT"
colnames(DF_LA_LFS)[12]<-"LAT"
colnames(DF_LA_IPUMS)[13]<-"LAI"
colnames(DF_LA_LFS)[13]<-"LAI"
colnames(DF_LA_IPUMS)[14]<-"POP"
colnames(DF_LA_LFS)[14]<-"POP"
colnames(DF_LA_IPUMS)[15]<-"POPW"
colnames(DF_LA_LFS)[15]<-"POPW"
# Keep numeric ID LAT ####

DF_LA_IPUMS<-DF_LA_IPUMS|>mutate(LAT=substr(LAI,1,2))
DF_LA_LFS<-DF_LA_LFS|>mutate(LAT=substr(LAI,1,2))
# Round weighted pop #####
DF_LA_LFS<-DF_LA_LFS|>mutate(POPW=round(POPW,0))
DF_LA_LFS<-as_tibble(DF_LA_LFS)

# Aggregate IPUMS data into age groups ######
DF_LA_IPUMS_A<-DF_LA_IPUMS|>
  mutate(AGE2 = case_when(
    AGE %in% 0:4 ~ "0-4",
    AGE %in% 5:9 ~ "5-9",
    AGE %in% 10:14 ~ "10-14",
    AGE %in% 15:19 ~ "15-19",
    AGE %in% 20:24 ~ "20-24",
    AGE %in% 25:29 ~ "25-29",
    AGE %in% 30:34 ~ "30-34",
    AGE %in% 35:39 ~ "35-39",
    AGE %in% 40:44 ~ "40-44",
    AGE %in% 45:49 ~ "45-49",
    AGE %in% 50:54 ~ "50-54",
    AGE %in% 55:59 ~ "55-59",
    AGE %in% 60:64 ~ "60-64",
    AGE %in% 65:69 ~ "65-69",
    AGE %in% 70:74 ~ "70-74",
    AGE %in% 75:79 ~ "75-79",
    AGE %in% 80:84 ~ "80-84",
    AGE %in% 85:89 ~ "85-89",
    AGE %in% 90:94 ~ "90-94",
    AGE %in% 95:150 ~ "95+")
  )|>
  group_by(SAMPLE,CNTRY_ISO,YEAR,SOURCE,CNTRY,SUBCONTINENT,CONTINENT,
           SEX,AGE2,MARST,EDATTAIN,LAT,LAI)|>
  summarise(POP=sum(POP),
            POPW=sum(POPW))|>ungroup()

# Read IPUMS sample with age groups ######
setwd("./input_data/AGE_GROUPS\\LA\\")

patt.shp <- ".*Rda*"

data.shp <-list.files(pattern=patt.shp)

LA_IPUMS_LIST <- sapply(data.shp, function(x) mget(load(x)), simplify = TRUE) 

DF_LA_IPUMS_A2 <- data.table::rbindlist(LA_IPUMS_LIST)

# Add variable CONTINENT and SUBCONTINENT ####
regionw<-as.data.frame(regionw)

DF_LA_IPUMS_A2<-as.data.frame(DF_LA_IPUMS_A2)

DF_LA_IPUMS_A2<- data.frame(DF_LA_IPUMS_A2,
                            regionw[match(DF_LA_IPUMS_A2[,"CNTRY_ISO"],
                                          regionw[,"ISO3"]),c("REGIONW")])
colnames(DF_LA_IPUMS_A2)[grep(names(DF_LA_IPUMS_A2 %>% select(last_col())), colnames(DF_LA_IPUMS_A2))]<-"REGIONW"

DF_LA_IPUMS_A2<- data.frame(DF_LA_IPUMS_A2,
                            regionw[match(DF_LA_IPUMS_A2[,"CNTRY_ISO"],
                                          regionw[,"ISO3"]),c("EU Member States")])
colnames(DF_LA_IPUMS_A2)[grep(names(DF_LA_IPUMS_A2 %>% select(last_col())), colnames(DF_LA_IPUMS_A2))]<-"CNTRY"


DF_LA_IPUMS_A2<-DF_LA_IPUMS_A2%>%
  mutate(SUBCONTINENT=case_when(REGIONW == 11 ~ "Eastern Africa",
                                REGIONW == 12 ~ "Middle Africa",
                                REGIONW == 13 ~ "Northern Africa",
                                REGIONW == 14 ~ "Southern Africa",
                                REGIONW == 15 ~ "Western Africa",
                                REGIONW == 21 ~ "Caribbean" ,
                                REGIONW == 22 ~ "Central America" ,
                                REGIONW == 23 ~ "South America" ,
                                REGIONW == 24 ~ "North America",
                                REGIONW == 31 ~ "Central Asia",
                                REGIONW == 32 ~ "Eastern Asia",
                                REGIONW == 33 ~ "Southern Asia" ,
                                REGIONW == 34 ~ "South-Eastern Asia",
                                REGIONW == 35 ~ "Western Asia",
                                REGIONW == 41 ~ "Eastern Europe" ,
                                REGIONW == 42 ~ "Northern Europe" ,
                                REGIONW == 43 ~ "Southern Europe",
                                REGIONW == 44 ~ "Western Europe",
                                REGIONW == 51 ~ "Australia and New Zealand" ,
                                REGIONW == 52 ~ "Melanesia",
                                REGIONW == 53 ~ "Micronesia",
                                REGIONW == 54 ~ "Polynesia"))

DF_LA_IPUMS_A2<-DF_LA_IPUMS_A2%>%
  mutate(CONTINENT=case_when(SUBCONTINENT == "Eastern Africa" ~ "AFRICA",
                             SUBCONTINENT == "Middle Africa" ~ "AFRICA",
                             SUBCONTINENT == "Northern Africa" ~ "AFRICA",
                             SUBCONTINENT == "Southern Africa" ~ "AFRICA",
                             SUBCONTINENT == "Western Africa" ~ "AFRICA",
                             SUBCONTINENT == "Caribbean" ~ "LATIN-AMERICA",
                             SUBCONTINENT == "Central America" ~ "LATIN-AMERICA",
                             SUBCONTINENT == "South America" ~ "LATIN-AMERICA",
                             SUBCONTINENT == "North America" ~ "NORTH-AMERICA",
                             SUBCONTINENT == "Central Asia" ~ "ASIA",
                             SUBCONTINENT == "Eastern Asia" ~ "ASIA",
                             SUBCONTINENT == "Southern Asia" ~ "ASIA",
                             SUBCONTINENT == "South-Eastern Asia" ~ "ASIA",
                             SUBCONTINENT == "Western Asia" ~ "ASIA",
                             SUBCONTINENT == "Eastern Europe" ~ "EUROPE",
                             SUBCONTINENT == "Northern Europe" ~ "EUROPE",
                             SUBCONTINENT == "Southern Europe" ~ "EUROPE",
                             SUBCONTINENT == "Western Europe" ~ "EUROPE",
                             SUBCONTINENT == "Australia and New Zealand" ~ "OCEANIA",
                             SUBCONTINENT == "Melanesia" ~ "OCEANIA",
                             SUBCONTINENT == "Micronesia" ~ "OCEANIA",
                             SUBCONTINENT == "Polynesia" ~ "OCEANIA"))

DF_LA_IPUMS_A2$REGIONW<-NULL

# Add variable SOURCE ######
DF_LA_IPUMS_A2$SOURCE<-"IPUMS"

# Relocate variables ######
DF_LA_IPUMS_A2<- DF_LA_IPUMS_A2 %>% relocate(CNTRY_ISO, .before = YEAR)
DF_LA_IPUMS_A2<- DF_LA_IPUMS_A2 %>% relocate(SOURCE, .before = SEX)
DF_LA_IPUMS_A2<- DF_LA_IPUMS_A2 %>% relocate(CNTRY, .before = SEX)
DF_LA_IPUMS_A2<- DF_LA_IPUMS_A2 %>% relocate(SUBCONTINENT, .before = SEX)
DF_LA_IPUMS_A2<- DF_LA_IPUMS_A2 %>% relocate(CONTINENT, .before = SEX)

# Remove all labels####
DF_LA_IPUMS_A2<-DF_LA_IPUMS_A2 %>% 
  sjlabelled::remove_all_labels() %>% 
  tibble() 

# Change colnames #####
colnames(DF_LA_IPUMS_A2)[12]<-"LAT"

colnames(DF_LA_IPUMS_A2)[13]<-"LAI"

colnames(DF_LA_IPUMS_A2)[14]<-"POP"

colnames(DF_LA_IPUMS_A2)[15]<-"POPW"

# Keep numeric ID LAT #####
DF_LA_IPUMS_A2<-DF_LA_IPUMS_A2|>mutate(LAT=substr(LAI,1,2))

# Bind age groups dataframes ######
DF_LA_IPUMS_AF<-bind_rows(DF_LA_IPUMS_A,DF_LA_IPUMS_A2)

# Harmonized dataframe ######
DF_LA_IPUMS_H<-DF_LA_IPUMS_AF|>
  mutate(EDATTAIN2 = if_else(EDATTAIN %in% c(1,2), "L",
                             if_else(EDATTAIN %in% c(3), "M",
                                     if_else(EDATTAIN %in% c(4), "H",
                                             if_else(EDATTAIN %in% c(0), "9",    
                                                     as.character(EDATTAIN))))),
         MARST2 = if_else(MARST %in% c(0,1), 1,
                          if_else(MARST %in% c(3,4), 0, MARST,
                          ))
  )|>
  group_by(SAMPLE,CNTRY_ISO,YEAR,SOURCE,CNTRY,SUBCONTINENT,CONTINENT,
           SEX,AGE2,MARST2,EDATTAIN2,LAT,LAI)|>
  summarise(POP=sum(POP),
            POPW=sum(POPW))|>ungroup()

colnames(DF_LA_IPUMS_H)[9]<-"AGE"
colnames(DF_LA_IPUMS_H)[10]<-"MARST"
colnames(DF_LA_IPUMS_H)[11]<-"EDATTAIN"


unique(DF_LA_IPUMS_H$EDATTAIN)
unique(DF_LA_LFS$EDATTAIN)

unique(DF_LA_IPUMS_H$MARST)
unique(DF_LA_LFS$MARST)

names(DF_LA_IPUMS_H)

# GLAD ####

# Remove samples ########
`%notin%` <- Negate(`%in%`)
class(DF_LA_IPUMS)
ipums_remove<-c("BGD_1991_IPUMS",
                "BGD_2001_IPUMS",
                "IDN_2000_IPUMS",
                "ITA_2011_IPUMS",
                "HUN_1970_IPUMS",
                "HUN_2001_IPUMS",
                "POL_1978_IPUMS")

ipumsa_remove<-c(ipums_remove,
                 "IRL_1971_IPUMS",
                 "IRL_1979_IPUMS",
                 "IRL_1981_IPUMS",
                 "IRL_2016_IPUMS")

lfs_remove<-c("BEL_2015_LFS",
              "BGR_2010_LFS",
              "DEU_2000_LFS")


DF_LA_IPUMS2<-DF_LA_IPUMS|>filter(SAMPLE %notin%ipums_remove )

DF_LA_IPUMSAF2<-DF_LA_IPUMS_AF|>filter(SAMPLE %notin%ipumsa_remove )

DF_LA_LFS2<-DF_LA_LFS|>filter(SAMPLE %notin%lfs_remove)

DF_LA_IPUMS_H2<-DF_LA_IPUMS_H|>filter(SAMPLE %notin%ipumsa_remove)

HARMONIZED=bind_rows(DF_LA_IPUMS_H2,DF_LA_LFS2)

length(unique(DF_LA_IPUMS2$SAMPLE))
length(unique(DF_LA_IPUMSAF2$SAMPLE))
length(unique(DF_LA_LFS2$SAMPLE))
length(unique(HARMONIZED$SAMPLE))

sum(HARMONIZED$POP)

# Read codebook ####
CODEBOOK <- read_csv("./input_data/CODEBOOK_GLAD.csv")

# Function for labels ######
LABELS_LAT <- function(LAT) {
  case_when(
    LAT == 10 ~ "Alone",
    
    LAT == 20 ~ "With single parent",
    LAT == 21 ~ "With single parent extended",
    LAT == 22 ~ "With single parent extended composite",
    
    LAT == 30 ~ "With parents",
    LAT == 31 ~ "With parents extended",
    LAT == 32 ~ "With parents extended composite",
    
    LAT == 40 ~ "With partner",
    LAT == 41 ~ "With partner extended",
    LAT == 42 ~ "With partner extended composite",
    
    LAT == 50 ~ "With partner and children",
    LAT == 51 ~ "With partner and children extended",
    LAT == 52 ~ "With partner and children extended composite",
    
    LAT == 60 ~ "With children",
    LAT == 61 ~ "With children extended",
    LAT == 62 ~ "With children extended composite",
    
    LAT == 70 ~ "Extended",
    LAT == 71 ~ "Extended composite",
    
    LAT == 80 ~ "Non-relative"
  )
}

# List GLAD #####
GLAD <- list(
  'SINGLE AGES'=DF_LA_IPUMS2,
  'AGE GROUPS IPUMS'=DF_LA_IPUMSAF2,
  'AGE GROUPS LFS'=DF_LA_LFS2,
  HARMONIZED=HARMONIZED,
  CODEBOOK=CODEBOOK,
  'LABELS LAT' =LABELS_LAT,
  'ATLAS LIVING ARRANGEMENTS'="Access to the Atlas of Living Arrangements: http://bit.ly/4iqY6ol"
)


save(GLAD, file="./input_data/CORESIDENCE_GLAD_2025.Rda")

