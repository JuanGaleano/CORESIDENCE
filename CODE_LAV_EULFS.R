library(tidyverse)
library(haven)
library(tibble)
library(tidylog)
library(writexl)
library(readxl)
library(doParallel)
library(labelled)
`%notin%` <- Negate(`%in%`)

# Set working directory ####
setwd("./A5_LFS_SAMPLES/")

# Keep Samples 2000+ #####
extract_year <- function(name) {
  as.numeric(sub(".*_(\\d+)_.*", "\\1", name))
}

# Filter the list based on the year ######
filtered_list <- CORE_LFS_LIST[sapply(names(CORE_LFS_LIST), function(x)
  extract_year(x) >= 2000)]

# Rename, select and create variales over all samples ######
CORE_LFS_LIST<-lapply(filtered_list, function(df) {
  
  df <- df |>
    rename(SPLOC = HHSPOU,
           POPLOC = HHFATH,
           MOMLOC = HHMOTH) |>
    mutate(
      HHNUM = paste(HHNUM, REFWEEK, sep = "-"),
      RELATE = as.numeric(substr(RELATED, 1, 1)),
      RELATED = unclass(RELATED),
      RELATED2 =  as.numeric(substr(RELATED, 1, 2))
    ) |>
    select(-REFWEEK, 
           -HHSEQNUM, 
           -QUARTER, 
           -COUNTRY, 
           -REFYEAR, 
           -COUNTRYB) |>
    drop_na(COEFF)
  
  df <- df %>%
    mutate(PERNUM = row_number(), .by = "HHNUM")
  df
})



##### related by samples #######

RELATE <-lapply(CORE_LFS_LIST, function(df) {
  
  df<-as.data.frame(sort(unique(unclass(df$RELATED))))
  colnames(df)<-"RELATED"
  df<-df|>filter(RELATED!=9999)
  df<-df|>mutate(RELATED2=if_else(RELATED>=5000 &RELATED<6000, 5000,
                                  if_else(RELATED>=3000 &RELATED<3400, 3000,   
                                          if_else((RELATED>=4200 &RELATED<4211) | RELATED==4220, 4200,   
                                                  if_else(RELATED==4300, 4900,   
                                                          RELATED)))))
  df<-df|>select(RELATED2)
  df<-df|>distinct(RELATED2)
  df
})


# Set parallel environment #######
no_cores <- detectCores() - ceiling(detectCores() * .3)
registerDoParallel(cores = no_cores)
cl <- makeCluster(no_cores)

vec<-names(CORE_LFS_LIST)

# Add and select variables, filter population in private households ######
system.time(
  for (x in vec) {
    
    i <- x
    
    df<-CORE_LFS_LIST[[i]]
    
    df <- df |>
      mutate(
        SERIAL = unclass(HHNUM),
        RELATE = unclass(RELATE),
        RELATED = unclass(RELATED),
        RELATE_N = if_else(RELATE>3,6,RELATE),
        PERNUM = unclass(PERNUM),
        MOMLOC = unclass(MOMLOC),
        POPLOC = unclass(POPLOC),
        SPLOC = unclass(SPLOC),
        pop_w=1*COEFF
      ) 
    
    df <- df |> mutate(RELATED2 = if_else(
      RELATED >= 5000,
      5000,
      if_else(
        RELATED >= 3000 & RELATED < 3400,
        3000,
        if_else(
          RELATED >= 2000 & RELATED < 3000,
          2000,
          if_else(RELATED == 4110, 4100, if_else(
            RELATED == 4420, 4410, if_else((RELATED >= 4200 &
                                              RELATED <= 4211) | RELATED == 4220,
                                           4200,
                                           if_else(
                                             RELATED %in% c(4120, 4130, 4300, 4400, 4510, 4810, 4820, 4830, 4930),
                                             4900,
                                             RELATED
                                           )
            )
          ))
        )
      )
    ))
    
    
    # Split element by households: SERIAL #####
    listPOP <- split(df, f = df$SERIAL)
    
    # Create a function for detecting kinship relation: parallel ######
    
    FUN_1 <- function(df) {
      library(tidyverse)
      library(labelled)
      library(haven)
      library(tibble)
      library(purrr)
      `%notin%` <- Negate(`%in%`)
      
      POP<-df|>pull(POPLOC)
      MOM<-df|>pull(MOMLOC)
      
      df <- df |>
        rowwise()|>
        mutate(NCHILD = sum(PERNUM==c(POP,MOM)))|>
        ungroup()
      
      parents_f <- df |>
        filter(PERNUM %in% c(POPLOC))
      
      parents_m <- df |>
        filter(PERNUM %in% c(MOMLOC))
      
      parents<-bind_rows(parents_f,parents_m)
      
      
      df<-df|>
        rowwise()|>
        mutate(parent_of=sum(c(df$MOMLOC, df$POPLOC) %in% PERNUM),
               child_of=sum(df$PERNUM %in% c(MOMLOC, POPLOC)),
               spouse_of=sum(df$PERNUM %in% c(SPLOC)),
               sibmom = if_else(MOMLOC!=0, 
                                sum(df$MOMLOC == MOMLOC & df$PERNUM != PERNUM), 
                                0L),
               sibpop = if_else(POPLOC!=0, 
                                sum(df$POPLOC == POPLOC & df$PERNUM != PERNUM), 
                                0L)
        )|>
        ungroup()|>
        mutate(sibling_of=pmax(sibmom, sibpop))|>
        
        mutate(sibling_of=ifelse(sibling_of>0,sibling_of,
                                 ifelse(RELATED2==3000 & sum(RELATED2==3000)>1,sum(RELATED2==3000)-1,
                                        ifelse(RELATED2==4410, sum(RELATED2==4410),
                                               ifelse(RELATED2==1000 & any (RELATED2== 4410), sum(RELATED2== 4410),
                                                      ifelse(RELATED2==2000 & any (RELATED2== 4430), sum(RELATED2== 4430),   
                                                             ifelse(RELATED2==4430 & any (RELATED2== 2000), sum(RELATED2== 2000)+(sum(RELATED2== 4430)-1),
                                                                    0)))))))|>
        
        select(-sibmom,-sibpop)|>
        group_by(SERIAL)|>
        mutate(
          # Calculate how many grandparents each person has
          granchild_of = (
            # Check if MOMLOC is a child of anyone in the household
            (MOMLOC != 0 & MOMLOC %in% df$PERNUM[df$MOMLOC != 0]) +
              # Check if POPLOC is a child of anyone in the household
              (POPLOC != 0 & POPLOC %in% df$PERNUM[df$MOMLOC != 0])+
              
              (MOMLOC != 0 & MOMLOC %in% df$PERNUM[df$POPLOC != 0]) +
              # Check if POPLOC is a child of anyone in the household
              (POPLOC != 0 & POPLOC %in% df$PERNUM[df$POPLOC != 0])
          )
        )|>
        mutate(
          # Calculate how many grandparents each person has
          granchild_of = ifelse(granchild_of>0,granchild_of,
                                if_else(RELATED2==4100, sum(df$RELATED2 == 1000,df$RELATED2 == 2000),
                                        if_else(RELATED2 %in% c(1000,4410), sum(df$RELATED2 == 4500),0)))) |>
        ungroup() |>
        mutate(is_grandparent = if_else(PERNUM %in% parents$POPLOC | PERNUM %in% parents$MOMLOC, 1, 0))   |>
        mutate(grandparent_of =if_else(is_grandparent==1 & RELATED2 %in% c(1000,2000), sum(df$RELATED2 == 4100 &granchild_of>0, na.rm = TRUE),
                                       if_else(is_grandparent==1 & RELATED2 %in% c(4200), sum(df$RELATED2 == 3000 &granchild_of>0, na.rm = TRUE), 
                                               if_else(is_grandparent==1 & RELATED2 %in% c(4900,3000), sum(df$RELATED2 == 4900 &granchild_of>0, na.rm = TRUE), 
                                                       if_else(RELATED2 %in% c(1000,2000), sum(df$RELATED2 == 4100),
                                                               if_else(RELATED2 == 4500, sum(df$RELATED2 %in% c(1000,4410)),
                                                                       0))))))|>
        select(-is_grandparent)|>
        mutate(nind=rowSums(across(parent_of:grandparent_of)),
               nind2=dim(df)[1]-sum(df$RELATED2 == 5000, na.rm = TRUE),
               OR=ifelse(RELATED2==5000, 0, 
                         ifelse(nind<nind2-1,1,0)),
               ONR = if_else(any(RELATED2>=5000),1,0)
        )|>
        select(-nind,-nind2)
      
      
      df <- df |>
        mutate(sibling_of2=ifelse(RELATED2==3000 & sum(RELATED2==3000)>1,1,0))|>
        mutate(sibling_of3=ifelse(RELATED2==4410,1,
                                  ifelse(RELATED2==1000 & any (RELATED2== 4410), 1,
                                         ifelse(RELATED2==2000 & any (RELATED2== 4430), 1,   
                                                ifelse(RELATED2==4430 & any (RELATED2== 2000), 1,   
                                                       0)))))|>
        mutate(
          father = if_else(POPLOC > 0, 1, 0),
          mother = if_else(MOMLOC > 0, 2, 0),
          child  = if_else((NCHILD | parent_of) > 0, 2, 0),
          partner = if_else((SPLOC | spouse_of)  > 0 , 2, 0),
          sibling = if_else((sibling_of |sibling_of2|sibling_of3) > 0, 2, 0),
          grandparent= if_else((granchild_of> 0 | RELATED2==4100 | (RELATED2==1000 & any(RELATED==4500))), 2, 0),
          grandchild= if_else((grandparent_of> 0 | RELATED==4500 | (RELATED2 %in% c(1000,2000) & any(RELATED==4100))), 2, 0),
          other = if_else(OR > 0, 2, 0),
          non_rel = if_else(ONR > 0, 2, 0)
        )
      
    }
    
    # Apply function over each household  #####
    LOC_IPUMS1 <- parLapply(cl, listPOP, FUN_1)
    
    # Extract the results into a data frame #####
    DF_LOCIPUMS1 <- data.table::rbindlist(LOC_IPUMS1)

    # Compute Living Arrangement Value (LAV) ######
    relation<-c(1,2^1,2^2,2^3,2^4,2^5,2^6,2^7,2^8)
    
    DF_LOCIPUMS1 <- DF_LOCIPUMS1 |>
      add_count(SERIAL)|>
      mutate(
        GELAI = father + mother + child ^ 2 + partner ^ 3 + sibling ^ 4 + grandparent ^ 5 + grandchild ^ 6 + other ^ 7+ non_rel ^ 8,
        
        z1=map_dbl(GELAI, ~ ifelse(length(relation[relation <= .x]) > 0, max(relation[relation <= .x]), 0)),
        z2=map_dbl(GELAI-z1, ~ ifelse(length(relation[relation <= .x]) > 0, max(relation[relation <= .x]), 0)),
        z3=map_dbl(GELAI-z1-z2, ~ ifelse(length(relation[relation <= .x]) > 0, max(relation[relation <= .x]), 0)),
        z4=map_dbl(GELAI-z1-z2-z3, ~ ifelse(length(relation[relation <= .x]) > 0, max(relation[relation <= .x]), 0)),
        z5=map_dbl(GELAI-z1-z2-z3-z4, ~ ifelse(length(relation[relation <= .x]) > 0, max(relation[relation <= .x]), 0)),
        z6=map_dbl(GELAI-z1-z2-z3-z4-z5, ~ ifelse(length(relation[relation <= .x]) > 0, max(relation[relation <= .x]), 0)),
        z7=map_dbl(GELAI-z1-z2-z3-z4-z5-z6, ~ ifelse(length(relation[relation <= .x]) > 0, max(relation[relation <= .x]), 0)),
        z8=map_dbl(GELAI-z1-z2-z3-z4-z5-z6-z7, ~ ifelse(length(relation[relation <= .x]) > 0, max(relation[relation <= .x]), 0)),
        z9=map_dbl(GELAI-z1-z2-z3-z4-z5-z6-z7-z8, ~ ifelse(length(relation[relation <= .x]) > 0, max(relation[relation <= .x]), 0)),
        
      )|>
      mutate(
        across(starts_with("z"), ~case_when(
          . == 1 ~ "Father",
          . == 2 ~ "Mother",
          . == 4 ~ "Child",
          . == 8 ~ "Partner",
          . == 16 ~ "Sibling",
          . == 32 ~ "Grandaparent",
          . == 64 ~ "Grandchild",
          . == 128 ~ "Other relative",
          . == 256 ~ "Non-relative",
          TRUE ~ NA_character_
        ), .names = "label_{.col}"),
        GELAIQ=paste(label_z9,label_z8,label_z7,label_z6, label_z5, label_z4, label_z3, label_z2, label_z1, sep = "+"),
        GELAIQ=gsub("NA\\+", "", GELAIQ),
        GELAIQ = ifelse(GELAIQ=="NA", "Living alone", GELAIQ))|>
      select(-starts_with("label_"),
             -starts_with("z"),
             -HHPERS,-HHPARENT, -HHPARTNR,-HHCHILDR,-DEGURBA, 
             -father, -mother,-child, -partner, -sibling, -grandparent,-grandchild )
    
    # Add extra variables, rename #######
    regionw <- read_excel("regionw.xlsx")
    regionw<-as.data.frame(regionw)
    DF_LOCIPUMS1<-as.data.frame(DF_LOCIPUMS1)
    DF_LOCIPUMS1<- data.frame(DF_LOCIPUMS1,
                              regionw[match(DF_LOCIPUMS1[,"ISO3"],
                                            regionw[,"ISO3"]),c("EU Member States")])
    
    colnames(DF_LOCIPUMS1)[grep(names(DF_LOCIPUMS1 %>% select(last_col())), colnames(DF_LOCIPUMS1))]<-"CNTRY"
    
    
    DF_LOCIPUMS1<-DF_LOCIPUMS1|>
      rename(CONTINENT2=CONT2,
             CNTRY_ISO=ISO3, 
             EDATTAIN=HATLEV1D,
             MARST=MARSTAT)
    
   
    
    # Save micro data with LAV ######
    dataframe_name<-paste(unique(CORE_LFS_LIST[[i]]$SAMPLE),"_LA",sep="")
    assign(dataframe_name,DF_LOCIPUMS1_LA)
    
    objects_to_save1 <-dataframe_name
    save(list=objects_to_save1,
         file = paste("./LFS1_LA_K2/",objects_to_save1, ".Rda",sep=""))
    
    
    
    # Remove elements from working environment #####
    rm(list = setdiff(ls(), c("cl","no_cores","CORE_LFS_LIST","vec")))
    
    gc()
    
  }
)


