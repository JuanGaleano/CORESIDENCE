library(purrr)
library(knitr)
library(kableExtra)
library(tidyverse)
library(RColorBrewer)
library(tidyverse)
library(haven)
library(tibble)
library(tidylog)
library(writexl)
library(readxl)
library(doParallel)
`%notin%` <- Negate(`%in%`)

load("G:\\Shared drives\\CORESIDENCE\\WP2_DATA\\2_4_CORESIDENCE_DATABASE\\CORESIDENCE_LIVING_ARRANGEMENTS\\CORE_FILES\\DHS_TO_DB/ALB_2017-18_DHS.RData")

df<-`ALB_2017-18_DHS`
df<-df|>
  mutate(RELATED1=as_factor(RELATED))

no_cores <- detectCores() -5
registerDoParallel(cores = no_cores)
cl <- makeCluster(no_cores)

listPOP <- split(df, f = df$HHID2)
unique(df$RELATED)


#### POPLOC ######
FUN_POPLOC_DHS <- function(df) {
  library(tidyverse)
  library(haven)
  library(tibble)
  
  #POPLOC ######
  df <- df |>
    mutate(
      POPLOC2 = ifelse(HV114>0, HV114,NA))
  
  ## CHILDREN #####
  df <- df |>
    mutate(
      POPLOC2 = ifelse(!is.na(POPLOC2), POPLOC2,   
                       ifelse(RELATED %in% c(3100,3210) & any(RELATED == 1000 & HV104 == 1), 
                              HVIDX[RELATED == 1000 & HV104 == 1],
                              ifelse(RELATED  %in% c(3100,3210) & HV219 == 2 & any(RELATED %in% c(2100,2200) & HV104 == 1), 
                                     HVIDX[RELATED %in% c(2100,2200) & HV104 == 1],
                                     ifelse(RELATED %in% c(3100,3210) & !any(RELATED == 1000 & HV104 == 1),0,
                                            ifelse(RELATED  %in% c(3100,3210) & HV219 == 2 & !any(RELATED %in% c(2100,2200) & HV104 == 1),0,
                                                   NA))))))
  
  ## HEAD #####
  df <- df |>
    mutate(
      POPLOC2 = ifelse(!is.na(POPLOC2), POPLOC2,   
                       ifelse((RELATED == 1000 & any(RELATED == 4210 & HV104 == 1)), 
                              HVIDX[RELATED == 4210 & HV104 == 1],
                              ifelse((RELATED == 1000 & !any(RELATED == 4210) |
                                        RELATED == 1000 & sum(RELATED == 4210) == 1 & any(RELATED == 4210 & HV104 != 1)),0,
                                     NA))))
  
  ## PARTNER #####
  df <- df |>
    mutate(
      POPLOC2 = ifelse(!is.na(POPLOC2), POPLOC2,   
                       ifelse((RELATED == 2100 & any(RELATED == 4220 & HV104 == 1)), 
                              HVIDX[RELATED == 4220 & HV104 == 1],
                       ifelse((RELATED  ==2200 & any(RELATED == 4220 & HV104 == 1)), 
                                     HVIDX[RELATED == 4220 & HV104 == 1],
                              ifelse((RELATED == 2100 & !any(RELATED == 4220) |
                                        RELATED == 2100 & sum(RELATED == 4220) == 1 & any(RELATED == 4220 & HV104 != 1)),0,
                                     ifelse((RELATED == 2200 & !any(RELATED == 4220) |
                                               RELATED == 2200 & sum(RELATED == 4220) == 1 & any(RELATED == 4220 & HV104 != 1)),0,
                                            NA))))))
  
  ## CHILD IN LAW #####
  df <- df |>
    mutate(
      POPLOC2 = ifelse(!is.na(POPLOC2), POPLOC2,   
                       ifelse((RELATED == 3400 & !any(RELATED == 4900 | RELATED == 4920))|
                                (RELATED == 3400 & !any((RELATED == 4900 | RELATED == 4920) & HV104 == 1))|
                                (RELATED == 3400 & (max(HV105[RELATED == 4900])-HV105) < 20),0,
                              NA)))
  
  ## GRANDCHILD #####
  df <- df |>
    mutate(
      POPLOC2 = ifelse(!is.na(POPLOC2), POPLOC2,   
                       ifelse((RELATED == 4100 & sum (RELATED == 3100 & HV104 == 1)==1), HVIDX[RELATED == 3100 & HV104 == 1], # ok   
                              ifelse((RELATED == 4100 & sum (RELATED == 3400 & HV104 == 1)==1), HVIDX[RELATED == 3400 & HV104 == 1], # ok 
                                     ifelse((RELATED == 4100 & !any((RELATED == 3100 |RELATED == 3400)  & HV104 == 1)), 0, # ok
                                            NA)))))
  
  ## PARENTS #####
  df <- df |>
    mutate(
      POPLOC2 = ifelse(!is.na(POPLOC2), POPLOC2,   
                       ifelse((RELATED == 4210 & !any(RELATED == 4900 | RELATED == 4920))|
                                (RELATED == 4210 & !any((RELATED == 4900 | RELATED == 4920) & HV104 == 1))|
                                (RELATED == 4210 & (max(HV105[RELATED == 4900])-HV105) < 20),0,
                              NA)))
  
  
  ## PARENTS IN LAW #####
  df <- df |>
    mutate(
      POPLOC2 = ifelse(!is.na(POPLOC2), POPLOC2,   
                       ifelse((RELATED == 4220 & !any(RELATED == 4900 | RELATED == 4920))|
                                (RELATED == 4220 & !any((RELATED == 4900 | RELATED == 4920) & HV104 == 1))|
                                (RELATED == 4220 & (max(HV105[RELATED == 4900])-HV105) < 20),0,
                              NA)))
  ## SIBLING #####
  df <- df |>
    mutate(
      POPLOC2 = ifelse(!is.na(POPLOC2), POPLOC2,   
                       ifelse(RELATED == 4410, POPLOC2[RELATED == 1000], NA)),
      POPLOC2 == ifelse(!is.na(POPLOC2), POPLOC2,    
                        ifelse(is.na(POPLOC2) & RELATED == 4100 & lead(RELATED == 4100 & !is.na(POPLOC2)),
                               lead(POPLOC2),NA))
    )
  
  
  ## Niece/nephew by blood (4812) #####
  df <- df |>
    mutate(
      POPLOC2 = ifelse(!is.na(POPLOC2), POPLOC2,   
                       ifelse((RELATED == 4812 & any(RELATED == 4410 & HV104 == 1)), HVIDX[RELATED == 4410 & HV104 == 1], # ok     
                              ifelse((RELATED == 4812 & !any(RELATED == 4410))| # ok
                                       (RELATED == 4812 & !any(RELATED == 4410 & HV104 == 1)),0,NA))))
  
  ## Niece/nephew by marriage (4813) #####
  df <- df |>
    mutate(
      POPLOC2 = ifelse(!is.na(POPLOC2), POPLOC2,   
                       ifelse((RELATED == 4813 & !any(RELATED == 4900 | RELATED == 4920))|
                                (RELATED == 4813 & !any((RELATED == 4900 | RELATED == 4920) & HV104 == 1)),0,
                              NA)))
  
  
  ## OTHER RELATIVE (4900) #####
  df <- df |>
    mutate(
      POPLOC2 = ifelse(!is.na(POPLOC2), POPLOC2,
                       ifelse((RELATED == 4900 & !any(RELATED == 4900 & HV104 == 1))| # ok
                                (RELATED == 4900 & sum(RELATED == 4900) == 1)| # ok
                                (RELATED == 4900 & HV105 >= max(HV105[RELATED == 4900])),0,NA)))
  
  ## Non-relative, n.e.c. (5900) #####
  df <- df |>
    mutate(
      POPLOC2 = ifelse(!is.na(POPLOC2), POPLOC2,
                       ifelse((RELATED == 5900 & !any(RELATED == 5900 & HV104 == 1))| # ok
                                (RELATED == 5900 & sum(RELATED == 5900) == 1)|# ok
                                (RELATED == 5900 & HV105 == max(HV105[RELATED == 5900]))| # ok
                                (RELATED == 5900 & HV105 <= max(HV105[RELATED == 5900])), 0,NA)))
  
  # RESIDUALS ####
  df <- df |>
    mutate(
      POPLOC2 = ifelse(!is.na(POPLOC2), POPLOC2,
                       ifelse(RELATED==4100 & HV112>0,HV114,NA)))
  
  
  df <- df |>
    mutate(
      MOMLOC2 = ifelse(HV112>0, HV112,NA))
  
  
  ### MOMLOC #####
  df <- df |>
    mutate(
      MOMLOC2 = ifelse(HV112>0, HV112,NA))
  
  ## CHILDREN #####
  df <- df |>
    mutate(
      MOMLOC2 = ifelse(!is.na(MOMLOC2), MOMLOC2,   
                       ifelse(RELATED %in% c(3100,3210) & any(RELATED == 1000 & HV104 == 2), 
                              HVIDX[RELATED == 1000 & HV104 == 2],
                              ifelse(RELATED  %in% c(3100,3210) & HV219 == 1 & any(RELATED %in% c(2100,2200) & HV104 == 2), 
                                     HVIDX[RELATED %in% c(2100,2200) & HV104 == 2],
                                     ifelse(RELATED %in% c(3100,3210) & !any(RELATED == 1000 & HV104 == 2),0,
                                            ifelse(RELATED  %in% c(3100,3210) & HV219 == 1 & !any(RELATED %in% c(2100,2200) & HV104 == 2),0,
                                                   NA))))))
  
  ## HEAD #####
  df <- df |>
    mutate(
      MOMLOC2 = ifelse(!is.na(MOMLOC2), MOMLOC2,   
                       ifelse((RELATED == 1000 & any(RELATED == 4210 & HV104 == 2)), 
                              HVIDX[RELATED == 4210 & HV104 == 2],
                              ifelse((RELATED == 1000 & !any(RELATED == 4210) |
                                        RELATED == 1000 & sum(RELATED == 4210) == 1 & any(RELATED == 4210 & HV104 != 2)),0,
                                     NA))))
  
  ## PARTNER #####
  df <- df |>
    mutate(
      MOMLOC2 = ifelse(!is.na(MOMLOC2), MOMLOC2,   
                       ifelse((RELATED == 2100 & any(RELATED == 4220 & HV104 == 2)), 
                              HVIDX[RELATED == 4220 & HV104 == 2],
                              ifelse((RELATED  ==2200 & any(RELATED == 4220 & HV104 == 2)), 
                                     HVIDX[RELATED == 4220 & HV104 == 2],
                              ifelse((RELATED == 2100 & !any(RELATED == 4220) |
                                        RELATED == 2100 & sum(RELATED == 4220) == 1 & any(RELATED == 4220 & HV104 != 2)),0,
                                     ifelse((RELATED == 2200 & !any(RELATED == 4220) |
                                               RELATED == 2200 & sum(RELATED == 4220) == 1 & any(RELATED == 4220 & HV104 != 2)),0,
                                            NA))))))
  
  ## CHILD IN LAW #####
  df <- df |>
    mutate(
      MOMLOC2 = ifelse(!is.na(MOMLOC2), MOMLOC2,   
                       ifelse((RELATED == 3400 & !any(RELATED == 4900 | RELATED == 4920))|
                                (RELATED == 3400 & !any((RELATED == 4900 | RELATED == 4920) & HV104 == 2))|
                                (RELATED == 3400 & (max(HV105[RELATED == 4900])-HV105) < 20),0,
                              NA)))
  
  ## GRANDCHILD #####
  df <- df |>
    mutate(
      MOMLOC2 = ifelse(!is.na(MOMLOC2), MOMLOC2,   
                       ifelse((RELATED == 4100 & sum (RELATED == 3100 & HV104 == 2)==1), HVIDX[RELATED == 3100 & HV104 == 2], # ok   
                              ifelse((RELATED == 4100 & sum (RELATED == 3400 & HV104 == 2)==1), HVIDX[RELATED == 3400 & HV104 == 2], # ok 
                                     ifelse((RELATED == 4100 & !any((RELATED == 3100 |RELATED == 3400)  & HV104 == 2)), 0, # ok
                                            NA)))))
  
  ## PARENTS #####
  df <- df |>
    mutate(
      MOMLOC2 = ifelse(!is.na(MOMLOC2), MOMLOC2,   
                       ifelse((RELATED == 4210 & !any(RELATED == 4900 | RELATED == 4920))|
                                (RELATED == 4210 & !any((RELATED == 4900 | RELATED == 4920) & HV104 == 2))|
                                (RELATED == 4210 & (max(HV105[RELATED == 4900])-HV105) < 20),0,
                              NA)))
  
  
  ## PARENTS IN LAW #####
  df <- df |>
    mutate(
      MOMLOC2 = ifelse(!is.na(MOMLOC2), MOMLOC2,   
                       ifelse((RELATED == 4220 & !any(RELATED == 4900 | RELATED == 4920))|
                                (RELATED == 4220 & !any((RELATED == 4900 | RELATED == 4920) & HV104 == 2))|
                                (RELATED == 4220 & (max(HV105[RELATED == 4900])-HV105) < 20),0,
                              NA)))
  ## SIBLING #####
  df <- df |>
    mutate(
      MOMLOC2 = ifelse(!is.na(MOMLOC2), MOMLOC2,   
                       ifelse(RELATED == 4410, MOMLOC2[RELATED == 1000], NA)),
      #POPLOC = ifelse(RELATED == 3100 & (any(RELATED == 2100 & HV104 == 2) & !any(RELATED == 1000)), 0, POPLOC),
      POPLOC2 == ifelse(!is.na(MOMLOC2), MOMLOC2,    
                        ifelse(is.na(MOMLOC2) & RELATED == 4100 & lead(RELATED == 4100 & !is.na(MOMLOC2)),
                               lead(MOMLOC2),NA))
    )
  
  
  ## Niece/nephew by blood (4812) #####
  df <- df |>
    mutate(
      MOMLOC2 = ifelse(!is.na(MOMLOC2), MOMLOC2,   
                       ifelse((RELATED == 4812 & any(RELATED == 4410 & HV104 == 2)), 
                              HVIDX[RELATED == 4410 & HV104 == 2], # ok     
                              ifelse((RELATED == 4812 & !any(RELATED == 4410))| # ok
                                       (RELATED == 4812 & !any(RELATED == 4410 & HV104 == 2)),0,NA))))
  
  ## Niece/nephew by marriage (4813) #####
  df <- df |>
    mutate(
      MOMLOC2 = ifelse(!is.na(MOMLOC2), MOMLOC2,   
                       ifelse((RELATED == 4813 & !any(RELATED == 4900 | RELATED == 4920))|
                                (RELATED == 4813 & !any((RELATED == 4900 | RELATED == 4920) & HV104 == 2)),0,
                              NA)))
  
  
  ## OTHER RELATIVE (4900) #####
  df <- df |>
    mutate(
      MOMLOC2 = ifelse(!is.na(MOMLOC2), MOMLOC2,
                       ifelse((RELATED == 4900 & !any(RELATED == 4900 & HV104 == 2))| # ok
                                (RELATED == 4900 & sum(RELATED == 4900) == 1)| # ok
                                (RELATED == 4900 & HV105 >= max(HV105[RELATED == 4900])),0,NA)))
  
  ## Non-relative, n.e.c. (5900) #####
  df <- df |>
    mutate(
      MOMLOC2 = ifelse(!is.na(MOMLOC2), MOMLOC2,
                       ifelse((RELATED == 5900 & !any(RELATED == 5900 & HV104 == 2))| # ok
                                (RELATED == 5900 & sum(RELATED == 5900) == 1)|# ok
                                (RELATED == 5900 & HV105 == max(HV105[RELATED == 5900]))| # ok
                                (RELATED == 5900 & HV105 <= max(HV105[RELATED == 5900])), 0,NA)))
  
  # RESIDUALS
  df <- df |>
    mutate(
      MOMLOC2 = ifelse(!is.na(MOMLOC2), POPLOC2,
                       ifelse(RELATED==4100 & HV114>0,HV112,NA)))
  
} 

system.time(
  POPLOC_DHS <- parLapply(cl, listPOP, FUN_POPLOC_DHS)
)


POPLOC_DHS_DF <- data.table::rbindlist(POPLOC_DHS)

# as.data.frame(colSums(is.na(POPLOC_DHS_DF[, c("POPLOC2","MOMLOC2")])))
# 
# df_filtered <- POPLOC_DHS_DF %>%
#   group_by(HHID) %>%
#   filter(any(is.na(MOMLOC2))) %>%
#   ungroup()
# 
# 
# napoploc<-POPLOC_DHS_DF|>
#   group_by(RELATED,POPLOC2)|>
#   summarise(n=n())|>
#   mutate(n_rel = round(n / sum(n)*100,2))


#### SPLOC #######

listPOPLOC <- split(POPLOC_DHS_DF , f = POPLOC_DHS_DF$HHID2)

FUN_SPLOC_DHS <-  function(df) {
  library(tidyverse)
  library(haven)
  library(tibble)
  
  # GENERAL #####
  # General condition: if two persons in household are the father and mother
  # of a household member, then those two persons are spouses. 
  df<-df|>
    mutate(
      SPLOC2 = map_dbl(HVIDX, ~ ifelse(any(.x %in% POPLOC & MOMLOC != 0),
                                       MOMLOC[POPLOC == .x & MOMLOC != 0],
                                       ifelse(any(.x %in% MOMLOC & POPLOC != 0),
                                              POPLOC[MOMLOC == .x & POPLOC != 0],
                                              NA_real_)))
    )
  
  ## HEAD #####
  df <- df |>
    mutate(
      SPLOC2 = ifelse(!is.na(SPLOC2), SPLOC2, 
                      ifelse((RELATED == 1000 & any(RELATED==2100)), HVIDX[RELATED == 2100],
                             ifelse((RELATED == 1000 & any(RELATED==2200)), HVIDX[RELATED == 2200],
                                    ifelse((RELATED == 1000 & !any(RELATED==2100)),0,
                                           ifelse((RELATED == 1000 & !any(RELATED==2200)),0,
                                                  NA))))))
  
  # PARTNER #####
  df <- df |>
    mutate(
      SPLOC2 = ifelse(!is.na(SPLOC2), SPLOC2, 
                      ifelse((RELATED == 2100 & any(RELATED==1000)), HVIDX[RELATED == 1000],
                             ifelse((RELATED == 2110 & any(RELATED==1000)), HVIDX[RELATED == 1000],
                                    ifelse((RELATED == 2100 & !any(RELATED==1000)),0,
                                           ifelse((RELATED == 2110 & !any(RELATED==1000)),0,
                                                  NA))))))
  
  # CHILDREN #####
  df <- df |>
    mutate(
      SPLOC2 = ifelse(!is.na(SPLOC2), SPLOC2,
                      ifelse((RELATED == 3100 & !any(RELATED==3400)),0,
                             ifelse((RELATED == 3210 & !any(RELATED==3400)),0,
                                    ifelse((RELATED == 3400 & !any(RELATED==3100)),0,
                                           ifelse((RELATED == 3100 & AGE<=15),0,
                                                  ifelse((RELATED == 3210 & AGE<=15),0,NA)))))))
  
  df <- df |>
    mutate(
      SPLOC2 = ifelse(!is.na(SPLOC2), SPLOC2,
                      ifelse(((RELATED == 3100 & HV104==1) & sum(RELATED==3100)==1) & 
                               ((RELATED == 3400 & HV104==2) & sum(RELATED==3400)==1),
                             HVIDX[RELATED == 3400 & HV104==2],
                             
                             ifelse(((RELATED == 3400 & HV104==2) & sum(RELATED==3400)==1) & 
                                      ((RELATED == 3100 & HV104==1) & sum(RELATED==3100)==1),
                                    HVIDX[RELATED == 3100 & HV104==1],       
                                    
                                    ifelse(((RELATED == 3100 & HV104==2) & sum(RELATED==3100)==1 & 
                                              sum(RELATED==3400& HV104==1)==1),
                                           HVIDX[RELATED == 3400 & HV104==1],
                                           
                                           ifelse((RELATED == 3100 & HV104==1) & lead(RELATED == 3400 & HV104==2), lead(HVIDX),
                                                  ifelse((RELATED == 3210 & HV104==1) & lead(RELATED == 3400 & HV104==2), lead(HVIDX),
                                                         ifelse((RELATED == 3400 & HV104==2) &  lag(RELATED == 3100 & HV104==1), lag(HVIDX),
                                                                ifelse((RELATED == 3400 & HV104==2) &  lag(RELATED == 3210 & HV104==1), lag(HVIDX),
                                                                       
                                                                       ifelse((RELATED == 3100 & HV104==2) & lead(RELATED == 3400 & HV104==1), lead(HVIDX),
                                                                              ifelse((RELATED == 3210 & HV104==2) & lead(RELATED == 3400 & HV104==1), lead(HVIDX),
                                                                                     ifelse((RELATED == 3400 & HV104==1) &  lag(RELATED == 3100 & HV104==2), lag(HVIDX),
                                                                                            ifelse((RELATED == 3400 & HV104==1) &  lag(RELATED == 3210 & HV104==2), lag(HVIDX),
                                                                                                   
                                                                                                   NA)))))))))))))
  
  
  df <- df %>%
    mutate(
      all_na_related_3100 = all(RELATED[is.na(SPLOC2)] == 3100, na.rm = TRUE),
      
      SPLOC2 =ifelse(!is.na(SPLOC2), SPLOC2,
                     ifelse(RELATED == 3100 & all_na_related_3100 &is.na(SPLOC2), 0, NA))
    ) %>%
    select(-all_na_related_3100) 
  
  # GRANDCHILDREN #####
  df <- df |>
    mutate(
      SPLOC2 = ifelse(!is.na(SPLOC2), SPLOC2,
                      ifelse(RELATED==4100 & !any(RELATED==4900),0,
                             ifelse(RELATED==4100 & AGE<=15,0,
                                    NA))))
  
  df <- df |>
    mutate(
      SPLOC2 = ifelse(!is.na(SPLOC2), SPLOC2,
                      ifelse((RELATED == 4100 &  HV104==1) & lead(RELATED == 4900 &  HV104==2), lead(HVIDX),
                             ifelse((RELATED == 4900 &  HV104==2) & lag(RELATED == 4100 &  HV104==1), lag(HVIDX),
                                    NA))))
  
  df <- df |>
    mutate(
      SPLOC2 = ifelse(!is.na(SPLOC2), SPLOC2,
                      ifelse((RELATED == 4100 &  HV104==2) & lead(RELATED == 4900 &  HV104==1), lead(HVIDX),
                             ifelse((RELATED == 4900 &  HV104==1) & lag(RELATED == 4100 &  HV104==2), lag(HVIDX),
                                    NA))))
  
  df <- df %>%
    mutate(
      all_na_related_3100 = all(RELATED[is.na(SPLOC2)] == 4100, na.rm = TRUE),
      
      SPLOC2 =ifelse(!is.na(SPLOC2), SPLOC2,
                     ifelse(RELATED == 4100 & all_na_related_3100 &is.na(SPLOC2), 0, NA))
    ) %>%
    select(-all_na_related_3100)
  
  # PARENTS #####
  df <- df |>
    mutate(
      SPLOC2 = ifelse(!is.na(SPLOC2), SPLOC2,
                      ifelse(RELATED == 4210 & sum(RELATED==4210)==1,0,
                             ifelse((RELATED == 4210 & HV104==1) & any(RELATED == 4210 & HV104==2), 
                                    HVIDX[RELATED == 4210 & HV104==2],
                                    ifelse((RELATED == 4210 & HV104==2) & any(RELATED == 4210 & HV104==1), 
                                           HVIDX[RELATED == 4210 & HV104==1],
                                           NA)))))
  
  # PARENTS IN LAW #####
  df <- df |>
    mutate(
      SPLOC2 = ifelse(!is.na(SPLOC2), SPLOC2,
                      ifelse(RELATED == 4220 & sum(RELATED==4220)==1,0,
                             ifelse((RELATED == 4220 & HV104==1) & any(RELATED == 4220 & HV104==2), 
                                    HVIDX[RELATED == 4220 & HV104==2],
                                    ifelse((RELATED == 4220 & HV104==2) & any(RELATED == 4220 & HV104==1), 
                                           HVIDX[RELATED == 4220 & HV104==1],
                                           NA)))))
  
  
  # SIBLING #####
  df <- df |>
    mutate(
      SPLOC2 = ifelse(!is.na(SPLOC2), SPLOC2,
                      ifelse(RELATED == 4410 & !any(RELATED==4900),0,
                             NA)))
  
  df <- df |>
    mutate(
      SPLOC2 = ifelse(!is.na(SPLOC2), SPLOC2,
                      ifelse((RELATED == 4410 &  HV104==1) & lead(RELATED == 4900 &  HV104==2), lead(HVIDX),
                             ifelse((RELATED == 4900 &  HV104==2) & lag(RELATED == 4410 &  HV104==1), lag(HVIDX),
                                    NA))))
  
  df <- df |>
    mutate(
      SPLOC2 = ifelse(!is.na(SPLOC2), SPLOC2,
                      ifelse((RELATED == 4410 &  HV104==2) & lead(RELATED == 4900 &  HV104==1), lead(HVIDX),
                             ifelse((RELATED == 4900 &  HV104==1) & lag(RELATED == 4410 &  HV104==2), lag(HVIDX),
                                    NA))))
  
  df <- df %>%
    mutate(
      all_na_related_3100 = all(RELATED[is.na(SPLOC2)] == 4410, na.rm = TRUE),
      
      SPLOC2 =ifelse(!is.na(SPLOC2), SPLOC2,
                     ifelse(RELATED == 4410 & all_na_related_3100 &is.na(SPLOC2), 0, NA))
    ) %>%
    select(-all_na_related_3100) 
  
  # GRANDCHILD ####
  # NICE NEPHEW BY BLOOD ####
  df <- df %>%
    mutate(
      all_na_related_3100 = all(RELATED[is.na(SPLOC2)] == 4812, na.rm = TRUE),
      
      SPLOC2 =ifelse(!is.na(SPLOC2), SPLOC2,
                     ifelse(RELATED == 4812 & all_na_related_3100 &is.na(SPLOC2), 0, NA))
    ) %>%
    select(-all_na_related_3100)
  
  
  # 5900 #####
  df <- df |>
    mutate(
      SPLOC2 = ifelse(!is.na(SPLOC2), SPLOC2,
                      ifelse(RELATED == 5900 & sum(RELATED==5900)==1,0,
                             NA)))
  
  # RESIDUALS #####
  
  df <- df %>%
    mutate(
      all_na_related_3100 = all(RELATED[is.na(SPLOC2)] == 3100, na.rm = TRUE),
      
      SPLOC2 =ifelse(!is.na(SPLOC2), SPLOC2,
                     ifelse(RELATED == 3100 & all_na_related_3100 &is.na(SPLOC2), 0, NA))
    ) %>%
    select(-all_na_related_3100) 
  
  df <- df |>
    mutate(
      SPLOC2 = ifelse(!is.na(SPLOC2), SPLOC2,
                      ifelse(RELATED == 3400 & lead(RELATED) == 3100, lead(HVIDX),
                             ifelse(RELATED == 3100 &  lag(RELATED) == 3400, lag(HVIDX),
                                    NA))))
  
  
  
  # RESIDUALS #####
  
  df <- df %>%
    mutate(
      SPLOC2 =ifelse(!is.na(SPLOC2), SPLOC2,
                     ifelse(AGE<=15, 0, NA)))  
  
  df <- df %>%
    mutate(
      SPLOC2 =ifelse(!is.na(SPLOC2), SPLOC2,
                     ifelse(RELATED==3100 & !any(RELATED[is.na(SPLOC2)] == 3400), 0, NA)))  
  
  df <- df %>%
    mutate(
      SPLOC2 =ifelse(!is.na(SPLOC2), SPLOC2,
                     ifelse(HV115==0, 0, NA)))
  
  df <- df %>%
    mutate(
      SPLOC2 =ifelse(!is.na(SPLOC2), SPLOC2,
                     ifelse(is.na(SPLOC2) & sum(is.na(SPLOC2)) == 1,0,NA)))
  
  df <- df |>
    mutate(
      SPLOC2 = ifelse(!is.na(SPLOC2), SPLOC2,
                      ifelse(is.na(SPLOC2) & n_distinct(HV104[is.na(SPLOC2)]) == 1, 
                             0, NA)))
  
  # Functions #####
  # Define a function to find the matching PERNUM for the condition or return 0
  find_matching_pernum <- function(sploc2, related, hv104, data) {
    if (is.na(sploc2) && related == 3100 && hv104 == 1) {
      # Find the HVIDX with related 3400 and HV104 2
      match <- data %>%
        filter(is.na(SPLOC2), RELATED == 3400, HV104 == 2) %>%
        pull(HVIDX)
      # If there is a match, return the PERNUM, otherwise return 0
      return(ifelse(length(match) > 0, match[1], NA))
    } else {
      return(NA)
    }
  }
  
  find_matching_pernum2 <- function(sploc2, related, hv104, data) {
    if (is.na(sploc2) && related == 3400 && hv104 == 2) {
      # Find the HVIDX with related 3400 and HV104 2
      match <- data %>%
        filter(is.na(SPLOC2), RELATED == 3100, HV104 == 1) %>%
        pull(HVIDX)
      # If there is a match, return the PERNUM, otherwise return 0
      return(ifelse(length(match) > 0, match[1], NA))
    } else {
      return(NA)
    }
  }
  
  
  
  # Add the new column using the function
  df <- df %>%
    rowwise() %>%
    mutate(
      SPLOC2 =ifelse(!is.na(SPLOC2), SPLOC2,
                     find_matching_pernum(SPLOC2, RELATED, HV104, df))
    )%>%
    mutate(
      SPLOC2 =ifelse(!is.na(SPLOC2), SPLOC2,
                     find_matching_pernum2(SPLOC2, RELATED, HV104, df))
    )%>%
    ungroup()
  
  # Functions #####
  # Define a function to find the matching PERNUM for the condition or return 0
  find_matching_pernum3 <- function(sploc2, related, hv104, data) {
    if (is.na(sploc2) && related == 4410 && hv104 == 1) {
      # Find the HVIDX with related 3400 and HV104 2
      match <- data %>%
        filter(is.na(SPLOC2), RELATED == 4900, HV104 == 2) %>%
        pull(HVIDX)
      # If there is a match, return the PERNUM, otherwise return 0
      return(ifelse(length(match) > 0, match[1], NA))
    } else {
      return(NA)
    }
  }
  
  find_matching_pernum4 <- function(sploc2, related, hv104, data) {
    if (is.na(sploc2) && related == 4900 && hv104 == 2) {
      # Find the HVIDX with related 3400 and HV104 2
      match <- data %>%
        filter(is.na(SPLOC2), RELATED == 4410, HV104 == 1) %>%
        pull(HVIDX)
      # If there is a match, return the PERNUM, otherwise return 0
      return(ifelse(length(match) > 0, match[1], NA))
    } else {
      return(NA)
    }
  }
  
  
  
  # Add the new column using the function
  df <- df %>%
    rowwise() %>%
    mutate(
      SPLOC2 =ifelse(!is.na(SPLOC2), SPLOC2,
                     find_matching_pernum3(SPLOC2, RELATED, HV104, df))
    )%>%
    mutate(
      SPLOC2 =ifelse(!is.na(SPLOC2), SPLOC2,
                     find_matching_pernum4(SPLOC2, RELATED, HV104, df))
    )%>%
    ungroup()
  
  
  
}  

system.time(
  SPLOC_DHS <- parLapply(cl, listPOPLOC, FUN_SPLOC_DHS)
)


SPLOC_DHS_DF <- data.table::rbindlist(SPLOC_DHS)    

out<-as.data.frame(colSums(is.na(SPLOC_DHS_DF[, c("POPLOC2","MOMLOC2", "SPLOC2")])))
colnames(out)[1]<-"NA values"

out<-out|>mutate(variable=row.names(out))|>
  relocate(variable, .before = `NA values`)
rownames(out) <- NULL

napoploc<-SPLOC_DHS_DF|>
  group_by(RELATED,SPLOC2)|>
  summarise(n=n())|>
  mutate(n_rel = round(n / sum(n)*100,2))



df<-SPLOC_DHS_DF
df[,29]<-NULL

df<-df|>select(-POPLOC,-MOMLOC,-SPLOC,-cnd)

save(df, file="G:\\Shared drives\\CORESIDENCE\\WP2_DATA\\2_4_CORESIDENCE_DATABASE\\CORESIDENCE_LIVING_ARRANGEMENTS\\CORE_FILES\\DHS_TO_GELAI\\ALB_2017_DHS_GELAI.Rda")

