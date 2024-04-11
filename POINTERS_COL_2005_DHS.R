library(purrr)
library(knitr)

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

load("I:\\Shared drives\\CORESIDENCE\\WP2_DATA\\2_4_CORESIDENCE_DATABASE\\CORESIDENCE_LIVING_ARRANGEMENTS\\CORE_FILES\\DHS_TO_DB/COL_2005_DHS.RData")

df<-`COL_2005_DHS`
df<-df|>
  mutate(RELATED1=as_factor(RELATED))

df<-df|>
  mutate(HV112=ifelse(RELATED1=="Biological child"&HV112==0&HV114==0,NA,HV112),
         HV114=ifelse(RELATED1=="Biological child"&HV112==0&HV114==0,NA,HV114))


no_cores <- detectCores() -68
registerDoParallel(cores = no_cores)
cl <- makeCluster(no_cores)

listPOP <- split(df, f = df$HHID2)
unique(df$RELATED)

pruebas<-listPOP[["00010001"]]
df<-pruebas

#### POPLOC ######
FUN_POPLOC_DHS <- function(df) {
  library(tidyverse)
  library(haven)
  library(tibble)
  
  #POPLOC ######
  df <- df |>
    mutate(
      POPLOC2 = ifelse(HV114 >= 0, HV114, NA_real_)
    )
  
  ## CHILDREN 3100, 3210 #####
  df <- df |>
    mutate(
      POPLOC2 = ifelse(!is.na(POPLOC2), POPLOC2,   
                       ifelse(RELATED %in% c(3100, 3210) & any(RELATED == 1000 & HV104 == 1), 
                              HVIDX[which.max(RELATED == 1000 & HV104 == 1)],
                              ifelse(RELATED  %in% c(3100, 3210) & HV219 == 2 & any(RELATED %in% c(2100, 2200) & HV104 == 1), 
                                     HVIDX[which.max(RELATED %in% c(2100, 2110) & HV104 == 1)],
                                     ifelse(RELATED %in% c(3100, 3210), 0, NA)
                              )
                       )
      )
    )
  
  ## HEAD 1000 #####
  df <- df |> 
    mutate(
      POPLOC2 = ifelse(!is.na(POPLOC2), POPLOC2,   
                       ifelse(RELATED == 1000 & any(RELATED == 4210 & HV104 == 1), 
                              HVIDX[which(RELATED == 4210 & HV104 == 1)][1],
                              ifelse(RELATED == 1000 & (!any(RELATED == 4210) |
                                                          (sum(RELATED == 4210) == 1 & any(RELATED == 4210 & HV104 != 1))), 0,
                                     NA)))
    )
  
  ## PARTNER 2100 #####
  df <- df |> 
    mutate(
      POPLOC2 = ifelse(!is.na(POPLOC2), POPLOC2,   
                       ifelse((RELATED %in% c(2100, 2110) & any(RELATED == 4220 & HV104 == 1)), 
                              HVIDX[which(RELATED == 4220 & HV104 == 1)][1],
                              ifelse((RELATED %in% c(2100, 2110) & !any(RELATED == 4220)) | 
                                       (RELATED %in% c(2100, 2110) & sum(RELATED == 4220) == 1 & any(RELATED == 4220 & HV104 != 1)), 0,
                                     NA)))
    )
  
  ## CHILD IN LAW 3400 #####
  df <- df |>
    mutate(
      POPLOC2 = ifelse(!is.na(POPLOC2), POPLOC2,   
                       ifelse(RELATED == 3400 & (
                         !any(RELATED == 4900 | RELATED == 4920) |
                           !any((RELATED == 4900 | RELATED == 4920) & HV104 == 1) |
                           (max(HV105[RELATED == 4900], na.rm = TRUE) - HV105 < 20)
                       ), 0, NA))
    )
  
  ## GRANDCHILD 4100 #####
  df <- df |>
    mutate(
      POPLOC2 = ifelse(!is.na(POPLOC2), POPLOC2,   
                       ifelse(RELATED == 4100 & sum(RELATED == 3100 & HV104 == 1) == 1, 
                              HVIDX[which(RELATED == 3100 & HV104 == 1)][1], # Ensure selection of the specific HVIDX
                              ifelse(RELATED == 4100 & sum(RELATED == 3400 & HV104 == 1) == 1, 
                                     HVIDX[which(RELATED == 3400 & HV104 == 1)][1], # Ensure selection of the specific HVIDX
                                     ifelse(RELATED == 4100 & !any((RELATED == 3100 | RELATED == 3400) & HV104 == 1), 
                                            0, 
                                            NA))))
    )
  
  ## PARENTS 4210 #####
  df <- df |>
    mutate(
      max_HV105_4900 = max(HV105[RELATED == 4900], na.rm = TRUE), # Calculate once for efficiency
      POPLOC2 = ifelse(!is.na(POPLOC2), POPLOC2,   
                       ifelse(
                         RELATED == 4210 & (
                           !any(RELATED == 4900 | RELATED == 4920) |
                             !any((RELATED == 4900 | RELATED == 4920) & HV104 == 1) |
                             (max_HV105_4900 - HV105 < 20)
                         ), 0, NA)
      )
    ) %>%
    select(-max_HV105_4900)
  
  
  ## PARENTS IN LAW 4220 #####
  df <- df |>
    mutate(
      max_HV105_4900 = max(HV105[RELATED == 4900], na.rm = TRUE), # Calculate the maximum once for efficiency
      POPLOC2 = ifelse(!is.na(POPLOC2), POPLOC2,   
                       ifelse(RELATED == 4220 & (
                         !any(RELATED == 4900 | RELATED == 4920) |
                           !any((RELATED == 4900 | RELATED == 4920) & HV104 == 1) |
                           (max_HV105_4900 - HV105 < 20)
                       ), 0, NA))
    ) %>%
    select(-max_HV105_4900) # Clean up by removing the auxiliary column
  
  ## SIBLING 4410 #####
  df <- df %>%
    mutate(
      first_POPLOC2_1000 = first(POPLOC2[RELATED == 1000], default = NA), # Get the first POPLOC2 value for RELATED == 1000
      POPLOC2 = ifelse(!is.na(POPLOC2), POPLOC2,
                       ifelse(RELATED == 4410, first_POPLOC2_1000, NA))
    ) %>%
    select(-first_POPLOC2_1000) # Remove the auxiliary column
  
  df <- df %>%
    mutate(
      POPLOC2 = ifelse(!is.na(POPLOC2), POPLOC2,  
                       ifelse(RELATED == 4100 & lead(RELATED) == 4100 & !is.na(lead(POPLOC2)),
                              lead(POPLOC2), NA))
    )
  
  ## SIBLING 4430 #####
  df <- df %>%
    mutate(
      first_POPLOC2_1000 = first(POPLOC2[RELATED == 2100], default = NA), # Get the first POPLOC2 value for RELATED == 1000
      POPLOC2 = ifelse(!is.na(POPLOC2), POPLOC2,
                       ifelse(RELATED == 4430, first_POPLOC2_1000, NA))
    ) %>%
    select(-first_POPLOC2_1000) # Remove the auxiliary column
  
  ## Niece/nephew by blood (4812) #####
  df <- df |>
    mutate(
      POPLOC2 = ifelse(!is.na(POPLOC2), POPLOC2,   
                       ifelse(RELATED == 4812 & any(RELATED == 4410 & HV104 == 1), 
                              HVIDX[which(RELATED == 4410 & HV104 == 1)][1], # Assuming you want the first matching HVIDX
                              ifelse(RELATED == 4812 & !any(RELATED == 4410 & HV104 == 1), 
                                     0, 
                                     NA)))
    )
  
  ## Niece/nephew by marriage (4813) #####
  df <- df |>
    mutate(
      POPLOC2 = ifelse(!is.na(POPLOC2), POPLOC2,   
                       ifelse(RELATED == 4813 & !any((RELATED == 4900 | RELATED == 4920) & HV104 == 1), 0, NA))
    )
  
  
  ## OTHER RELATIVE (4900) #####
  df <- df %>%
    mutate(
      max_HV105_4900 = max(HV105[RELATED == 4900], na.rm = TRUE), # Calculate max HV105 for RELATED == 4900
      POPLOC2 = ifelse(!is.na(POPLOC2), POPLOC2,
                       ifelse(RELATED == 4900 & 
                                (!any(RELATED == 4900 & HV104 == 1) | 
                                   sum(RELATED == 4900) == 1 | 
                                   HV105 >= max_HV105_4900), 0, NA))
    ) %>%
    select(-max_HV105_4900) # Remove auxiliary column after use
  
  ## Non-relative, n.e.c. (5900) #####
  df <- df |>
    mutate(
      max_HV105_5900 = max(HV105[RELATED == 5900], na.rm = TRUE), # Calculate max HV105 for RELATED == 5900 once for efficiency
      POPLOC2 = ifelse(!is.na(POPLOC2), POPLOC2,
                       ifelse(RELATED == 5900 & 
                                (!any(RELATED == 5900 & HV104 == 1) | 
                                   sum(RELATED == 5900) == 1 | 
                                   HV105 == max_HV105_5900 | 
                                   HV105 < max_HV105_5900), 0, NA))
    ) %>%
    select(-max_HV105_5900) # Remove auxiliary column after use
  
  ## domestic employww (5210) #####
  df <- df |>
    mutate(
      max_HV105_5210 = max(HV105[RELATED == 5210], na.rm = TRUE), # Calculate max HV105 for RELATED == 5900 once for efficiency
      POPLOC2 = ifelse(!is.na(POPLOC2), POPLOC2,
                       ifelse(RELATED == 5210 & 
                                (!any(RELATED == 5210 & HV104 == 1) | 
                                   sum(RELATED == 5210) == 1 | 
                                   HV105 == max_HV105_5210 | 
                                   HV105 < max_HV105_5210), 0, NA))
    ) %>%
    select(-max_HV105_5210) # Remove auxiliary column after use
  
  
  
  # RESIDUALS ####
  df <- df |>
    mutate(
      POPLOC2 = ifelse(!is.na(POPLOC2), POPLOC2,
                       ifelse(RELATED==4100 & HV112>0,HV114,NA)))
  
  
  ### MOMLOC #####
  df <- df |>
    mutate(
      MOMLOC2 = ifelse(HV112>=0, HV112,NA))
  
  
  ## CHILDREN #####
  df <- df |>
    mutate(
      MOMLOC2 = ifelse(!is.na(MOMLOC2), MOMLOC2,   
                       ifelse(RELATED %in% c(3100, 3210) & any(RELATED == 1000 & HV104 == 2), 
                              HVIDX[which.max(RELATED == 1000 & HV104 == 2)],
                              ifelse(RELATED  %in% c(3100, 3210) & HV219 == 1 & any(RELATED %in% c(2100, 2200) & HV104 == 2), 
                                     HVIDX[which.max(RELATED %in% c(2100, 2110) & HV104 == 2)],
                                     ifelse(RELATED %in% c(3100, 3210), 0, 
                                            NA)))))
  
  
  
  ## HEAD #####
  df <- df |> 
    mutate(
      MOMLOC2 = ifelse(!is.na(MOMLOC2), MOMLOC2,   
                       ifelse(RELATED == 1000 & any(RELATED == 4210 & HV104 == 2), 
                              HVIDX[which(RELATED == 4210 & HV104 == 2)][1],
                              ifelse(RELATED == 1000 & (!any(RELATED == 4210) |
                                                          (sum(RELATED == 4210) == 1 & any(RELATED == 4210 & HV104 != 2))), 0,
                                     NA))))
  
  
  
  ## PARTNER #####
  df <- df |> 
    mutate(
      MOMLOC2 = ifelse(!is.na(MOMLOC2), MOMLOC2,   
                       ifelse((RELATED %in% c(2100, 2200) & any(RELATED == 4220 & HV104 == 2)), 
                              HVIDX[which(RELATED == 4220 & HV104 == 2)][1],
                              ifelse((RELATED %in% c(2100, 2110) & !any(RELATED == 4220)) | 
                                       (RELATED %in% c(2100, 2110) & sum(RELATED == 4220) == 1 & any(RELATED == 4220 & HV104 != 2)), 0,
                                     NA)))
    )
  
  
  ## CHILD IN LAW #####
  df <- df |>
    mutate(
      MOMLOC2 = ifelse(!is.na(MOMLOC2), MOMLOC2,   
                       ifelse(RELATED == 3400 & (
                         !any(RELATED == 4900 | RELATED == 4920) |
                           !any((RELATED == 4900 | RELATED == 4920) & HV104 == 2) |
                           (max(HV105[RELATED == 4900], na.rm = TRUE) - HV105 < 20)
                       ), 0, NA))
    )
  
  
  ## GRANDCHILD #####
  df <- df |>
    mutate(
      MOMLOC2 = ifelse(!is.na(MOMLOC2), MOMLOC2,   
                       ifelse(RELATED == 4100 & sum(RELATED == 3100 & HV104 == 2) == 1, 
                              HVIDX[which(RELATED == 3100 & HV104 == 2)][1], # Ensure selection of the specific HVIDX
                              ifelse(RELATED == 4100 & sum(RELATED == 3400 & HV104 == 2) == 1, 
                                     HVIDX[which(RELATED == 3400 & HV104 == 2)][1], # Ensure selection of the specific HVIDX
                                     ifelse(RELATED == 4100 & !any((RELATED == 3100 | RELATED == 3400) & HV104 == 2), 
                                            0, 
                                            NA))))
    )
  
  ## PARENTS #####
  df <- df |>
    mutate(
      max_HV105_4900 = max(HV105[RELATED == 4900], na.rm = TRUE), # Calculate once for efficiency
      MOMLOC2 = ifelse(!is.na(MOMLOC2), MOMLOC2,   
                       ifelse(
                         RELATED == 4210 & (
                           !any(RELATED == 4900 | RELATED == 4920) |
                             !any((RELATED == 4900 | RELATED == 4920) & HV104 == 2) |
                             (max_HV105_4900 - HV105 < 20)
                         ), 0, NA)
      )
    ) %>%
    select(-max_HV105_4900)
  
  ## PARENTS IN LAW #####
  df <- df |>
    mutate(
      max_HV105_4900 = max(HV105[RELATED == 4900], na.rm = TRUE), # Calculate the maximum once for efficiency
      MOMLOC2 = ifelse(!is.na(MOMLOC2), MOMLOC2,   
                       ifelse(RELATED == 4220 & (
                         !any(RELATED == 4900 | RELATED == 4920) |
                           !any((RELATED == 4900 | RELATED == 4920) & HV104 == 2) |
                           (max_HV105_4900 - HV105 < 20)
                       ), 0, NA))
    ) %>%
    select(-max_HV105_4900) # Clean up by removing the auxiliary column
  ## SIBLING #####
  df <- df %>%
    mutate(
      first_MOMLOC2_1000 = first(MOMLOC2[RELATED == 1000], default = NA), # Get the first POPLOC2 value for RELATED == 1000
      MOMLOC2 = ifelse(!is.na(MOMLOC2), MOMLOC2,
                       ifelse(RELATED == 4410, first_MOMLOC2_1000, NA))
    ) %>%
    select(-first_MOMLOC2_1000) # Remove the auxiliary column
  
  df <- df %>%
    mutate(
      MOMLOC2 = ifelse(!is.na(MOMLOC2), MOMLOC2,  
                       ifelse(RELATED == 4100 & lead(RELATED) == 4100 & !is.na(lead(MOMLOC2)),
                              lead(MOMLOC2), NA))
    )
  
  
  ## SIBLING 4430 #####
  df <- df %>%
    mutate(
      first_MOMLOC2_1000 = first(MOMLOC2[RELATED == 2100], default = NA), # Get the first POPLOC2 value for RELATED == 1000
      MOMLOC2 = ifelse(!is.na(MOMLOC2), MOMLOC2,
                       ifelse(RELATED == 4430, first_MOMLOC2_1000, NA))
    ) %>%
    select(-first_MOMLOC2_1000) # Remove the auxiliary column
  
  ## Niece/nephew by blood (4812) #####
  df <- df |>
    mutate(
      MOMLOC2 = ifelse(!is.na(MOMLOC2), MOMLOC2,   
                       ifelse(RELATED == 4812 & any(RELATED == 4410 & HV104 == 2), 
                              HVIDX[which(RELATED == 4410 & HV104 == 2)][1], # Assuming you want the first matching HVIDX
                              ifelse(RELATED == 4812 & !any(RELATED == 4410 & HV104 == 2), 
                                     0, 
                                     NA)))
    )
  
  
  ## Niece/nephew by marriage (4813) #####
  df <- df |>
    mutate(
      MOMLOC2 = ifelse(!is.na(MOMLOC2), MOMLOC2,   
                       ifelse(RELATED == 4813 & !any((RELATED == 4900 | RELATED == 4920) & HV104 == 2), 0, NA))
    )
  
  
  ## OTHER RELATIVE (4900) #####
  df <- df %>%
    mutate(
      max_HV105_4900 = max(HV105[RELATED == 4900], na.rm = TRUE), # Calculate max HV105 for RELATED == 4900
      MOMLOC2 = ifelse(!is.na(MOMLOC2), MOMLOC2,
                       ifelse(RELATED == 4900 & 
                                (!any(RELATED == 4900 & HV104 == 2) | 
                                   sum(RELATED == 4900) == 1 | 
                                   HV105 >= max_HV105_4900), 0, NA))
    ) %>%
    select(-max_HV105_4900) # Remove auxiliary column after use
  
  ## Non-relative, n.e.c. (5900) #####
  df <- df |>
    mutate(
      max_HV105_5900 = max(HV105[RELATED == 5900], na.rm = TRUE), # Calculate max HV105 for RELATED == 5900 once for efficiency
      MOMLOC2 = ifelse(!is.na(MOMLOC2), MOMLOC2,
                       ifelse(RELATED == 5900 & 
                                (!any(RELATED == 5900 & HV104 == 2) | 
                                   sum(RELATED == 5900) == 1 | 
                                   HV105 == max_HV105_5900 | 
                                   HV105 < max_HV105_5900), 0, NA))
    ) %>%
    select(-max_HV105_5900) # Remove auxiliary column after use
  
  ## Non-relative, n.e.c. (5900) #####
  df <- df |>
    mutate(
      max_HV105_5210 = max(HV105[RELATED == 5210], na.rm = TRUE), # Calculate max HV105 for RELATED == 5900 once for efficiency
      MOMLOC2 = ifelse(!is.na(MOMLOC2), MOMLOC2,
                       ifelse(RELATED == 5210 & 
                                (!any(RELATED == 5210 & HV104 == 2) | 
                                   sum(RELATED == 5210) == 1 | 
                                   HV105 == max_HV105_5210 | 
                                   HV105 < max_HV105_5210), 0, NA))
    ) %>%
    select(-max_HV105_5210) # Remove auxiliary column after use
  
  # RESIDUALS #####
  df <- df |>
    mutate(
      MOMLOC2 = ifelse(!is.na(MOMLOC2), MOMLOC2,
                       ifelse(RELATED==4100 & HV114>0,HV112,NA)))
  
} 

system.time(
  POPLOC_DHS <- parLapply(cl, listPOP, FUN_POPLOC_DHS)
)


POPLOC_DHS_DF <- data.table::rbindlist(POPLOC_DHS)

as.data.frame(colSums(is.na(POPLOC_DHS_DF[, c("POPLOC2","MOMLOC2")])))


napoploc<-POPLOC_DHS_DF|>
  group_by(RELATED,POPLOC2)|>
  summarise(n=n())|>
  mutate(n_rel = round(n / sum(n)*100,2))


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
                             ifelse((RELATED == 1000 & any(RELATED==2110)), HVIDX[RELATED == 2200],
                                    ifelse((RELATED == 1000 & !any(RELATED==2100)),0,
                                           ifelse((RELATED == 1000 & !any(RELATED==2110)),0,
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
                      ifelse(RELATED == 4410 & !any(RELATED==4430),0,
                             NA)))
  
  df <- df |>
    mutate(
      SPLOC2 = ifelse(!is.na(SPLOC2), SPLOC2,
                      ifelse(RELATED == 4430 & !any(RELATED==4410),0,
                             NA)))
  
  
  df <- df |>
    mutate(
      SPLOC2 = ifelse(!is.na(SPLOC2), SPLOC2,
                      ifelse((RELATED == 4410 &  HV104==1) & lead(RELATED == 4430 &  HV104==2), lead(HVIDX),
                             ifelse((RELATED == 4430 &  HV104==2) & lag(RELATED == 4410 &  HV104==1), lag(HVIDX),
                                    NA))))
  
  df <- df |>
    mutate(
      SPLOC2 = ifelse(!is.na(SPLOC2), SPLOC2,
                      ifelse((RELATED == 4410 &  HV104==2) & lead(RELATED == 4430 &  HV104==1), lead(HVIDX),
                             ifelse((RELATED == 4430 &  HV104==1) & lag(RELATED == 4410 &  HV104==2), lag(HVIDX),
                                    NA))))
  
  df <- df %>%
    mutate(
      all_na_related_3100 = all(RELATED[is.na(SPLOC2)] == 4410, na.rm = TRUE),
      
      SPLOC2 =ifelse(!is.na(SPLOC2), SPLOC2,
                     ifelse(RELATED == 4410 & all_na_related_3100 &is.na(SPLOC2), 0, NA))
    ) %>%
    select(-all_na_related_3100) 
  
  # ### GRANDCHILD ####
  # df <- df %>%
  #   mutate(
  #     all_na_related_3100 = all(RELATED[is.na(SPLOC2)] == 4100, na.rm = TRUE),
  #     
  #     SPLOC2 =ifelse(!is.na(SPLOC2), SPLOC2,
  #                    ifelse(RELATED == 4100 & all_na_related_3100 &is.na(SPLOC2), 0, NA))
  #   ) %>%
  #   select(-all_na_related_3100)
  
  
  ### NICE NEPHEW BY BLOOD ####
  df <- df %>%
    mutate(
      all_na_related_3100 = all(RELATED[is.na(SPLOC2)] == 4812, na.rm = TRUE),
      
      SPLOC2 =ifelse(!is.na(SPLOC2), SPLOC2,
                     ifelse(RELATED == 4812 & all_na_related_3100 &is.na(SPLOC2), 0, NA))
    ) %>%
    select(-all_na_related_3100)
  
  
  ## 5900 #####
  df <- df |>
    mutate(
      SPLOC2 = ifelse(!is.na(SPLOC2), SPLOC2,
                      ifelse(RELATED == 5900 & sum(RELATED==5900)==1,0,
                             NA)))
  
  
  
  
  ## 5900 #####
  
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
  
  
  
  ## RESIDUALS #####
  
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
out

napoploc<-SPLOC_DHS_DF|>
  group_by(RELATED,SPLOC2)|>
  summarise(n=n())|>
  mutate(n_rel = round(n / sum(n)*100,2))



df<-SPLOC_DHS_DF

df<-df|>select(-POPLOC,-MOMLOC,-SPLOC,-cnd,-RELATED1)

save(df, file="I:\\Shared drives\\CORESIDENCE\\WP2_DATA\\2_4_CORESIDENCE_DATABASE\\CORESIDENCE_LIVING_ARRANGEMENTS\\CORE_FILES\\DHS_TO_GELAI\\COL_2005_DHS_GELAI.Rda")
