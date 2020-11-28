library(tidyverse)

#### read in data ####
readmit1 <- readxl::read_xlsx("../data/BEH_IP_discharges_2018CY_deidentified_3.xlsx", sheet = 1)
readmit2 <- readxl::read_xlsx("../data/BEH_IP_discharges_2018CY_deidentified_3.xlsx", sheet = 2)
readmit3 <- readxl::read_xlsx("../data/BEH_IP_discharges_2018CY_deidentified_3.xlsx", sheet = 3)

diagnoses <- readxl::read_xlsx("../data/icd10.xlsx", sheet = 1)
diagnoses <- dplyr::distinct(diagnoses)

#### break down diagnoses into their categories #### 
psychotic <- diagnoses %>% dplyr::filter(type == "psychotic") %>% dplyr::select(code)
personality <- diagnoses %>% dplyr::filter(type == "personality") %>% dplyr::select(code)
anxiety <- diagnoses %>% dplyr::filter(type == "anxiety") %>% dplyr::select(code)
mood <- diagnoses %>% dplyr::filter(type == "mood") %>% dplyr::select(code)
substance <- diagnoses %>% dplyr::filter(type == "substance") %>% dplyr::select(code)
medical <- diagnoses %>% dplyr::filter(type == "medical") %>% dplyr::select(code)

other <- diagnoses %>% dplyr::filter(type == "other") %>% dplyr::select(code)
Exclude <- diagnoses %>% dplyr::filter(type == "Exclude") %>% dplyr::select(code)

#### dealing with the diagnoses ####

# remove the diagnoses that are more descriptors than diagnoses
exclude_function <- function(variable){
  ifelse(variable %in% Exclude$code, NA, variable)
}

diags <- names(readmit1[17:35])

#### DO NOT TRY TO PIPE THIS IN ONE GO #####
readmit1 <- readmit1 %>% 
  dplyr::mutate_at(diags, exclude_function)

readmit1 <- readmit1 %>% 
  dplyr::mutate(sum_diags = rowSums(!is.na(readmit1[,16:35])),
                primary_dx = case_when(PrimDx %in% psychotic$code ~ "psychotic",
                                       PrimDx %in% personality$code ~ "personality",
                                       PrimDx %in% anxiety$code ~ "anxiety",
                                       PrimDx %in% mood$code ~ "mood",
                                       PrimDx %in% substance$code ~ "substance",
                                       PrimDx %in% medical$code ~ "medical",
                                       TRUE ~ "other")) 

readmit1 <- readmit1 %>%  
  tidyr::unite(all_diagnoses, c(names(readmit1[17:35])), # now all the diagnoses are from 17 to 36
        sep = " ", remove = FALSE, na.rm = TRUE) 

####
readmit1 <- readmit1 %>% 
  dplyr::mutate(psychotic = grepl(paste(psychotic$code, collapse = "|"), all_diagnoses),
         personality = grepl(paste(personality$code, collapse = "|"), all_diagnoses),
         anxiety = grepl(paste(anxiety$code, collapse = "|"), all_diagnoses),
         mood = grepl(paste(mood$code, collapse = "|"), all_diagnoses),
         other = grepl(paste(other$code, collapse = "|"), all_diagnoses),
         medical_count = NA,
         substance_count = NA,
         mood_count = NA,
         psychotic_count = NA,
         anxiety_count = NA,
         personality_count = NA,
         other_count = NA)


for (i in 1:nrow(readmit1)){
  readmit1$medical_count[i] <- sum(str_extract_all(readmit1$all_diagnoses[i], 
                                                   paste(medical$code, collapse = "|"))[[1]] %in% medical$code)
  readmit1$substance_count[i] <- sum(str_extract_all(readmit1$all_diagnoses[i], 
                                                     paste(substance$code, collapse = "|"))[[1]] %in% substance$code)
  readmit1$mood_count[i] <- sum(str_extract_all(readmit1$all_diagnoses[i],
                                                paste(mood$code, collapse = "|"))[[1]] %in% mood$code)
  readmit1$psychotic_count[i] <- sum(str_extract_all(readmit1$all_diagnoses[i],
                                                     paste(psychotic$code, collapse = "|"))[[1]] %in% psychotic$code)
  readmit1$anxiety_count[i] <- sum(str_extract_all(readmit1$all_diagnoses[i],
                                                   paste(anxiety$code, collapse = "|"))[[1]] %in% anxiety$code)
  readmit1$personality_count[i] <- sum(str_extract_all(readmit1$all_diagnoses[i],
                                                       paste(personality$code, collapse = "|"))[[1]] %in% personality$code)
  readmit1$other_count[i] <- sum(str_extract_all(readmit1$all_diagnoses[i],
                                                 paste(other$code, collapse = "|"))[[1]] %in% other$code)
  }


sum(rowSums(readmit1[c(44:50)]) == readmit1$sum_diags-1) # SANITY CHECK 

readmit1 <- readmit1 %>% 
  dplyr::mutate(insurance = case_when(PrimPyrRptGrp %in% c("Commercial", "Commercial Managed Care") ~ "Private",
                                      PrimPyrRptGrp %in% c("Medicaid", "Medicaid MC", "Medicare", "Medicare MC") ~ "Public",
                                      TRUE ~ "other"),
                race = case_when(Ethnicity == "Hispanic" ~ "Hispanic",
                                 TRUE ~ Race),
                race = case_when(race %in% c("Other", "Unknown") ~ "Other",
                                 TRUE ~ race))


#### readmit2 ###
diags2 <- names(readmit2[17:35])

#### DO NOT TRY TO PIPE THIS IN ONE GO #####
readmit2 <- readmit2 %>% 
  dplyr::mutate_at(diags2, exclude_function)

readmit2 <- readmit2 %>% 
  dplyr::mutate(sum_diags = rowSums(!is.na(readmit2[,16:35])),
                primary_dx = case_when(PrimDx %in% psychotic$code ~ "psychotic",
                                       PrimDx %in% personality$code ~ "personality",
                                       PrimDx %in% anxiety$code ~ "anxiety",
                                       PrimDx %in% mood$code ~ "mood",
                                       PrimDx %in% substance$code ~ "substance",
                                       PrimDx %in% medical$code ~ "medical",
                                       TRUE ~ "other")) 

readmit2 <- readmit2 %>%  
  tidyr::unite(all_diagnoses, c(names(readmit2[17:35])), # now all the diagnoses are from 17 to 36
               sep = " ", remove = FALSE, na.rm = TRUE) 

####
readmit2 <- readmit2 %>% 
  dplyr::mutate(psychotic = grepl(paste(psychotic$code, collapse = "|"), all_diagnoses),
                personality = grepl(paste(personality$code, collapse = "|"), all_diagnoses),
                anxiety = grepl(paste(anxiety$code, collapse = "|"), all_diagnoses),
                mood = grepl(paste(mood$code, collapse = "|"), all_diagnoses),
                other = grepl(paste(other$code, collapse = "|"), all_diagnoses),
                medical_count = NA,
                substance_count = NA,
                mood_count = NA,
                psychotic_count = NA,
                anxiety_count = NA,
                personality_count = NA,
                other_count = NA)


for (i in 1:nrow(readmit2)){
  readmit2$medical_count[i] <- sum(str_extract_all(readmit2$all_diagnoses[i], 
                                                   paste(medical$code, collapse = "|"))[[1]] %in% medical$code)
  readmit2$substance_count[i] <- sum(str_extract_all(readmit2$all_diagnoses[i], 
                                                     paste(substance$code, collapse = "|"))[[1]] %in% substance$code)
  readmit2$mood_count[i] <- sum(str_extract_all(readmit2$all_diagnoses[i],
                                                paste(mood$code, collapse = "|"))[[1]] %in% mood$code)
  readmit2$psychotic_count[i] <- sum(str_extract_all(readmit2$all_diagnoses[i],
                                                     paste(psychotic$code, collapse = "|"))[[1]] %in% psychotic$code)
  readmit2$anxiety_count[i] <- sum(str_extract_all(readmit2$all_diagnoses[i],
                                                   paste(anxiety$code, collapse = "|"))[[1]] %in% anxiety$code)
  readmit2$personality_count[i] <- sum(str_extract_all(readmit2$all_diagnoses[i],
                                                       paste(personality$code, collapse = "|"))[[1]] %in% personality$code)
  readmit2$other_count[i] <- sum(str_extract_all(readmit2$all_diagnoses[i],
                                                 paste(other$code, collapse = "|"))[[1]] %in% other$code)
}


sum(rowSums(readmit2[c(44:50)]) == readmit2$sum_diags-1) # SANITY CHECK 

readmit2 <- readmit2 %>% 
  dplyr::mutate(insurance = case_when(PrimPyrRptGrp %in% c("Commercial", "Commercial Managed Care") ~ "Private",
                                      PrimPyrRptGrp %in% c("Medicaid", "Medicaid MC", "Medicare", "Medicare MC") ~ "Public",
                                      TRUE ~ "other"),
                race = case_when(Ethnicity == "Hispanic" ~ "Hispanic",
                                 TRUE ~ Race),
                race = case_when(race %in% c("Other", "Unknown") ~ "Other",
                                 TRUE ~ race))



#### Readmissions Cleaning (readmit3) ####
readmit3 <- readmit3 %>% 
  group_by(PT) %>% 
  mutate(mrn_number = first(MRN)) %>% 
  ungroup()

readmit_3 <- c(13,14,15,25,26,34,36,37,47,64,67,77,79,91,105,106)

before_readmit <- readmit3 %>% 
  filter(PT %in% readmit3$PT[readmit3$ReAd30 == 1]) 

before_readmit_3 <- before_readmit %>% 
  filter(PT %in% readmit_3)

before_readmit <- before_readmit %>% 
  filter(!PT %in% readmit_3) %>% 
  group_by(PT) %>% 
  slice(1) %>% 
  ungroup()

before_readmit_3 <- before_readmit_3 %>% 
  mutate(ReAd30 = replace_na(ReAd30, 0)) %>% 
  filter(ReAd30 == 0 & (is.na(DaysBtwAdm) == TRUE | DaysBtwAdm == 36)) 


before_readmit_3 <- before_readmit_3[-2,]
before_readmit <- dplyr::bind_rows(before_readmit, before_readmit_3)
before_readmit <- before_readmit[which(rowMeans(!is.na(before_readmit)) > 0.5), ]

diags3 <- names(before_readmit[23:41])

#### DO NOT TRY TO PIPE THIS IN ONE GO #####
before_readmit <- before_readmit %>% 
  dplyr::mutate_at(diags3, exclude_function)

before_readmit$sum_diags <- rowSums(!is.na(before_readmit[,22:41]))

before_readmit <- before_readmit %>% 
  mutate(primary_dx = case_when(PrimDx %in% psychotic$code ~ "psychotic",
                         PrimDx %in% personality$code ~ "personality",
                         PrimDx %in% anxiety$code ~ "anxiety",
                         PrimDx %in% mood$code ~ "mood",
                         PrimDx %in% substance$code ~ "substance",
                         PrimDx %in% medical$code ~ "medical",
                         TRUE ~ "other"))

before_readmit <- before_readmit %>%  
  tidyr::unite(all_diagnoses, c(names(before_readmit[23:41])), # now all the diagnoses are from 17 to 36
               sep = " ", remove = FALSE, na.rm = TRUE) 

####
before_readmit <- before_readmit %>% 
  dplyr::mutate(psychotic = grepl(paste(psychotic$code, collapse = "|"), all_diagnoses),
                personality = grepl(paste(personality$code, collapse = "|"), all_diagnoses),
                anxiety = grepl(paste(anxiety$code, collapse = "|"), all_diagnoses),
                mood = grepl(paste(mood$code, collapse = "|"), all_diagnoses),
                other = grepl(paste(other$code, collapse = "|"), all_diagnoses)) 


before_readmit$medical_count <- rep(0, nrow(before_readmit))
before_readmit$substance_count <- rep(0, nrow(before_readmit))
before_readmit$mood_count <- rep(0, nrow(before_readmit))
before_readmit$psychotic_count <- rep(0, nrow(before_readmit))
before_readmit$anxiety_count <- rep(0, nrow(before_readmit))
before_readmit$personality_count <- rep(0, nrow(before_readmit))
before_readmit$other_count <- rep(0, nrow(before_readmit))


for (i in 1:nrow(before_readmit)){
  before_readmit$medical_count[i] <- sum(str_extract_all(before_readmit$all_diagnoses[i], 
                                                   paste(medical$code, collapse = "|"))[[1]] %in% medical$code)
  before_readmit$substance_count[i] <- sum(str_extract_all(before_readmit$all_diagnoses[i], 
                                                     paste(substance$code, collapse = "|"))[[1]] %in% substance$code)
  before_readmit$mood_count[i] <- sum(str_extract_all(before_readmit$all_diagnoses[i],
                                                paste(mood$code, collapse = "|"))[[1]] %in% mood$code)
  before_readmit$psychotic_count[i] <- sum(str_extract_all(before_readmit$all_diagnoses[i],
                                                     paste(psychotic$code, collapse = "|"))[[1]] %in% psychotic$code)
  before_readmit$anxiety_count[i] <- sum(str_extract_all(before_readmit$all_diagnoses[i],
                                                   paste(anxiety$code, collapse = "|"))[[1]] %in% anxiety$code)
  before_readmit$personality_count[i] <- sum(str_extract_all(before_readmit$all_diagnoses[i],
                                                       paste(personality$code, collapse = "|"))[[1]] %in% personality$code)
  before_readmit$other_count[i] <- sum(str_extract_all(before_readmit$all_diagnoses[i],
                                                 paste(other$code, collapse = "|"))[[1]] %in% other$code)
}


sum(rowSums(before_readmit[c(51:57)]) == before_readmit$sum_diags-1)# SANITY CHECK 

before_readmit <- before_readmit %>% 
  dplyr::mutate(insurance = case_when(PrimPyrRptGrp %in% c("Commercial", "Commercial Managed Care") ~ "Private",
                                      PrimPyrRptGrp %in% c("Medicaid", "Medicaid MC", "Medicare", "Medicare MC") ~ "Public",
                                      TRUE ~ "other"),
                race = case_when(Ethnicity == "Hispanic" ~ "Hispanic",
                                 TRUE ~ Race),
                race = case_when(race %in% c("Other", "Unknown") ~ "Other",
                                 TRUE ~ race))

before_readmit


#### Combining the data sets (readmit2 and readmit) #### 
before_readmit <- before_readmit %>% 
  mutate(MRN = mrn_number,
         readmit = 1) %>% 
  select(MRN, Sex, Age, race, PES_12Month, LOS, Attending, sum_diags, primary_dx, psychotic, personality,
         anxiety, mood, other, medical_count, substance_count, mood_count, psychotic_count, anxiety_count,
         personality_count, other_count, insurance, readmit)

readmit2 <- readmit2 %>% 
  mutate(readmit = 0,
         Sex = Gender) %>% 
  select(MRN, Sex, Age, race, PES_12Month, LOS, Attending, sum_diags, primary_dx, psychotic, personality,
         anxiety, mood, other, medical_count, substance_count, mood_count, psychotic_count, anxiety_count,
         personality_count, other_count, insurance, readmit)
  
  
full_admit <- dplyr::bind_rows(before_readmit, readmit2)

full_admit$other_diagnoses_count <- rowSums(full_admit[c(10,12:14)])

