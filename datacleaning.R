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

diags <- names(readmit1[16:35])

readmit1 <- readmit1 %>% 
  dplyr::mutate_at(diags, exclude_function)

readmit1 <- readmit1 %>% 
  dplyr::mutate(sum_diags = rowSums(!is.na(readmit1[,16:35]))) 

readmit1 <- readmit1 %>%  
  tidyr::unite(all_diagnoses, c(names(readmit1[16:35])), # now all the diagnoses are from 17 to 36
        sep = " ", remove = FALSE, na.rm = TRUE) 

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


sum(rowSums(readmit1[43:49]) == readmit1$sum_diags)


