exclude_function <- function(variable){
  ifelse(variable %in% Exclude$code, NA, variable)
}


diag_cleaning_function <- function(data, start_diag, end_diag) {
  diags <- names(data[start_diag:end_diag])
  #### DO NOT TRY TO PIPE THIS IN ONE GO #####
  data <- data %>% 
    dplyr::mutate_at(diags, exclude_function)
  data <- data %>% 
    dplyr::mutate(sum_diags = rowSums(!is.na(data[,start_diag-1:end_diag])),
                  primary_dx = case_when(PrimDx %in% psychotic$code ~ "psychotic",
                                         PrimDx %in% personality$code ~ "personality",
                                         PrimDx %in% anxiety$code ~ "anxiety",
                                         PrimDx %in% mood$code ~ "mood",
                                         PrimDx %in% substance$code ~ "substance",
                                         PrimDx %in% medical$code ~ "medical",
                                         TRUE ~ "other")) 
  
  data <- data %>%  
    tidyr::unite(all_diagnoses, c(names(data[start_diag:end_diag])), # now all the diagnoses are from 17 to 36
                 sep = " ", remove = FALSE, na.rm = TRUE) 
  data <- data %>% 
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
  for (i in 1:nrow(data)){
    data$medical_count[i] <- sum(str_extract_all(data$all_diagnoses[i], 
                                                     paste(medical$code, collapse = "|"))[[1]] %in% medical$code)
    data$substance_count[i] <- sum(str_extract_all(data$all_diagnoses[i], 
                                                       paste(substance$code, collapse = "|"))[[1]] %in% substance$code)
    data$mood_count[i] <- sum(str_extract_all(data$all_diagnoses[i],
                                                  paste(mood$code, collapse = "|"))[[1]] %in% mood$code)
    data$psychotic_count[i] <- sum(str_extract_all(data$all_diagnoses[i],
                                                       paste(psychotic$code, collapse = "|"))[[1]] %in% psychotic$code)
    data$anxiety_count[i] <- sum(str_extract_all(data$all_diagnoses[i],
                                                     paste(anxiety$code, collapse = "|"))[[1]] %in% anxiety$code)
    data$personality_count[i] <- sum(str_extract_all(data$all_diagnoses[i],
                                                         paste(personality$code, collapse = "|"))[[1]] %in% personality$code)
    data$other_count[i] <- sum(str_extract_all(data$all_diagnoses[i],
                                                   paste(other$code, collapse = "|"))[[1]] %in% other$code)
  }
  data <- data %>% 
    dplyr::mutate(insurance = case_when(PrimPyrRptGrp %in% c("Commercial", "Commercial Managed Care") ~ "Private",
                                        PrimPyrRptGrp %in% c("Medicaid", "Medicaid MC", "Medicare", "Medicare MC") ~ "Public",
                                        TRUE ~ "other"),
                  race = case_when(Ethnicity == "Hispanic" ~ "Hispanic",
                                   TRUE ~ Race),
                  race = case_when(race %in% c("Other", "Unknown") ~ "Other",
                                   TRUE ~ race))
  
}

