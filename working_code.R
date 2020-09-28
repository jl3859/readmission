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


mood_count = NA,
psychotic_count = NA,
anxiety_count = NA,
personality_count = NA,
other_count = NA)