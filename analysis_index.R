source("datacleaning.R")


full_admit <- full_admit %>% 
  mutate(age_group = case_when(Age > 17 & Age <= 24 ~ "Emerging Adulthood",
                               Age > 25 & Age <= 34 ~ "Early Adulthood",
                               Age > 35 & Age <= 44 ~ "Early Middle Age",
                               Age > 45 & Age <= 64 ~ "Middle Adulthood",
                               Age > 65 ~ "Late Adulthood"),
         is_substance = ifelse(substance_count == 0, FALSE, TRUE))


full_admit <- full_admit %>% 
  mutate(pes_index = case_when(PES_12Month == 0 ~ 0,
                               PES_12Month == 1 | PES_12Month == 2 ~ 2,
                               PES_12Month >= 3 & PES_12Month <= 4 ~ 4,
                               PES_12Month >= 5 ~ 5,
                               TRUE ~ 5),
         los_index = case_when(LOS <= 5 ~ 5, 
                               LOS > 5 & LOS < 10 ~ 5,
                               LOS >= 10 & LOS < 15 ~ 3,
                               LOS >= 15 & LOS < 29 ~ 1,
                               TRUE ~ 0),
         primdiag_index = case_when(primary_dx == "medical" ~ 7,
                                    primary_dx == "psychotic" ~ 4,
                                    primary_dx == "mood" ~ 3,
                                    primary_dx == "substance" ~ 1,
                                    TRUE ~ 0),
         med_index = case_when(medical_count == 0 ~ 0,
                               medical_count > 0 | medical_count < 3 ~ 2,
                               TRUE ~ 4),
         person_index = case_when(personality == TRUE ~ 2,
                                  TRUE ~ 0),
         other_index = case_when(other_count == 0 ~ 0,
                                 other_count == 1 ~ 1,
                                 TRUE ~ 2))

full_admit$index <- rowSums(full_admit[c(27:32)])
hist(full_admit$index)


index_auc <- rep(0, 10000)
index_mat <- matrix(ncol = 10000, nrow = 26)
index_mat_ci <- matrix(ncol = 10000, nrow = 26)
index_dat <- data.frame(index = c(0:25))
for (i in 1:10000){
  #### create dataset with 50/50 readmit 
  temp <- caret::upSample(full_admit, as.factor(full_admit$readmit))
  temp <- na.omit(temp)
  sample_n <- sample.int(nrow(temp), floor(0.80 * nrow(temp)), replace = F)
  # Create a train dataset with the sampled data
  train_data <- temp[sample_n, ]
  table(train_data$readmit)
  test_data <- temp[-sample_n, ]
  
  index_fit <- glm(readmit ~ index, data = train_data, family = "binomial")
  prob_in <- predict(index_fit, newdata = test_data, type = "response")
  index_mat[,i] <-  predict(index_fit, newdata = index_dat, type = "response")
  index_mat_ci[,i] <- predict(index_fit, newdata = index_dat,  type = "response", 
                              se.fit = TRUE)$se.fit
  ## Compute AUC
  pred_in <- ROCR::prediction(prob_in, test_data$readmit)
  perf_in <- ROCR::performance(pred_in, "auc")
  index_auc[i] <- as.numeric(perf_in@y.values[[1]])
}
mean(index_auc) #0.745486


#### get confidence intervals 
index_fit <- glm(readmit ~ index, data = train_data, family = "binomial")
predict(index_fit, newdata = index_dat,  type = "response", se.fit = TRUE)

index_dat$pred <- rep(0, nrow(index_dat))
index_dat$ci <- rep(0, nrow(index_dat))

for (i in 1:nrow(index_dat)){
  index_dat$pred[i] <- mean(index_mat[i,])
  index_dat$ci[i] <- mean(index_mat_ci[i,])
}



ggplot(index_dat, aes(x = index, y = pred)) +
  geom_point() +
  geom_errorbar(aes(ymin=pred-1.96*ci, ymax=pred+1.96*ci), width=.1)+
  theme_classic() +
  labs(x = "Index", y = "Early Readmission Probability") +
  geom_hline(yintercept = 0.5, width = 1, type = "dashed", color = "blue")


index10 <- table(full_admit$index > 10, full_admit$readmit)
diag(index10 )[2]/(index10[2,1]+ diag(index10)[2]) # precision
diag(index10 )[2]/(index10[1,2]+ diag(index10)[2]) # recall 

index11 <- table(full_admit$index > 11, full_admit$readmit)
diag(index11)[2]/(index11[2,1]+ diag(index11)[2]) # precision
diag(index11)[2]/(index11[1,2]+ diag(index11)[2]) # recall 


index12 <- table(full_admit$index > 12, full_admit$readmit)
diag(index12)[2]/(index12[2,1]+ diag(index12)[2]) # precision
diag(index12)[2]/(index12[1,2]+ diag(index12)[2]) # recall 


index13 <- table(full_admit$index > 13, full_admit$readmit)
diag(index13)[2]/(index13[2,1]+ diag(index13)[2]) # precision
diag(index13)[2]/(index13[1,2]+ diag(index13)[2]) # recall 

