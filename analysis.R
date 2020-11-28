library(caret)
library(ranger)
library(ROCR)

source("datacleaning.R")

#### create dataset with 50/50 readmit 
temp <- caret::upSample(full_admit, as.factor(full_admit$readmit))
temp <- na.omit(temp)
sample_n <- sample.int(nrow (temp), floor(0.80 * nrow(temp)), replace = F)
# Create a train dataset with the sampled data
train_data <- temp[sample_n, ]
table(train_data$readmit)
test_data <- temp[-sample_n, ]

index_fit <- glm(readmit ~ index , data = train_data)
prob_in <- predict(index_fit, newdata = test_data, type = "response")
## Compute AUC
pred_in <- ROCR::prediction(prob_in, test_data$readmit)
perf_in <- ROCR::performance(pred_in, "auc")
as.numeric(perf_in@y.values[[1]])


fit1 <- glm(readmit ~ as.factor(Sex) + as.factor(race) + Age , data = train_data)

fit2 <- glm(readmit ~ as.factor(Sex) + as.factor(race) + Age + PES_12Month + LOS + 
              as.factor(insurance), data = train_data)


fit3 <- glm(readmit ~ as.factor(Sex) + as.factor(race) + Age + PES_12Month + LOS +
              as.factor(primary_dx) + medical_count + substance_count + personality_count + 
              other_diagnoses_count, data = train_data, family = "binomial")

fit4 <- glm(readmit ~ as.factor(Sex) + as.factor(race) + Age + PES_12Month + LOS +
                      as.factor(primary_dx) + medical_count + substance_count + personality_count + 
                      other_diagnoses_count + as.factor(primary_dx)*medical_count, data = train_data)

fit5 <- glm(readmit ~ as.factor(Sex) + as.factor(race) + Age + PES_12Month + LOS +
              as.factor(primary_dx) + medical_count + substance_count + personality_count + 
              other_diagnoses_count + as.factor(primary_dx)*medical_count +
              as.factor(primary_dx)*substance_count, data = train_data)

### AUC for fit1
prob <- predict(fit1, newdata = test_data, type = "response")
## Compute AUC
pred <- ROCR::prediction(prob, test_data$readmit)
perf <- ROCR::performance(pred, "auc")
as.numeric(perf@y.values[[1]]) # 0.6283566


### AUC for fit2
prob2 <- predict(fit2, test_data, type = "response")
## Compute AUC
pred2 <- ROCR::prediction(prob2, test_data$readmit)
perf2 <- ROCR::performance(pred2, "auc")
as.numeric(perf2@y.values[[1]]) #0.6818475


### AUC for fit3
prob3 <- predict(fit3, newdata = test_data, type = "response")
## Compute AUC
pred3 <- ROCR::prediction(prob3, test_data$readmit)
perf3 <- ROCR::performance(pred3, "auc")
as.numeric(perf3@y.values[[1]]) #0.7732904


### AUC for fit4
prob4 <- predict(fit4, newdata = test_data, type = "response")
## Compute AUC
pred4 <- ROCR::prediction(prob4, test_data$readmit)
perf4 <- ROCR::performance(pred4, "auc")
as.numeric(perf4@y.values[[1]]) #0.7930541


### AUC for fit5
prob5 <- predict(fit5, newdata = test_data, type = "response")
## Compute AUC
pred5 <- ROCR::prediction(prob5, test_data$readmit)
perf5 <- ROCR::performance(pred5, "auc")
as.numeric(perf5@y.values[[1]]) #0.7935553

##### 
fit1_coefs <- data.frame(summary(fit1)$coef)
fit1_coefs$pvalue_adj <- p.adjust(fit1_coefs$Pr...t.., method = "bonferroni")
write.csv(fit1_coefs, file = "fit1_coefs.csv")

fit2_coefs <- data.frame(summary(fit2)$coef)
fit2_coefs$pvalue_adj <- p.adjust(fit2_coefs$Pr...t.., method = "bonferroni")
write.csv(fit2_coefs, file = "fit2_coefs.csv")

fit3_coefs <- data.frame(summary(fit3)$coef)
fit3_coefs$pvalue_adj <- p.adjust(fit3_coefs$Pr...z.., method = "bonferroni")
write.csv(fit3_coefs, file = "fit3_coefs.csv")

fit4_coefs <- data.frame(summary(fit4)$coef)
fit4_coefs$pvalue_adj <- p.adjust(fit4_coefs$Pr...t.., method = "bonferroni")
write.csv(fit4_coefs, file = "fit4_coefs.csv")

fit5_coefs <- data.frame(summary(fit5)$coef)
fit5_coefs$pvalue_adj <- p.adjust(fit5_coefs$Pr...t.., method = "bonferroni")
write.csv(fit5_coefs, file = "fit5_coefs.csv")



#### Random Forest Model ####
train_data_rf <- train_data %>% 
  select(readmit, Sex, race, Age, PES_12Month, LOS, primary_dx, medical_count, 
         is_substance, personality_count, other_diagnoses_count) %>% 
  mutate_at(c("Sex", "race", "primary_dx"), as.factor)

rfmodel_opt <- ranger::ranger(as.factor(readmit) ~ .,
                        data = train_data_rf, num.trees = 1000, mtry = 2, splitrule = "gini", 
                        respect.unordered.factors = TRUE, probability = TRUE, importance = "impurity")

test.rf <- test_data %>% 
  select(readmit, Sex, race, Age, PES_12Month, LOS, primary_dx, medical_count, 
         substance_count, personality_count, other_diagnoses_count) %>% 
  mutate(opt.prob = predict(rfmodel_opt, test_data, type = 'response')$predictions[,2])

pred.rf <- ROCR::prediction(test.rf$opt.prob, test_data$readmit)
perf.rf <- ROCR::performance(pred.rf, "auc")
opt_AUC <- perf.rf@y.values[[1]]
cat('The AUC of random forest model is:', opt_AUC)


important.variables <- data.frame(importance = rfmodel_opt$variable.importance)
important.variables$predictors <- rownames(important.variables)
important.variables <- important.variables %>% arrange(desc(importance))
important.variables$predictors <- tolower(important.variables$predictors)

ggplot(data = important.variables, aes(x = predictors, y = importance)) + 
  labs(x = "Predictors", y = "Importance") + 
  geom_bar(stat = "identity") + theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

fit1_age <- glm(readmit ~ as.factor(Sex) + as.factor(race) + as.factor(age_group) , data = train_data
                ,family = "binomial")

fit2_age <- glm(readmit ~ as.factor(Sex) + as.factor(race) + as.factor(age_group) + PES_12Month + LOS + 
              as.factor(insurance), data = train_data, family = "binomial")

fit3_age <- glm(readmit ~ as.factor(Sex) + as.factor(race) + as.factor(age_group) + PES_12Month + LOS +
                  as.factor(primary_dx) + medical_count + substance_count + personality_count + 
                  other_diagnoses_count, data = train_data, family = "binomial")


fit4_age <- glm(readmit ~ as.factor(Sex) + as.factor(race) + as.factor(age_group) + PES_12Month + LOS +
              as.factor(primary_dx) + medical_count + substance_count + personality_count + 
              other_diagnoses_count + as.factor(primary_dx)*medical_count, data = train_data)

fit5_age <- glm(readmit ~ as.factor(Sex) + as.factor(race) + as.factor(age_group) + PES_12Month + LOS +
              as.factor(primary_dx) + medical_count + substance_count + personality_count + 
              other_diagnoses_count + as.factor(primary_dx)*medical_count +
              as.factor(primary_dx)*substance_count, data = train_data)


### AUC for fit1
prob_age <- predict(fit1_age, newdata = test_data, type = "response")
## Compute AUC
pred_age <- ROCR::prediction(prob_age, test_data$readmit)
perf_age <- ROCR::performance(pred_age, "auc")
as.numeric(perf_age@y.values[[1]]) # 0.5891071


### AUC for fit2
prob2_age <- predict(fit2_age, test_data, type = "response")
## Compute AUC
pred2_age <- ROCR::prediction(prob2_age, test_data$readmit)
perf2_age <- ROCR::performance(pred2_age, "auc")
as.numeric(perf2_age@y.values[[1]]) #0.7121948


### AUC for fit3
prob3_age <- predict(fit3_age, newdata = test_data, type = "response")
## Compute AUC
pred3_age <- ROCR::prediction(prob3_age, test_data$readmit)
perf3_age <- ROCR::performance(pred3_age, "auc")
as.numeric(perf3_age@y.values[[1]]) #0.7657399


### AUC for fit4
prob4_age <- predict(fit4_age, newdata = test_data, type = "response")
## Compute AUC
pred4_age <- ROCR::prediction(prob4_age, test_data$readmit)
perf4_age <- ROCR::performance(pred4_age, "auc")
as.numeric(perf4_age@y.values[[1]]) #0.7817741


### AUC for fit5
prob5_age <- predict(fit5_age, newdata = test_data, type = "response")
## Compute AUC
pred5_age <- ROCR::prediction(prob5_age, test_data$readmit)
perf5_age <- ROCR::performance(pred5_age, "auc")
as.numeric(perf5_age@y.values[[1]]) #0.7879523


train_data_rf <- train_data %>% 
  select(readmit, Sex, race, age_group, PES_12Month, LOS, primary_dx, medical_count, 
         substance_count, personality_count, other_diagnoses_count) %>% 
  mutate_at(c("Sex", "race", "primary_dx"), as.factor)

rfmodel_opt <- ranger::ranger(as.factor(readmit) ~ .,
                              data = train_data_rf, num.trees = 1000, mtry = 2, splitrule = "gini", 
                              respect.unordered.factors = TRUE, probability = TRUE, importance = "impurity")

test.rf <- test_data %>% 
  select(readmit, Sex, race, Age, PES_12Month, LOS, primary_dx, medical_count, 
         substance_count, personality_count, other_diagnoses_count) %>% 
  mutate(opt.prob = predict(rfmodel_opt, test_data, type = 'response')$predictions[,2])

pred.rf <- ROCR::prediction(test.rf$opt.prob, test_data$readmit)
perf.rf <- ROCR::performance(pred.rf, "auc")
opt_AUC <- perf.rf@y.values[[1]]
cat('The AUC of random forest model is:', opt_AUC)


important.variables <- data.frame(importance = rfmodel_opt$variable.importance)
important.variables$predictors <- rownames(important.variables)
important.variables <- important.variables %>% arrange(desc(importance))
important.variables$predictors <- tolower(important.variables$predictors)

ggplot(data = important.variables, aes(x = predictors, y = importance)) + 
  labs(x = "Predictors", y = "Importance") + 
  geom_bar(stat = "identity") + theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
