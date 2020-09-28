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

fit1 <- glm(readmit ~ as.factor(Sex) + as.factor(race) + Age , data = train_data)

fit2 <- glm(readmit ~ as.factor(Sex) + as.factor(race) + Age + PES_12Month + LOS + 
              as.factor(insurance), data = train_data)

fit3 <- glm(readmit ~ as.factor(Sex) + as.factor(race) + Age + PES_12Month + LOS +
              as.factor(primary_dx) + medical_count + substance_count + personality_count + 
              other_diagnoses_count -1, data = train_data, family = "binomial")

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
as.numeric(perf@y.values[[1]]) # 0.5331556


### AUC for fit2
prob2 <- predict(fit2, test_data, type = "response")
## Compute AUC
pred2 <- ROCR::prediction(prob2, test_data$readmit)
perf2 <- ROCR::performance(pred2, "auc")
as.numeric(perf2@y.values[[1]]) #0.6272901


### AUC for fit3
prob3 <- predict(fit3, newdata = test_data, type = "response")
## Compute AUC
pred3 <- ROCR::prediction(prob3, test_data$readmit)
perf3 <- ROCR::performance(pred3, "auc")
as.numeric(perf3@y.values[[1]]) #0.756426


### AUC for fit4
prob4 <- predict(fit4, newdata = test_data, type = "response")
## Compute AUC
pred4 <- ROCR::prediction(prob4, test_data$readmit)
perf4 <- ROCR::performance(pred4, "auc")
as.numeric(perf4@y.values[[1]]) #0.7742002


### AUC for fit5
prob5 <- predict(fit5, newdata = test_data, type = "response")
## Compute AUC
pred5 <- ROCR::prediction(prob5, test_data$readmit)
perf5 <- ROCR::performance(pred5, "auc")
as.numeric(perf5@y.values[[1]]) #0.7753623

##### 

summary(fit3)



#### Random Forest Model ####
train_data <- train_data %>% 
  select(readmit, Sex, race, Age, PES_12Month, LOS, primary_dx, medical_count, 
         substance_count, personality_count, other_diagnoses_count) %>% 
  mutate_at(c("Sex", "race", "primary_dx"), as.factor)

rfmodel_opt <- ranger::ranger(as.factor(readmit) ~ .,
                        data = train_data, num.trees = 1000, mtry = 2, splitrule = "gini", 
                        respect.unordered.factors = TRUE, probability = TRUE, importance = "impurity")

test.rf <- test_data %>% 
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

