library(dplyr)
library(mgcv)
library(ggplot2)
library(tidyr)

heart = read.csv('/Users/mbp/Documents/DDS/DS/ML/AS2/heart_failure_clinical_records_dataset.csv')
# targetName <- "DEATH_EVENT"
# let's put the target label last on the right 
# patients_data <- patients_data%>%select(-targetName,targetName)
# target_index <- dim(patients_data)[2]

library(corrplot)
bcp <- cor(heart[,1:13])
corrplot.mixed(bcp) 


glimpse(heart)
summary(heart)
head(heart)
pairs(heart)
names(heart)
plot(heart$age, heart$DEATH_EVENT)
plot(heart$anaemia)
summarise(heart)

heart$anaemia <- as.factor(heart$anaemia)
heart$diabetes <- as.factor(heart$diabetes)
heart$high_blood_pressure <- as.factor(heart$high_blood_pressure)
heart$sex <- as.factor(heart$sex)
heart$DEATH_EVENT <- as.factor(heart$DEATH_EVENT)


library(mgcv)
set.seed('5')

patients_dataset <- heart[sample(nrow(heart)),] # shuffle the rows
# patients_dataset <- heart%>%select(-targetName,targetName)

# split training set and test set
training_set_perce <- 80
# the training set is the first training_set_perce% of the whole dataset
training_set_first_index <- 1 # NEW
training_set_last_index <- round(dim(heart)[1]*training_set_perce/100) # NEW

test_set_first_index <- training_set_last_index + 1
test_set_last_index <- dim(heart)[1] # NEW

prc_data_train <- heart[training_set_first_index:training_set_last_index, ] 
prc_data_test <- heart[test_set_first_index:test_set_last_index, ] 

target_index <- dim(heart)[2]
prc_data_train_labels <- prc_data_train[, target_index] 
prc_data_test_labels <- prc_data_test[, target_index] 



gam_model <- gam(DEATH_EVENT ~ sex + smoking + diabetes + high_blood_pressure + anaemia + s(age) + s(ejection_fraction) + s(serum_sodium) + s(serum_creatinine) + s(platelets) + s(creatinine_phosphokinase) + s(time), data = prc_data_train, family = "binomial")

summary(gam_model)

par(mfrow = c(3, 3))
plot(gam_model,select = 1, pch = 20, shade = TRUE, residuals = TRUE)
plot(gam_model,select = 2, pch = 20, shade = TRUE, residuals = TRUE)
plot(gam_model,select = 3, pch = 20, shade = TRUE, residuals = TRUE)
plot(gam_model,select = 4, pch = 20, shade = TRUE, residuals = TRUE)
plot(gam_model,select = 5, pch = 20, shade = TRUE, residuals = TRUE)
plot(gam_model,select = 6, pch = 20, shade = TRUE, residuals = TRUE)
plot(gam_model,select = 7, pch = 20, shade = TRUE, residuals = TRUE)
plot(gam_model,select = 8, pch = 20, shade = TRUE, residuals = TRUE)
plot(gam_model,select = 9, pch = 20, shade = TRUE, residuals = TRUE)
plot(gam_model,select = 10, pch = 20, shade = TRUE, residuals = TRUE)
plot(gam_model,select = 11, pch = 20, shade = TRUE, residuals = TRUE)
plot(gam_model,select = 12, pch = 20, shade = TRUE, residuals = TRUE)

par(mfrow = c(2, 3))
plot(gam_model, residuals = TRUE)

gam_prediction = predict(gam_model, prc_data_test)
Predict<-rep(0,dim(prc_data_test)[1])
Predict[gam_prediction>=0.5]=1
Actual<-prc_data_test$DEATH_EVENT
table(Predict, Actual)
library(caret)
confusionMatrix(, positive = '0')



par(mfrow = c(1, 1))
library(pROC)
roc_obj <- roc(response = prc_data_test$DEATH_EVENT, predictor = predict(gam_model,prc_data_test), levels = c("0", "1"), direction = "<")
auc_val <- auc(roc_obj)


plot(roc_obj,xlab = "False Positive Rate or [1 - True Negative Rate]", ylab = "True Positive Rate", legend("bottomright", legend = paste("AUC =", round(auc_val, 2))))

