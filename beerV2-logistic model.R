##
## The Beer Preference data set is used to illustrate how to estimate a linear probability and 
## logistic regression model, and to compute the confusion matrix. Some performance measures are 
## then computed (Accuracy, Sensitivity, Specificity). 
## --------------------------------------------------------------------------------------------
##
# Read the data. 
#
Beer.Preferences <- read.csv("Beer Preferences.csv")
#
# In this case dollar amounts will be read as character strings. 
# You can change that in Excel - or in R as follows
#
#
Beer.Preferences$Income <- as.numeric(sub('$','',as.character(Beer.Preferences$Income),fixed=TRUE))
#
# Suppose we want Light to be the success class
Beer.Preferences$Preference <- factor(Beer.Preferences$Preference,levels=c("Regular","Light"))
#
#
set.seed(12345)
inTrain <- sample(nrow(Beer.Preferences), 0.6*nrow(Beer.Preferences))
#
train <- data.frame(Beer.Preferences[inTrain,])
test <- data.frame(Beer.Preferences[-inTrain,])
#
# Fit the model on the training data
fit <- glm(Preference ~ Income+Age, data = train, family = "binomial")
summary(fit)
#
Actual <- train$Preference
predicted.probability.train <- predict(fit, type = "response") 
## Note the predictions are probabilities
cutoff <- 0.5
Predicted <- ifelse(predicted.probability.train > cutoff, "Light","Regular")
Predicted <- factor(Predicted,levels=c("Regular","Light"))
# 
(confusion <- table(Actual,Predicted))
#
## This can be used to compute the performance measures for the training data at a cutoff of 0.5
## (This because Predicted was computed with a cutoff of 0.5; but any other cutoff could be used as well)
##
## Accuracy
##
(accuracy <- sum(Actual == Predicted)/nrow(train))
##
## Sensitivity
##
(sensitivity <- sum(Predicted == "Light" & Actual == "Light")/sum(Actual == "Light"))
##
## Specificity
##
(specificity <- sum(Predicted == "Regular" & Actual == "Regular")/sum(Actual == "Regular"))
##
# +-----------------------------------------------------------------------------------------------------
## Performance measures for the test set
##
cutoff <- 0.5
ActualTest <- test$Preference
predicted.probability.test <- predict(fit, type = "response", newdata = test)
PredictedTest <- ifelse( predicted.probability.test > cutoff, "Light", "Regular")
PredictedTest <- factor(PredictedTest,levels=c("Regular","Light"))
##
## Confusion matrix (out of sample)
##
(confusionTest <- table(ActualTest, PredictedTest))
##
## Accuracy
##
(accuracyTest <- sum(ActualTest == PredictedTest)/nrow(test))
##
## Sensitivity
##
(sensitivityTest <- sum(PredictedTest == "Light" & ActualTest == "Light")/sum(ActualTest == "Light"))
##
## Specificity
##
(specificityTest <- sum(PredictedTest == "Regular" & ActualTest == "Regular")/sum(ActualTest == "Regular"))
##
# +---------------------------------------------------------------------------------------------------------+
cutoff <- seq(0, 1, length = nrow(train))
acc <- numeric(nrow(train))
sen <- numeric(nrow(train))
spec <- numeric(nrow(train))

## We'll collect it in a data frame.  (We could also just keep it in three vectors)
acc.table <- data.frame(Cutoff = cutoff, ACCURACY = acc,SENSITIVITY = sen, SPECIFICITY = spec)
##
for (i in 1:nrow(train)) {
  Predicted <- ifelse(predicted.probability.train > cutoff[i], "Light","Regular")
  Predicted <- factor(Predicted,levels=c("Regular","Light"))
  # 
  confusion <- table(Actual,Predicted)
  acc.table$ACCURACY[i] <- (confusion[1,1]+confusion[2,2])/sum(confusion)
  acc.table$SENSITIVITY[i] <- sum(predicted.probability.train > cutoff[i] & Actual == "Light")/sum(Actual == "Light")
  acc.table$SPECIFICITY[i] <- sum(predicted.probability.train <= cutoff[i] & Actual == "Regular")/sum(Actual == "Regular")
}


plot(ACCURACY ~ cutoff, data = acc.table, type = "o",xlab="Cutoff",ylab="Accuracy",col="blue",lty=2, ylim=c(0,1))
lines(SENSITIVITY~cutoff,data = acc.table, type="o",col="green",lty=2)
lines(SPECIFICITY~cutoff,data = acc.table, type="o",col="red",lty=2)
z <- which.max(acc.table$ACCURACY)
cutoff[z]
