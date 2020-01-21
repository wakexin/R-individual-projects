# Individual Project

#### Kexin Wang

#### 12/1/2019
---
title: "In class Final"
date: "12/5/2019"
output: html_document
---


# Initialization
```{r}
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}
```

```{r}
fightdata <- read.csv("FightData.csv")
str(fightdata)

fightdata$DOB.x = as.character(fightdata$DOB.x)
fightdata$DOB.x = as.Date(fightdata$DOB.x, format = "%m/%d/%Y")
head(fightdata$DOB.x)
fightdata$age.x = as.numeric(Sys.Date() - fightdata$DOB.x)
fightdata$age.x <- fightdata$age.x/365

fightdata$DOB.y = as.character(fightdata$DOB.y)
fightdata$DOB.y = as.Date(fightdata$DOB.y, format = "%m/%d/%Y")
head(fightdata$DOB.y)
fightdata$age.y = as.numeric(Sys.Date() - fightdata$DOB.y)
fightdata$age.y <- fightdata$age.y/365


fightdata$DOB.x <- NULL
fightdata$DOB.y <- NULL

```

Using the data on the date of birth of the players to create new columns of the age of the players. Then delete the original columns of date of birth. 

```{r}
summary(fightdata$Stance.x)
summary(fightdata$Stance.y)
str(fightdata$Stance.x)
fightdata$Stance.x = as.character(fightdata$Stance.x)
fightdata[fightdata$Stance.x == "Open Stance"|fightdata$Stance.x == "Sideways"|fightdata$Stance.x == "Switch"|is.na(fightdata$Stance.x),]$Stance.x <- "Other"
fightdata = fastDummies::dummy_columns(fightdata, select_columns = "Stance.x")

fightdata$Stance.y = as.character(fightdata$Stance.y)
fightdata[fightdata$Stance.y == "Open Stance"|fightdata$Stance.y == "Sideways"|fightdata$Stance.y == "Switch"|is.na(fightdata$Stance.y),]$Stance.y <- "Other"
fightdata = fastDummies::dummy_columns(fightdata, select_columns = "Stance.y")


fightdata$Stance.x<-NULL
fightdata$Stance.y<-NULL

```

Looking at both the stance columns of x and y, I found Orthodox and Southpaw are the major stances of the players. Then I set all the other stances, including NA values,  under Other. TNext, I created dummy variables for stance x and stance y, and deleted the original stance.x and stance.y



```{r}

fightdata$y <- fightdata$result
fightdata$result <- NULL

colMeans(is.na(fightdata))

fightdata2 <- fightdata[complete.cases(fightdata),]
model1 <- lm(Reach.x ~ Height.x + Weight.x, data = fightdata2)
summary(model1)

predicted_Reach_X <- predict(model1, newdata = fightdata)
Reach_X_comp <- data.frame(predicted_Reach_X, fightdata$Reach.x)


fightdata$Reach.x <- ifelse(is.na(fightdata$Reach.x), predicted_Reach_X, fightdata$Reach.x)


model2 <- lm(Reach.y ~ Height.y + Weight.y, data = fightdata2)
summary(model2)

predicted_Reach_Y <- predict(model2, newdata = fightdata)
Reach_Y_comp <- data.frame(predicted_Reach_Y, fightdata$Reach.y)
fightdata$Reach.y <- ifelse(is.na(fightdata$Reach.y), predicted_Reach_Y, fightdata$Reach.y)

colMeans(is.na(fightdata))
str(fightdata)
```

For the ease of implementing models, I created a new column and copy my result column and paste them into the column y, so it can appear at the rightest of my dataset. Next, I deleted the column result, because all the information is now in the column y. 

The next move is to deal with the N/A values occured in my dataset. Using the function colMeans(is.na(fightdata)), I found Reach.x and Reach.y having the highest percentageof N/A values in my dataset. I want to find out how weight and height have on one's reach. I used complete cases to create a new dataset and has no missing values. Running a linear regression model, I found both height and weight are significant in predicting the reach of the fighter. Then I used Height.x and Weight.x to predicted the corresponding N/A values in Reach.x. I used Height.y and Weight.y to predicted the corresponding N/A values in Reach.y. 

```{r}

meanSAPm.x <- mean(fightdata[fightdata$SApM.x != 0,]$SApM.x)
meanSAPm.y <- mean(fightdata[fightdata$SApM.y != 0,]$SApM.y)
fightdata$SApM.x <- ifelse(fightdata$SApM.x == 0, meanSAPm.x, fightdata$SApM.x)
fightdata$SApM.y <- ifelse(fightdata$SApM.y == 0, meanSAPm.y, fightdata$SApM.y)


meanSLpM.x <- mean(fightdata[fightdata$SLpM.x != 0,]$SLpM.x)
meanSLpM.y <- mean(fightdata[fightdata$SLpM.y != 0,]$SLpM.y)
fightdata$SLpM.x <- ifelse(fightdata$SLpM.x == 0, meanSLpM.x, fightdata$SLpM.x)
fightdata$SLpM.y <- ifelse(fightdata$SLpM.y == 0, meanSLpM.y, fightdata$SLpM.y)

meanStr..Acc..x <- mean(fightdata[fightdata$Str..Acc..x != 0,]$Str..Acc..x)
meanStr..Acc..y <- mean(fightdata[fightdata$Str..Acc..y != 0,]$Str..Acc..y)
fightdata$Str..Acc..x <- ifelse(fightdata$Str..Acc..x == 0, meanStr..Acc..x, fightdata$Str..Acc..x)
fightdata$Str..Acc..y <- ifelse(fightdata$Str..Acc..y == 0, meanStr..Acc..y, fightdata$Str..Acc..y)

meanStr..Def..x <- mean(fightdata[fightdata$Str..Def..x!= 0,]$Str..Def..x)
meanStr..Def..y <- mean(fightdata[fightdata$Str..Def..y!= 0,]$Str..Def..y)
fightdata$Str..Def..x <- ifelse(fightdata$Str..Def..x  == 0, meanStr..Def..x, fightdata$Str..Def..x)
fightdata$Str..Def..y <- ifelse(fightdata$Str..Def..y  == 0, meanStr..Def..y, fightdata$Str..Def..y)

meanTD.Avg..x <- mean(fightdata[fightdata$TD.Avg..x!= 0,]$TD.Avg..x)
meanTD.Avg..y <- mean(fightdata[fightdata$TD.Avg..y!= 0,]$TD.Avg..y)
fightdata$TD.Avg..x <- ifelse(fightdata$TD.Avg..x  == 0, meanTD.Avg..x, fightdata$TD.Avg..x)
fightdata$TD.Avg..y <- ifelse(fightdata$TD.Avg..y  == 0, meanTD.Avg..y, fightdata$TD.Avg..y)

meanTD.Acc..x <- mean(fightdata[fightdata$TD.Acc..x!= 0,]$TD.Acc..x)
meanTD.Acc..y <- mean(fightdata[fightdata$TD.Acc..y!= 0,]$TD.Acc..y)
fightdata$TD.Acc..x <- ifelse(fightdata$TD.Acc..x  == 0, meanTD.Acc..x, fightdata$TD.Acc..x)
fightdata$TD.Acc..y <- ifelse(fightdata$TD.Acc..y  == 0, meanTD.Acc..y, fightdata$TD.Acc..y)

meanTD.Def..x <- mean(fightdata[fightdata$TD.Def..x != 0,]$TD.Def..x)
meanTD.Def..y <- mean(fightdata[fightdata$TD.Def..y!= 0,]$TD.Def..y)
fightdata$TD.Def..x <- ifelse(fightdata$TD.Def..x == 0, meanTD.Def..x, fightdata$TD.Def..x)
fightdata$TD.Def..y <- ifelse(fightdata$TD.Def..y == 0, meanTD.Def..y, fightdata$TD.Def..y)

meanSub..Avg..x <- mean(fightdata[fightdata$Sub..Avg..x != 0,]$Sub..Avg..x)
meanSub..Avg..y <- mean(fightdata[fightdata$Sub..Avg..y != 0,]$Sub..Avg..y)
fightdata$Sub..Avg..x <- ifelse(fightdata$Sub..Avg..x == 0, meanSub..Avg..x, fightdata$Sub..Avg..x)
fightdata$Sub..Avg..y <- ifelse(fightdata$Sub..Avg..y == 0, meanSub..Avg..y, fightdata$Sub..Avg..y)

colMeans(fightdata == 0)
colMeans(is.na(fightdata))

fightdata = fightdata[complete.cases(fightdata),]

levels(fightdata$y) <- c(0,1)
fightdata$y = as.numeric(fightdata$y)
fightdata <- as.data.frame(lapply(fightdata, normalize))


```

For the fighting stats, I decided to use the mean to replace stats with 0, so I will not miss any valuable information.
Thus far, there are still a few rows containing n/a values for several variables. For example, if there is no Height.x and Weight.x, I could not predict the corresponding Reach.x. If I were to use mean value to fill up the all these missing value, greataer errors will be generated for the predictions, because they are all means(values made up). Thus, I used function `complete cases` to remove reamining rows with n/a.
Although rows decreased from 6230 to 5522, all the remaining rows are more significant in predicting the results.

## Data Randomization and creating both train and test dataset
```{r}
data_rand <- fightdata[sample(nrow(fightdata)),]
train <- data_rand[1:ceiling(.8*nrow(fightdata)),]
test <- data_rand[(ceiling(.8*nrow(fightdata)+1)) : nrow(fightdata),]
nrow(train)
nrow(test)
```


# Q1 and Q2


## Model 1 - Logitic Regression Model and its improved version 
```{r}

library(caret)
library(e1071)

logit.model1 <- glm(y~ ., data = train,family = "binomial")

summary(logit.model1)

log.predictions1 <- predict(logit.model1, newdata = test, type = "response")
log.predictions1 <- ifelse(log.predictions1 >= 0.5, 1, 0)
log.predictions1 <- as.factor(log.predictions1)
test$y <- as.factor(test$y)
levels(test$y) <- c("loss", "win")
levels(log.predictions1) <- c("loss", "win")
confusionMatrix(test$y, log.predictions1)

## Using the step function to find the significant factors 
step(logit.model1)

## Build the second logistic regression model with the factors identified by the Step Function

logit.model2 <- glm(formula = y ~ SLpM.x + SApM.x + Str..Def..x + TD.Avg..x + 
    TD.Acc..x + TD.Def..x + Reach.y + SLpM.y + Str..Acc..y + 
    SApM.y + Str..Def..y + TD.Avg..y + TD.Acc..y + TD.Def..y + 
    Sub..Avg..y + age.x + age.y + Stance.x_Orthodox + Stance.x_Other + 
    Stance.y_Orthodox + Stance.y_Other, family = "binomial", 
    data = train)
log.predictions2 <- predict(logit.model2, newdata = test, type = "response")
summary(log.predictions2)


log.predictions2 <- ifelse(log.predictions2 >= 0.5, 1, 0)
log.predictions2 <- as.factor(log.predictions2)
levels(log.predictions2) 
levels(log.predictions2) <- c("loss", "win")

confusionMatrix(test$y, log.predictions2)

```


## Model 2 - SVM Model and its improved version 
```{r}
library(kernlab)

svm_train <- as.data.frame(data_rand[1:ceiling(.8*nrow(data_rand)),])
svm_test <- as.data.frame(data_rand[ceiling(.8*nrow(data_rand)+1) : nrow(data_rand), ])

svm_test$y <- as.factor(svm_test$y)
levels(svm_test$y) <- c("loss", "win")


svm.model1 <- ksvm(y ~ SLpM.x + SApM.x + Str..Def..x + TD.Avg..x + 
    TD.Acc..x + TD.Def..x + Reach.y + SLpM.y + Str..Acc..y + 
    SApM.y + Str..Def..y + TD.Avg..y + TD.Acc..y + TD.Def..y + 
    Sub..Avg..y + age.x + age.y + Stance.x_Orthodox + Stance.x_Other + 
    Stance.y_Orthodox + Stance.y_Other,data = svm_train, kernel = "polydot")
                   

levels(svm_test$y) <- c("loss", "win")

svm.predictions1 <- predict(svm.model1, svm_test)
svm.predictions1 <- ifelse(svm.predictions1>=0.5,1,0)

svm.predictions1 <- as.factor(svm.predictions1)
levels(svm.predictions1) <- c("loss", "win")
agreement <- svm.predictions1 == svm_test$y
table(agreement)

#View(test)
prop.table(table(agreement))

svm_test$y = as.factor(svm_test$y)
confusionMatrix(svm_test$y, svm.predictions1)


## Improved SVM Model 

svm.model2 <- ksvm(y ~ Height.x + Reach.x + SLpM.x + SApM.x + Str..Def..x + 
    TD.Avg..x + TD.Acc..x + TD.Def..x + Height.y + Weight.y + 
    Reach.y + SLpM.y + Str..Acc..y + SApM.y + Str..Def..y + TD.Avg..y + 
    TD.Acc..y + TD.Def..y + Sub..Avg..y + age.x + age.y + Stance.x_Orthodox + 
    Stance.x_Other + Stance.y_Other, data = svm_train,
    kernel = "rbfdot")

svm.predictions2 <- predict(svm.model2, svm_test)
svm.predictions2 <- ifelse(svm.predictions2>=0.5,1,0)
svm.predictions2 <- as.factor(svm.predictions2)
levels(svm.predictions2) <- c("loss", "win")
confusionMatrix(svm_test$y, svm.predictions2)

```

## Model 3 - Decision Tree Model and its improved version 
```{r}

set.seed(1002)

fightdata_DT <- data_rand
train_DT <- fightdata_DT[1:ceiling(.8*nrow(fightdata_DT)),]
test_DT <- fightdata_DT[(ceiling(.8*nrow(fightdata_DT)+1)) : nrow(fightdata_DT),]


# check the proportion of class variable
prop.table(table(train_DT$default))
prop.table(table(test_DT$default))

## Step 3: Training a model on the data ----
# build the simplest decision tree

test_DT$y = as.factor(test_DT$y)
train_DT$y = as.factor(train_DT$y)
library(C50)

DT.model1 <- C5.0(train_DT[-31], train_DT$y)
predicted_DT = predict(DT.model1, test_DT)
levels(predicted_DT) = c("loss", "win")
test_DT$y = as.factor(test_DT$y)
levels(test_DT$y) = c("loss", "win")
confusionMatrix(predicted_DT, test_DT$y)


## Improved Decision Tree Model 

DT.model2 <- C5.0(train_DT[-31], train_DT$y, trials = 17)
predicted_DT2 = predict(DT.model2, test_DT)
levels(predicted_DT2) = c("loss", "win")
test_DT$y = as.factor(test_DT$y)
levels(test_DT$y) = c("loss", "win")
confusionMatrix(predicted_DT2, test_DT$y)

```


# Q3

#Combined predictions of the three models 
```{r}

levels(log.predictions2 ) = c(0,1)
levels(svm.predictions2) = c(0,1)
levels(predicted_DT2) = c(0,1)


log.predictions2  <- as.numeric(log.predictions2 )-1
svm.predictions2 <- as.numeric(svm.predictions2)-1
predicted_DT2 <- as.numeric(predicted_DT2)-1


combined <- log.predictions2 + svm.predictions2 + predicted_DT2
combined_prediction <- ifelse(combined >= 2, 1, 0)
combined_prediction <- as.factor(combined_prediction)

levels(combined_prediction) = c("loss", "win")
confusionMatrix(combined_prediction, test$y)

```

The three models selected for the combined results are the three improved models. They are logitic model 2, svm model 2, and decision tree model 2. Since there are three models that we selected for the combined final prediction, the majority voting scheme is applicable. The combined result has a range of 0 - 3, so I set the majority voting at 2. If two or more models predict win, then the combined result is win. On the other hand, if fewer than two models predict win, then the combined result is loss. 

# Q4 

According to the confusion matrix for the combined final prediction, the Kappa value is 0.3889. 

# Q5 Conclusion 

Predicting UFC fight results is actually hard. The best predicting model so far has an accuracy around 70%. The models I have built can indeed help me make decisions on fights betting. SLpM, SApM,  Str..Def, Reach, TD.Acc, and age are most significant when predicting the result of the fight. By simply applying the statistics and personal information of the fighters, we can predict the result of the fight. There are indeed challenges and risks when using the model to make decisions. First of all, these statistics are all career statistics, we can harldly trace the recent performance of the players. The fighters could have a fantastic early careers, but suffer from near-term low ebb. Secondly, the age of the players are not accurate beacause we don't know when the game was played. Age has a significant impact on the result of the game. If we can get the age of the player when the fight occured, it will be more significant in predicting the result. In order to improve the relaibtility and value of the model, it should consider the fact of the recent performance and the exact age when fighter was fighting. 



                   
