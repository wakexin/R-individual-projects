# Individual Project

#### Kexin Wang

#### 12/1/2019

# Initialization

    
    
    normalize <- function(x) { 
      return((x - min(x)) / (max(x) - min(x)))
    }

#0. Clean Data ##First of all, I took a look at the data. There are several
variables that are not numerical variables. 

##0.1 For DOB, I turned Date of
birth (DOB.x, DOB.y) into age to get valuable insight. To do that, I
subtracted the year from date of birth which is in day/month/year formate and
divided the number of days to get number of years, which is age. 

##0.2 For Stance, I created a new factor “other” to replace player’s stance with N/A. A
few rows still have n/a values, but predicting those values such as weight and
height will create greater errors for the predictions. Therefore, I used
function complete.cases to remove reamining rows with NA. 

##0.3 For NA values in Reach, I built a model, using height and weight to predict the reach of
missing values in reach.

    
    
    fightdata <- read.csv("FightData.csv")
    str(fightdata)
    
    
    ## 'data.frame':    6230 obs. of  27 variables:
    ##  $ result     : Factor w/ 2 levels "loss","win": 2 2 2 1 1 2 2 1 2 1 ...
    ##  $ Height.x   : int  72 72 71 70 66 69 68 71 74 72 ...
    ##  $ Weight.x   : int  170 155 155 265 145 145 155 185 185 185 ...
    ##  $ Reach.x    : int  72 74 72 72 70 68 66 74 75 NA ...
    ##  $ Stance.x   : Factor w/ 5 levels "Open Stance",..: 2 2 5 2 NA 2 2 4 4 NA ...
    ##  $ DOB.x      : Factor w/ 1342 levels "1/1/1968","1/10/1981",..: 1021 1007 913 574 469 1208 1261 934 841 NA ...
    ##  $ SLpM.x     : num  2.39 2.57 2.03 3.03 1.75 0.65 2.39 2.8 3.23 0 ...
    ##  $ Str..Acc..x: int  45 36 47 44 39 52 38 55 46 0 ...
    ##  $ SApM.x     : num  2.8 2.57 4.07 2.83 1.78 0.95 2.37 3.15 3.27 0 ...
    ##  $ Str..Def..x: int  65 50 35 51 48 53 57 48 55 0 ...
    ##  $ TD.Avg..x  : num  1.13 3.08 0.43 0.57 2.02 0 2.64 3.47 0 0 ...
    ##  $ TD.Acc..x  : int  30 37 9 55 22 0 42 57 0 0 ...
    ##  $ TD.Def..x  : int  75 55 57 68 56 0 86 50 82 0 ...
    ##  $ Sub..Avg..x: num  0.4 1.1 1.7 0.2 0.5 3.4 1.1 1.3 0.1 0 ...
    ##  $ Height.y   : int  68 NA NA 79 72 70 70 73 73 75 ...
    ##  $ Weight.y   : int  170 155 155 265 155 155 155 185 185 185 ...
    ##  $ Reach.y    : int  70 NA NA 81 77 NA NA 73 73 77 ...
    ##  $ Stance.y   : Factor w/ 5 levels "Open Stance",..: 2 NA NA 2 NA 2 2 5 5 4 ...
    ##  $ DOB.y      : Factor w/ 1363 levels "1/1/1968","1/1/1976",..: 302 662 662 1017 502 701 701 231 231 120 ...
    ##  $ SLpM.y     : num  2.33 2.33 2.33 0.85 2.35 0.79 0.79 2.21 2.21 4.34 ...
    ##  $ Str..Acc..y: int  28 48 48 44 42 30 30 50 50 49 ...
    ##  $ SApM.y     : num  2.6 3.93 3.93 2.48 3.46 1.36 1.36 2.72 2.72 2.17 ...
    ##  $ Str..Def..y: int  58 55 55 52 47 41 41 46 46 59 ...
    ##  $ TD.Avg..y  : num  0 2 2 2.12 2.68 5.09 5.09 2.83 2.83 0.78 ...
    ##  $ TD.Acc..y  : int  0 28 28 40 42 50 50 50 50 40 ...
    ##  $ TD.Def..y  : int  0 63 63 33 76 0 0 57 57 68 ...
    ##  $ Sub..Avg..y: num  0 0 0 4.2 0.3 0 0 0.4 0.4 1.3 ...
    
    
    fightdata$DOB.x = as.character(fightdata$DOB.x)
    fightdata$DOB.x = as.Date(fightdata$DOB.x, format = "%m/%d/%Y")
    head(fightdata$DOB.x)
    
    
    ## [1] "1982-07-23" "1983-07-20" "1981-06-25" "1974-03-23" "1982-02-24"
    ## [6] "1974-08-08"
    
    
    fightdata$age.x = as.numeric(Sys.Date() - fightdata$DOB.x)
    fightdata$age.x <- fightdata$age.x/365
    
    fightdata$DOB.y = as.character(fightdata$DOB.y)
    fightdata$DOB.y = as.Date(fightdata$DOB.y, format = "%m/%d/%Y")
    head(fightdata$DOB.y)
    
    
    ## [1] "1986-11-05" "1983-04-11" "1983-04-11" "1976-07-17" "1991-02-28"
    ## [6] "1980-04-21"
    
    
    fightdata$age.y = as.numeric(Sys.Date() - fightdata$DOB.y)
    fightdata$age.y <- fightdata$age.y/365
    
    
    fightdata$DOB.x <- NULL
    fightdata$DOB.y <- NULL
    
    summary(fightdata$Stance.x)
    
    
    ## Open Stance    Orthodox    Sideways    Southpaw      Switch        NA's 
    ##          11        4457           3        1194         153         412
    
    
    str(fightdata$Stance.x)
    
    
    ##  Factor w/ 5 levels "Open Stance",..: 2 2 5 2 NA 2 2 4 4 NA ...
    
    
    fightdata$Stance.x = as.character(fightdata$Stance.x)
    fightdata[fightdata$Stance.x == "Open Stance"|fightdata$Stance.x == "Sideways"|fightdata$Stance.x == "Switch"|is.na(fightdata$Stance.x),]$Stance.x <- "Other"
    fightdata = fastDummies::dummy_columns(fightdata, select_columns = "Stance.x")
    
    fightdata$Stance.y = as.character(fightdata$Stance.y)
    fightdata[fightdata$Stance.y == "Open Stance"|fightdata$Stance.y == "Sideways"|fightdata$Stance.y == "Switch"|is.na(fightdata$Stance.y),]$Stance.y <- "Other"
    fightdata = fastDummies::dummy_columns(fightdata, select_columns = "Stance.y")
    
    
    fightdata$Stance.x<-NULL
    fightdata$Stance.y<-NULL
    
    fightdata$y<- fightdata$result
    fightdata$result <- NULL
    
    fightdata2 <- fightdata[complete.cases(fightdata),]
    model1 <- lm(Reach.x ~ Height.x + Weight.x, data = fightdata2)
    summary(model1)
    
    
    ## 
    ## Call:
    ## lm(formula = Reach.x ~ Height.x + Weight.x, data = fightdata2)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.6635 -1.3050  0.0083  1.2177  6.3365 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 3.454318   0.794085    4.35 1.39e-05 ***
    ## Height.x    0.940564   0.013625   69.03  < 2e-16 ***
    ## Weight.x    0.014905   0.001365   10.92  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.783 on 4002 degrees of freedom
    ## Multiple R-squared:  0.7919, Adjusted R-squared:  0.7918 
    ## F-statistic:  7614 on 2 and 4002 DF,  p-value: < 2.2e-16
    
    
    predicted_Reach_X = predict(model1, newdata = fightdata)
    Reach_X_comp = data.frame(predicted_Reach_X, fightdata$Reach.x)
    
    
    fightdata$Reach.x = ifelse(is.na(fightdata$Reach.x), predicted_Reach_X, fightdata$Reach.x)
    
    
    model2 = lm(Reach.y ~ Height.y + Weight.y, data = fightdata2)
    summary(model2)
    
    
    ## 
    ## Call:
    ## lm(formula = Reach.y ~ Height.y + Weight.y, data = fightdata2)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.4038 -1.2845 -0.1567  1.2342  6.4008 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  5.80861    0.82667   7.027 2.48e-12 ***
    ## Height.y     0.89510    0.01432  62.501  < 2e-16 ***
    ## Weight.y     0.01954    0.00144  13.572  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.806 on 4002 degrees of freedom
    ## Multiple R-squared:  0.7946, Adjusted R-squared:  0.7945 
    ## F-statistic:  7741 on 2 and 4002 DF,  p-value: < 2.2e-16
    
    
    predicted_Reach_Y = predict(model2, newdata = fightdata)
    Reach_Y_comp = data.frame(predicted_Reach_Y, fightdata$Reach.y)
    fightdata$Reach.y = ifelse(is.na(fightdata$Reach.y), predicted_Reach_Y, fightdata$Reach.y)
    
    colMeans(is.na(fightdata))
    
    
    ##          Height.x          Weight.x           Reach.x            SLpM.x 
    ##       0.020545746       0.005939005       0.020385233       0.000000000 
    ##       Str..Acc..x            SApM.x       Str..Def..x         TD.Avg..x 
    ##       0.000000000       0.000000000       0.000000000       0.000000000 
    ##         TD.Acc..x         TD.Def..x       Sub..Avg..x          Height.y 
    ##       0.000000000       0.000000000       0.000000000       0.019422151 
    ##          Weight.y           Reach.y            SLpM.y       Str..Acc..y 
    ##       0.005457464       0.019903692       0.000000000       0.000000000 
    ##            SApM.y       Str..Def..y         TD.Avg..y         TD.Acc..y 
    ##       0.000000000       0.000000000       0.000000000       0.000000000 
    ##         TD.Def..y       Sub..Avg..y             age.x             age.y 
    ##       0.000000000       0.000000000       0.068539326       0.064044944 
    ## Stance.x_Orthodox    Stance.x_Other Stance.x_Southpaw Stance.y_Orthodox 
    ##       0.000000000       0.000000000       0.000000000       0.000000000 
    ##    Stance.y_Other Stance.y_Southpaw                 y 
    ##       0.000000000       0.000000000       0.000000000
    
    
    str(fightdata)
    
    
    ## 'data.frame':    6230 obs. of  31 variables:
    ##  $ Height.x         : int  72 72 71 70 66 69 68 71 74 72 ...
    ##  $ Weight.x         : int  170 155 155 265 145 145 155 185 185 185 ...
    ##  $ Reach.x          : num  72 74 72 72 70 ...
    ##  $ SLpM.x           : num  2.39 2.57 2.03 3.03 1.75 0.65 2.39 2.8 3.23 0 ...
    ##  $ Str..Acc..x      : int  45 36 47 44 39 52 38 55 46 0 ...
    ##  $ SApM.x           : num  2.8 2.57 4.07 2.83 1.78 0.95 2.37 3.15 3.27 0 ...
    ##  $ Str..Def..x      : int  65 50 35 51 48 53 57 48 55 0 ...
    ##  $ TD.Avg..x        : num  1.13 3.08 0.43 0.57 2.02 0 2.64 3.47 0 0 ...
    ##  $ TD.Acc..x        : int  30 37 9 55 22 0 42 57 0 0 ...
    ##  $ TD.Def..x        : int  75 55 57 68 56 0 86 50 82 0 ...
    ##  $ Sub..Avg..x      : num  0.4 1.1 1.7 0.2 0.5 3.4 1.1 1.3 0.1 0 ...
    ##  $ Height.y         : int  68 NA NA 79 72 70 70 73 73 75 ...
    ##  $ Weight.y         : int  170 155 155 265 155 155 155 185 185 185 ...
    ##  $ Reach.y          : num  70 NA NA 81 77 ...
    ##  $ SLpM.y           : num  2.33 2.33 2.33 0.85 2.35 0.79 0.79 2.21 2.21 4.34 ...
    ##  $ Str..Acc..y      : int  28 48 48 44 42 30 30 50 50 49 ...
    ##  $ SApM.y           : num  2.6 3.93 3.93 2.48 3.46 1.36 1.36 2.72 2.72 2.17 ...
    ##  $ Str..Def..y      : int  58 55 55 52 47 41 41 46 46 59 ...
    ##  $ TD.Avg..y        : num  0 2 2 2.12 2.68 5.09 5.09 2.83 2.83 0.78 ...
    ##  $ TD.Acc..y        : int  0 28 28 40 42 50 50 50 50 40 ...
    ##  $ TD.Def..y        : int  0 63 63 33 76 0 0 57 57 68 ...
    ##  $ Sub..Avg..y      : num  0 0 0 4.2 0.3 0 0 0.4 0.4 1.3 ...
    ##  $ age.x            : num  37.4 36.4 38.5 45.7 37.8 ...
    ##  $ age.y            : num  33.1 36.7 36.7 43.4 28.8 ...
    ##  $ Stance.x_Orthodox: int  1 1 0 1 0 1 1 0 0 0 ...
    ##  $ Stance.x_Other   : int  0 0 1 0 1 0 0 0 0 1 ...
    ##  $ Stance.x_Southpaw: int  0 0 0 0 0 0 0 1 1 0 ...
    ##  $ Stance.y_Orthodox: int  1 0 0 1 0 1 1 0 0 0 ...
    ##  $ Stance.y_Other   : int  0 1 1 0 1 0 0 1 1 0 ...
    ##  $ Stance.y_Southpaw: int  0 0 0 0 0 0 0 0 0 1 ...
    ##  $ y                : Factor w/ 2 levels "loss","win": 2 2 2 1 1 2 2 1 2 1 ...

##0.4 Now, let’s take a look at numerical and int variables. In this part, I
handled the ‘0’ value in fightdata by replacing them with the mean value.For
rows that has too many NA, I used as.complete function to remove those rows to
ensure accuracy of my model later on.

    
    
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
    
    
    ##          Height.x          Weight.x           Reach.x            SLpM.x 
    ##                NA                NA                NA         0.0000000 
    ##       Str..Acc..x            SApM.x       Str..Def..x         TD.Avg..x 
    ##         0.0000000         0.0000000         0.0000000         0.0000000 
    ##         TD.Acc..x         TD.Def..x       Sub..Avg..x          Height.y 
    ##         0.0000000         0.0000000         0.0000000                NA 
    ##          Weight.y           Reach.y            SLpM.y       Str..Acc..y 
    ##                NA                NA         0.0000000         0.0000000 
    ##            SApM.y       Str..Def..y         TD.Avg..y         TD.Acc..y 
    ##         0.0000000         0.0000000         0.0000000         0.0000000 
    ##         TD.Def..y       Sub..Avg..y             age.x             age.y 
    ##         0.0000000         0.0000000                NA                NA 
    ## Stance.x_Orthodox    Stance.x_Other Stance.x_Southpaw Stance.y_Orthodox 
    ##         0.2845907         0.9070626         0.8083467         0.2701445 
    ##    Stance.y_Other Stance.y_Southpaw                 y 
    ##         0.9146067         0.8152488         0.0000000
    
    
    colMeans(is.na(fightdata))
    
    
    ##          Height.x          Weight.x           Reach.x            SLpM.x 
    ##       0.020545746       0.005939005       0.020385233       0.000000000 
    ##       Str..Acc..x            SApM.x       Str..Def..x         TD.Avg..x 
    ##       0.000000000       0.000000000       0.000000000       0.000000000 
    ##         TD.Acc..x         TD.Def..x       Sub..Avg..x          Height.y 
    ##       0.000000000       0.000000000       0.000000000       0.019422151 
    ##          Weight.y           Reach.y            SLpM.y       Str..Acc..y 
    ##       0.005457464       0.019903692       0.000000000       0.000000000 
    ##            SApM.y       Str..Def..y         TD.Avg..y         TD.Acc..y 
    ##       0.000000000       0.000000000       0.000000000       0.000000000 
    ##         TD.Def..y       Sub..Avg..y             age.x             age.y 
    ##       0.000000000       0.000000000       0.068539326       0.064044944 
    ## Stance.x_Orthodox    Stance.x_Other Stance.x_Southpaw Stance.y_Orthodox 
    ##       0.000000000       0.000000000       0.000000000       0.000000000 
    ##    Stance.y_Other Stance.y_Southpaw                 y 
    ##       0.000000000       0.000000000       0.000000000
    
    
    fightdata = fightdata[complete.cases(fightdata),]
    
    levels(fightdata$y) <- c(0,1)
    fightdata$y = as.numeric(fightdata$y)
    fightdata <- as.data.frame(lapply(fightdata, normalize))

#1.Build three models, I chosed three models: logistic model, knn models and
SVM to compare the result of predictions. ##First, we randomize the data,
splitting 80% of the data as train data and 20% of the data as test data.

    
    
    data_rand <- fightdata[sample(nrow(fightdata)),]
    train <- data_rand[1:ceiling(.8*nrow(fightdata)),]
    test <- data_rand[(ceiling(.8*nrow(fightdata)+1)) : nrow(fightdata),]
    nrow(train)
    
    
    ## [1] 4418
    
    
    nrow(test)
    
    
    ## [1] 1104

## Logistic Model

    
    
    ## 1.1 Frist I used all the variables and used logistic model to predict.
    logit.model1<- glm(y~ ., data = train,family = "binomial")
    summary(logit.model1)
    
    
    ## 
    ## Call:
    ## glm(formula = y ~ ., family = "binomial", data = train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.6248  -1.0345   0.3553   1.0101   2.5368  
    ## 
    ## Coefficients: (2 not defined because of singularities)
    ##                   Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)        1.24274    0.65166   1.907 0.056515 .  
    ## Height.x          -0.79960    0.71098  -1.125 0.260736    
    ## Weight.x          -0.85420    0.61897  -1.380 0.167575    
    ## Reach.x            1.11922    0.68001   1.646 0.099787 .  
    ## SLpM.x             4.43888    0.50909   8.719  < 2e-16 ***
    ## Str..Acc..x       -0.29988    0.46385  -0.646 0.517963    
    ## SApM.x            -6.14014    0.68544  -8.958  < 2e-16 ***
    ## Str..Def..x        1.34045    0.47705   2.810 0.004956 ** 
    ## TD.Avg..x          1.20501    0.43874   2.747 0.006023 ** 
    ## TD.Acc..x         -0.43502    0.19891  -2.187 0.028745 *  
    ## TD.Def..x          0.77680    0.19243   4.037 5.42e-05 ***
    ## Sub..Avg..x       -0.57525    0.70411  -0.817 0.413934    
    ## Height.y           0.51632    0.71612   0.721 0.470910    
    ## Weight.y           3.20272    1.51323   2.116 0.034304 *  
    ## Reach.y           -1.57151    0.69081  -2.275 0.022913 *  
    ## SLpM.y            -9.79443    1.21208  -8.081 6.44e-16 ***
    ## Str..Acc..y       -0.89058    0.46486  -1.916 0.055390 .  
    ## SApM.y             7.46984    1.02421   7.293 3.03e-13 ***
    ## Str..Def..y       -2.00886    0.50073  -4.012 6.02e-05 ***
    ## TD.Avg..y         -0.46004    0.36808  -1.250 0.211369    
    ## TD.Acc..y          0.54569    0.18897   2.888 0.003881 ** 
    ## TD.Def..y         -0.67129    0.20078  -3.344 0.000827 ***
    ## Sub..Avg..y       -1.38645    0.62393  -2.222 0.026275 *  
    ## age.x             -2.16191    0.34428  -6.280 3.40e-10 ***
    ## age.y              2.56345    0.42511   6.030 1.64e-09 ***
    ## Stance.x_Orthodox -0.22120    0.08373  -2.642 0.008245 ** 
    ## Stance.x_Other    -0.57316    0.16383  -3.498 0.000468 ***
    ## Stance.x_Southpaw       NA         NA      NA       NA    
    ## Stance.y_Orthodox  0.15838    0.08476   1.869 0.061673 .  
    ## Stance.y_Other     0.49342    0.16397   3.009 0.002619 ** 
    ## Stance.y_Southpaw       NA         NA      NA       NA    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 6122.8  on 4417  degrees of freedom
    ## Residual deviance: 5278.7  on 4389  degrees of freedom
    ## AIC: 5336.7
    ## 
    ## Number of Fisher Scoring iterations: 4
    
    
    ## 1.2 I improved logistic model by using step function to find significant variables and plugged them back in the logisitic model, which results in a higher accuracy rate  and higher kappa value
    
    'step(logit.model1)'
    
    
    ## [1] "step(logit.model1)"
    
    
    logit.model2 <- glm(y ~ Height.x + Reach.x + SLpM.x + SApM.x + Str..Def..x + 
        TD.Avg..x + TD.Acc..x + TD.Def..x + Height.y + Weight.y + 
        Reach.y + SLpM.y + Str..Acc..y + SApM.y + Str..Def..y + TD.Avg..y + 
        TD.Acc..y + TD.Def..y + Sub..Avg..y + age.x + age.y + Stance.x_Orthodox + 
        Stance.x_Other + Stance.y_Other, family = "binomial", data = train)
    summary(logit.model2)
    
    
    ## 
    ## Call:
    ## glm(formula = y ~ Height.x + Reach.x + SLpM.x + SApM.x + Str..Def..x + 
    ##     TD.Avg..x + TD.Acc..x + TD.Def..x + Height.y + Weight.y + 
    ##     Reach.y + SLpM.y + Str..Acc..y + SApM.y + Str..Def..y + TD.Avg..y + 
    ##     TD.Acc..y + TD.Def..y + Sub..Avg..y + age.x + age.y + Stance.x_Orthodox + 
    ##     Stance.x_Other + Stance.y_Other, family = "binomial", data = train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.6317  -1.0401   0.3642   1.0134   2.4407  
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)        1.18008    0.56916   2.073 0.038140 *  
    ## Height.x          -1.02820    0.69792  -1.473 0.140687    
    ## Reach.x            0.97907    0.67182   1.457 0.145023    
    ## SLpM.x             4.32036    0.45032   9.594  < 2e-16 ***
    ## SApM.x            -6.00960    0.62951  -9.546  < 2e-16 ***
    ## Str..Def..x        1.52799    0.44250   3.453 0.000554 ***
    ## TD.Avg..x          1.09313    0.43127   2.535 0.011254 *  
    ## TD.Acc..x         -0.46238    0.19756  -2.341 0.019257 *  
    ## TD.Def..x          0.77176    0.18882   4.087 4.37e-05 ***
    ## Height.y           0.48560    0.71289   0.681 0.495761    
    ## Weight.y           2.11063    1.18057   1.788 0.073807 .  
    ## Reach.y           -1.54027    0.69002  -2.232 0.025601 *  
    ## SLpM.y            -9.69738    1.20443  -8.051 8.18e-16 ***
    ## Str..Acc..y       -0.91023    0.46351  -1.964 0.049558 *  
    ## SApM.y             7.65619    1.02318   7.483 7.28e-14 ***
    ## Str..Def..y       -1.93299    0.49136  -3.934 8.36e-05 ***
    ## TD.Avg..y         -0.45088    0.36785  -1.226 0.220308    
    ## TD.Acc..y          0.54986    0.18864   2.915 0.003559 ** 
    ## TD.Def..y         -0.67915    0.20009  -3.394 0.000688 ***
    ## Sub..Avg..y       -1.31645    0.62220  -2.116 0.034360 *  
    ## age.x             -2.21799    0.33884  -6.546 5.91e-11 ***
    ## age.y              2.52222    0.42427   5.945 2.77e-09 ***
    ## Stance.x_Orthodox -0.21437    0.08341  -2.570 0.010162 *  
    ## Stance.x_Other    -0.57209    0.16368  -3.495 0.000474 ***
    ## Stance.y_Other     0.37299    0.14876   2.507 0.012166 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 6122.8  on 4417  degrees of freedom
    ## Residual deviance: 5285.3  on 4393  degrees of freedom
    ## AIC: 5335.3
    ## 
    ## Number of Fisher Scoring iterations: 4
    
    
    log.predictions1 <- predict(logit.model1, newdata = test, type = "response")
    
    
    ## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type
    ## == : prediction from a rank-deficient fit may be misleading
    
    
    summary(log.predictions1)
    
    
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## 0.02125 0.36741 0.52452 0.51495 0.66108 0.99673
    
    
    log.predictions2 <- predict(logit.model2, newdata = test, type = "response")
    summary(log.predictions2)
    
    
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## 0.02014 0.36499 0.52555 0.51513 0.66295 0.99695
    
    
    library(caret)
    
    
    ## Loading required package: lattice
    
    
    ## Loading required package: ggplot2
    
    
    library(e1071)
    
    log.predictions1 <- ifelse(log.predictions1 >= 0.5, 1, 0)
    log.predictions1 <- as.factor(log.predictions1)
    levels(log.predictions1) 
    
    
    ## [1] "0" "1"
    
    
    test$y <- as.factor(test$y)
    levels(test$y) <- c("loss", "win")
    levels(log.predictions1) <- c("loss", "win")
    
    confusionMatrix(test$y, log.predictions1)
    
    
    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction loss win
    ##       loss  327 200
    ##       win   168 409
    ##                                          
    ##                Accuracy : 0.6667         
    ##                  95% CI : (0.638, 0.6945)
    ##     No Information Rate : 0.5516         
    ##     P-Value [Acc > NIR] : 4.512e-15      
    ##                                          
    ##                   Kappa : 0.3302         
    ##                                          
    ##  Mcnemar's Test P-Value : 0.1061         
    ##                                          
    ##             Sensitivity : 0.6606         
    ##             Specificity : 0.6716         
    ##          Pos Pred Value : 0.6205         
    ##          Neg Pred Value : 0.7088         
    ##              Prevalence : 0.4484         
    ##          Detection Rate : 0.2962         
    ##    Detection Prevalence : 0.4774         
    ##       Balanced Accuracy : 0.6661         
    ##                                          
    ##        'Positive' Class : loss           
    ## 
    
    
    log.predictions2 <- ifelse(log.predictions2 >= 0.5, 1, 0)
    log.predictions2 <- as.factor(log.predictions2)
    levels(log.predictions2) 
    
    
    ## [1] "0" "1"
    
    
    levels(log.predictions2) <- c("loss", "win")
    
    confusionMatrix(test$y, log.predictions2)
    
    
    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction loss win
    ##       loss  330 197
    ##       win   170 407
    ##                                           
    ##                Accuracy : 0.6676          
    ##                  95% CI : (0.6389, 0.6953)
    ##     No Information Rate : 0.5471          
    ##     P-Value [Acc > NIR] : 2.462e-16       
    ##                                           
    ##                   Kappa : 0.3323          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.1747          
    ##                                           
    ##             Sensitivity : 0.6600          
    ##             Specificity : 0.6738          
    ##          Pos Pred Value : 0.6262          
    ##          Neg Pred Value : 0.7054          
    ##              Prevalence : 0.4529          
    ##          Detection Rate : 0.2989          
    ##    Detection Prevalence : 0.4774          
    ##       Balanced Accuracy : 0.6669          
    ##                                           
    ##        'Positive' Class : loss            
    ## 

### Extract significant variables

    
    
    toselect.x = summary(logit.model1)$coeff[-1,4] < 0.05
    relevant.x <- names(toselect.x)[toselect.x == TRUE] 
    sig.formula <- as.formula(paste("y ~",paste(relevant.x, collapse= "+")))

##knn models

    
    
    ##First, we randomize the data, splitting 80% of the data as train data and 20% of the data as test data.
    knn_train <- as.data.frame(data_rand[1:ceiling(.8*nrow(data_rand)), -31], drop = FALSE)
    knn_test <- as.data.frame(data_rand[ceiling(.8*nrow(data_rand)+1) : nrow(data_rand), -31], drop = FALSE)
    train_labels <- data_rand[1:ceiling(.8*nrow(data_rand)),31]
    test_labels <- data_rand[ceiling(.8*nrow(data_rand)+1): nrow(data_rand),31]
    
    library(class)
    ##1.2 I use total observation^(1/2) to get a optimal k
    test_pred1 <- knn(train = knn_train, test = knn_test, cl = train_labels, k = 74)
    
    ##2.2 I futher improve the knn model by taking another k
    library(gmodels)
    CrossTable(x= test_labels, y = test_pred1, prop.chisq = FALSE)
    
    
    ## 
    ##  
    ##    Cell Contents
    ## |-------------------------|
    ## |                       N |
    ## |           N / Row Total |
    ## |           N / Col Total |
    ## |         N / Table Total |
    ## |-------------------------|
    ## 
    ##  
    ## Total Observations in Table:  1104 
    ## 
    ##  
    ##              | test_pred1 
    ##  test_labels |         0 |         1 | Row Total | 
    ## -------------|-----------|-----------|-----------|
    ##            0 |       280 |       247 |       527 | 
    ##              |     0.531 |     0.469 |     0.477 | 
    ##              |     0.607 |     0.384 |           | 
    ##              |     0.254 |     0.224 |           | 
    ## -------------|-----------|-----------|-----------|
    ##            1 |       181 |       396 |       577 | 
    ##              |     0.314 |     0.686 |     0.523 | 
    ##              |     0.393 |     0.616 |           | 
    ##              |     0.164 |     0.359 |           | 
    ## -------------|-----------|-----------|-----------|
    ## Column Total |       461 |       643 |      1104 | 
    ##              |     0.418 |     0.582 |           | 
    ## -------------|-----------|-----------|-----------|
    ## 
    ## 
    
    
    test_pred2 <- knn(train = knn_train, test = knn_test, cl = train_labels, k = 50)
    CrossTable(x= test_labels, y = test_pred2, prop.chisq = FALSE)
    
    
    ## 
    ##  
    ##    Cell Contents
    ## |-------------------------|
    ## |                       N |
    ## |           N / Row Total |
    ## |           N / Col Total |
    ## |         N / Table Total |
    ## |-------------------------|
    ## 
    ##  
    ## Total Observations in Table:  1104 
    ## 
    ##  
    ##              | test_pred2 
    ##  test_labels |         0 |         1 | Row Total | 
    ## -------------|-----------|-----------|-----------|
    ##            0 |       283 |       244 |       527 | 
    ##              |     0.537 |     0.463 |     0.477 | 
    ##              |     0.597 |     0.387 |           | 
    ##              |     0.256 |     0.221 |           | 
    ## -------------|-----------|-----------|-----------|
    ##            1 |       191 |       386 |       577 | 
    ##              |     0.331 |     0.669 |     0.523 | 
    ##              |     0.403 |     0.613 |           | 
    ##              |     0.173 |     0.350 |           | 
    ## -------------|-----------|-----------|-----------|
    ## Column Total |       474 |       630 |      1104 | 
    ##              |     0.429 |     0.571 |           | 
    ## -------------|-----------|-----------|-----------|
    ## 
    ## 

## svm

    
    
    library(kernlab)
    
    
    ## 
    ## Attaching package: 'kernlab'
    
    
    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     alpha
    
    
    ##First, we randomize the data, splitting 80% of the data as train data and 20% of the data as test data.
    svm_train <- as.data.frame(data_rand[1:ceiling(.8*nrow(data_rand)),])
    svm_test <- as.data.frame(data_rand[ceiling(.8*nrow(data_rand)+1) : nrow(data_rand), ])
    
    svm_test$y <- as.factor(svm_test$y)
    levels(svm_test$y) <- c("loss", "win")
    
    svm.model1 <- ksvm(y ~ Height.x + Reach.x + SLpM.x + SApM.x + Str..Def..x + 
        TD.Avg..x + TD.Acc..x + TD.Def..x + Height.y + Weight.y + 
        Reach.y + SLpM.y + Str..Acc..y + SApM.y + Str..Def..y + TD.Avg..y + 
        TD.Acc..y + TD.Def..y + Sub..Avg..y + age.x + age.y + Stance.x_Orthodox + 
        Stance.x_Other + Stance.y_Other, data = svm_train,
        kernel = "rbfdot")
    
    ## The following 3 other models I used to improve the performance of my model
    levels(svm_test$y) <- c("loss", "win")
    
    svm.predictions1 <- predict(svm.model1, svm_test)
    svm.predictions1 <- ifelse(svm.predictions1>=0.5,1,0)
    
    svm.predictions1 <- as.factor(svm.predictions1)
    levels(svm.predictions1) <- c("loss", "win")
    agreement <- svm.predictions1 == svm_test$y
    table(agreement)
    
    
    ## agreement
    ## FALSE  TRUE 
    ##   335   769
    
    
    #View(test)
    prop.table(table(agreement))
    
    
    ## agreement
    ##    FALSE     TRUE 
    ## 0.303442 0.696558
    
    
    svm_test$y = as.factor(svm_test$y)
    confusionMatrix(svm_test$y, svm.predictions1)
    
    
    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction loss win
    ##       loss  368 159
    ##       win   176 401
    ##                                           
    ##                Accuracy : 0.6966          
    ##                  95% CI : (0.6685, 0.7236)
    ##     No Information Rate : 0.5072          
    ##     P-Value [Acc > NIR] : <2e-16          
    ##                                           
    ##                   Kappa : 0.3927          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.382           
    ##                                           
    ##             Sensitivity : 0.6765          
    ##             Specificity : 0.7161          
    ##          Pos Pred Value : 0.6983          
    ##          Neg Pred Value : 0.6950          
    ##              Prevalence : 0.4928          
    ##          Detection Rate : 0.3333          
    ##    Detection Prevalence : 0.4774          
    ##       Balanced Accuracy : 0.6963          
    ##                                           
    ##        'Positive' Class : loss            
    ## 
    
    
    svm.model2 <- ksvm(y ~ Height.x + Reach.x + SLpM.x + SApM.x + Str..Def..x + 
        TD.Avg..x + TD.Acc..x + TD.Def..x + Height.y + Weight.y + 
        Reach.y + SLpM.y + Str..Acc..y + SApM.y + Str..Def..y + TD.Avg..y + 
        TD.Acc..y + TD.Def..y + Sub..Avg..y + age.x + age.y + Stance.x_Orthodox + 
        Stance.x_Other + Stance.y_Other, data = svm_train,
        kernel = "polydot")
    
    
    ##  Setting default kernel parameters
    
    
    svm.predictions2 <- predict(svm.model2, svm_test)
    svm.predictions2 <- ifelse(svm.predictions2>=0.5,1,0)
    svm.predictions2 <- as.factor(svm.predictions2)
    levels(svm.predictions2) <- c("loss", "win")
    confusionMatrix(svm_test$y, svm.predictions2)
    
    
    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction loss win
    ##       loss  320 207
    ##       win   141 436
    ##                                           
    ##                Accuracy : 0.6848          
    ##                  95% CI : (0.6565, 0.7121)
    ##     No Information Rate : 0.5824          
    ##     P-Value [Acc > NIR] : 1.567e-12       
    ##                                           
    ##                   Kappa : 0.3648          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.0004933       
    ##                                           
    ##             Sensitivity : 0.6941          
    ##             Specificity : 0.6781          
    ##          Pos Pred Value : 0.6072          
    ##          Neg Pred Value : 0.7556          
    ##              Prevalence : 0.4176          
    ##          Detection Rate : 0.2899          
    ##    Detection Prevalence : 0.4774          
    ##       Balanced Accuracy : 0.6861          
    ##                                           
    ##        'Positive' Class : loss            
    ## 
    
    
    svm.model3 <- ksvm(y ~ ., data = svm_train,
        kernel = "polydot")
    
    
    ##  Setting default kernel parameters
    
    
    svm.predictions3 <- predict(svm.model3, svm_test)
    svm.predictions3 <- ifelse(svm.predictions3>=0.5,1,0)
    svm.predictions3 <- as.factor(svm.predictions3)
    levels(svm.predictions3) <- c("loss", "win")
    confusionMatrix(svm_test$y, svm.predictions3)
    
    
    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction loss win
    ##       loss  315 212
    ##       win   151 426
    ##                                           
    ##                Accuracy : 0.6712          
    ##                  95% CI : (0.6426, 0.6989)
    ##     No Information Rate : 0.5779          
    ##     P-Value [Acc > NIR] : 1.249e-10       
    ##                                           
    ##                   Kappa : 0.3377          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.001637        
    ##                                           
    ##             Sensitivity : 0.6760          
    ##             Specificity : 0.6677          
    ##          Pos Pred Value : 0.5977          
    ##          Neg Pred Value : 0.7383          
    ##              Prevalence : 0.4221          
    ##          Detection Rate : 0.2853          
    ##    Detection Prevalence : 0.4774          
    ##       Balanced Accuracy : 0.6718          
    ##                                           
    ##        'Positive' Class : loss            
    ## 
    
    
    svm.model4 <- ksvm(sig.formula, data = svm_train,
        kernel = "rbfdot")
    svm.predictions4 <- predict(svm.model4, svm_test)
    svm.predictions4 <- ifelse(svm.predictions4 >=0.5,1,0)
    svm.predictions4 <- as.factor(svm.predictions4)
    levels(svm.predictions4) <- c("loss", "win")
    confusionMatrix(svm_test$y, svm.predictions4)
    
    
    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction loss win
    ##       loss  346 181
    ##       win   178 399
    ##                                           
    ##                Accuracy : 0.6748          
    ##                  95% CI : (0.6463, 0.7024)
    ##     No Information Rate : 0.5254          
    ##     P-Value [Acc > NIR] : <2e-16          
    ##                                           
    ##                   Kappa : 0.3481          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.9159          
    ##                                           
    ##             Sensitivity : 0.6603          
    ##             Specificity : 0.6879          
    ##          Pos Pred Value : 0.6565          
    ##          Neg Pred Value : 0.6915          
    ##              Prevalence : 0.4746          
    ##          Detection Rate : 0.3134          
    ##    Detection Prevalence : 0.4774          
    ##       Balanced Accuracy : 0.6741          
    ##                                           
    ##        'Positive' Class : loss            
    ## 

## As we can see, svm model 1 is the one with highest accuracy

## Final Result Combined

## Combine all the models with highest accuracy

    
    
    levels(svm.predictions1) = c(0,1)
    levels(test_pred2) = c(0,1)
    levels(log.predictions2) = c(0,1)
    
    svm.predictions1 <- as.numeric(svm.predictions1)-1
    log.predictions2 <- as.numeric(log.predictions2)-1
    test_pred2 <- as.numeric(test_pred2)-1
    
    combined <- log.predictions2 + svm.predictions1 + test_pred2
    combined_prediction <- ifelse(combined>=1.5, 1, 0)
    combined_prediction <- as.factor(combined_prediction)
    
    levels(combined_prediction) = c("loss", "win")
    confusionMatrix(combined_prediction, test$y)
    
    
    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction loss win
    ##       loss  351 166
    ##       win   176 411
    ##                                          
    ##                Accuracy : 0.6902         
    ##                  95% CI : (0.662, 0.7174)
    ##     No Information Rate : 0.5226         
    ##     P-Value [Acc > NIR] : <2e-16         
    ##                                          
    ##                   Kappa : 0.3787         
    ##                                          
    ##  Mcnemar's Test P-Value : 0.6265         
    ##                                          
    ##             Sensitivity : 0.6660         
    ##             Specificity : 0.7123         
    ##          Pos Pred Value : 0.6789         
    ##          Neg Pred Value : 0.7002         
    ##              Prevalence : 0.4774         
    ##          Detection Rate : 0.3179         
    ##    Detection Prevalence : 0.4683         
    ##       Balanced Accuracy : 0.6892         
    ##                                          
    ##        'Positive' Class : loss           
    ## 

## Kappa value:0, accuaracy: 0.4928, not as high as svm model.

# Insight

## The highest I got is from svm. I learn that data cleaning is the important
first step to start building all the models and using height and weight to
predict. I also learn that svm is the most accurate model to predict, and
combining all the models together does not neccessarily means I will get the
highest accuracy result.

