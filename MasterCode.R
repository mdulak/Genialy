
###packages ####
writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")

#need to increase memory limit of project due to high volume of data
memory.limit(size=15000)
Sys.which("make")
install.packages("mice")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("visdat")
install.packages("tidymodels")
install.packages("DataExplorer")
install.packages("corrr")
install.packages("glmnet")
install.packages("e1071")
install.packages("workflows")
install.packages("recipes")
install.packages("parsnip")
install.packages("Metrics")
library(ggplot2) 
library(tidyverse)
library(gbm)
library(dplyr) 
library(readxl)
library(stringr)
library(tidymodels)
library(mice)
library(visdat)
library(DataExplorer)
library(plyr)
library(stats)
library(caret)
library(corrr)
library(glmnet)
library(Metrics)
library(onehot)
library(e1071)
library(workflows)
library(recipes)
library(parsnip)
library(MASS)
library(randomForest)
set.seed(123)
#####reading in data ####
test<-read.csv("C:/Users/Morgan's Laptop/Desktop/Trinity/FALL 21/BAT3305 Machine Learning/Assignment1/test.csv")
train<-read.csv("C:/Users/Morgan's Laptop/Desktop/Trinity/FALL 21/BAT3305 Machine Learning/Assignment1/train.csv")

####visualizing missing data #####
vis_dat(test)
missingpercent <- plot_missing(test,missing_only = T)

#removing features with greater than 50% NA's
#removing features with majority (>90% of the same value (Street))
#removing calculated data 
# ex: total rooms above ground is just sum of rooms in other features
# ex: total bsmt SF is just sum of finished/unfinished SF

test <- test %>%  dplyr::select(-FireplaceQu,-Fence,-Alley,-MiscFeature,-PoolQC,-Utilities,-Street,-TotRmsAbvGrd,-TotalBsmtSF,-GrLivArea, -Condition2)
train <- train %>% dplyr::select(-FireplaceQu,-Fence,-Alley,-MiscFeature,-PoolQC,-Utilities,-Street,-TotRmsAbvGrd,-TotalBsmtSF,-GrLivArea, -Condition2)

#####factor fixing ######
train$MSSubClass <- factor(train$MSSubClass)
train$MSZoning <- as.factor(train$MSZoning)
train$LotShape <- as.factor(train$LotShape)
train$LandContour <- as.factor(train$LandContour)
train$RoofStyle <- as.factor(train$RoofStyle)
train$RoofMatl <- as.factor(train$RoofMatl)
train$Exterior1st <- as.factor(train$Exterior1st)
train$Condition1 <- as.factor(train$Condition1)
train$LotConfig <- as.factor(train$LotConfig)
train$LandSlope <- as.factor(train$LandSlope)
train$Neighborhood <- as.factor(train$Neighborhood)
train$BldgType <- as.factor(train$BldgType)
train$HouseStyle <- as.factor(train$HouseStyle)
train$OverallQual <- factor(train$OverallQual, levels = c(1,2,3,4,5,6,7,8,9,10))
train$OverallCond <- factor(train$OverallCond, levels = c(1,2,3,4,5,6,7,8,9,10))
train$Exterior2nd <- as.factor(train$Exterior2nd)
train$MasVnrType <- as.factor(train$MasVnrType)
train$ExterQual <- factor(train$ExterQual, levels = c("Po","Fa","Ta","Gd","Ex"))
train$ExterCond <- factor(train$ExterCond, levels = c("Po","Fa","Ta","Gd","Ex"))
train$Foundation <- as.factor(train$Foundation)
train$BsmtQual <- factor(train$BsmtQual, levels = c("None","Po","Fa","Ta","Gd","Ex"))
train$BsmtCond <- factor(train$BsmtCond, levels = c("None","Po","Fa","Ta","Gd","Ex"))
train$BsmtExposure <- factor(train$BsmtExposure, levels = c("None","No","Mn","Av","Gd"))
train$BsmtFinType1 <- factor(train$BsmtFinType1, levels = c("None","Unf","LwQ","Rec","BLQ","ALQ","GLQ"))
train$BsmtFinType2 <- factor(train$BsmtFinType2, levels = c("None","Unf","LwQ","Rec","BLQ","ALQ","GLQ"))
train$Heating <- as.factor(train$Heating)
train$HeatingQC <- factor(train$HeatingQC, levels = c("Po","Fa","Ta","Gd","Ex"))
train$CentralAir <- as.factor(train$CentralAir)
train$Electrical <- as.factor(train$Electrical)
train$KitchenQual <- factor(train$KitchenQual, levels = c("Po","Fa","Ta","Gd","Ex"))
train$Functional <- as.factor(train$Functional)
train$GarageType <- as.factor(train$GarageType)
train$GarageFinish <- factor(train$GarageFinish, levels = c("None","Unf","RFn","Fin"))
train$GarageQual <- factor(train$GarageQual, levels = c("None","Po","Fa","Ta","Gd","Ex"))
train$GarageCond <- factor(train$GarageCond, levels = c("None","Po","Fa","Ta","Gd","Ex"))
train$PavedDrive <- factor(train$PavedDrive, levels = c("N","P","Y"))
train$SaleType <- as.factor(train$SaleType)
train$SaleCondition <- as.factor(train$SaleCondition)

test$MSSubClass <- factor(test$MSSubClass)
test$MSZoning <- as.factor(test$MSZoning)
test$LotShape <- as.factor(test$LotShape)
test$LandContour <- as.factor(test$LandContour)
test$RoofStyle <- as.factor(test$RoofStyle)
test$RoofMatl <- as.factor(test$RoofMatl)
test$Exterior1st <- as.factor(test$Exterior1st)
test$Condition1 <- as.factor(test$Condition1)
test$LotConfig <- as.factor(test$LotConfig)
test$LandSlope <- as.factor(test$LandSlope)
test$Neighborhood <- as.factor(test$Neighborhood)
test$BldgType <- as.factor(test$BldgType)
test$HouseStyle <- as.factor(test$HouseStyle)
test$OverallQual <- factor(test$OverallQual, levels = c(1,2,3,4,5,6,7,8,9,10))
test$OverallCond <- factor(test$OverallCond, levels = c(1,2,3,4,5,6,7,8,9,10))
test$Exterior2nd <- as.factor(test$Exterior2nd)
test$MasVnrType <- as.factor(test$MasVnrType)
test$ExterQual <- factor(test$ExterQual, levels = c("Po","Fa","Ta","Gd","Ex"))
test$ExterCond <- factor(test$ExterCond, levels = c("Po","Fa","Ta","Gd","Ex"))
test$Foundation <- as.factor(test$Foundation)
test$BsmtQual <- factor(test$BsmtQual, levels = c("None","Po","Fa","Ta","Gd","Ex"))
test$BsmtCond <- factor(test$BsmtCond, levels = c("None","Po","Fa","Ta","Gd","Ex"))
test$BsmtExposure <- factor(test$BsmtExposure, levels = c("None","No","Mn","Av","Gd"))
test$BsmtFinType1 <- factor(test$BsmtFinType1, levels = c("None","Unf","LwQ","Rec","BLQ","ALQ","GLQ"))
test$BsmtFinType2 <- factor(test$BsmtFinType2, levels = c("None","Unf","LwQ","Rec","BLQ","ALQ","GLQ"))
test$Heating <- as.factor(test$Heating)
test$HeatingQC <- factor(test$HeatingQC, levels = c("Po","Fa","Ta","Gd","Ex"))
test$CentralAir <- as.factor(test$CentralAir)
test$Electrical <- as.factor(test$Electrical)
test$KitchenQual <- factor(test$KitchenQual, levels = c("Po","Fa","Ta","Gd","Ex"))
test$Functional <- as.factor(test$Functional)
test$GarageType <- as.factor(test$GarageType)
test$GarageFinish <- factor(test$GarageFinish, levels = c("None","Unf","RFn","Fin"))
test$GarageQual <- factor(test$GarageQual, levels = c("None","Po","Fa","Ta","Gd","Ex"))
test$GarageCond <- factor(test$GarageCond, levels = c("None","Po","Fa","Ta","Gd","Ex"))
test$PavedDrive <- factor(test$PavedDrive, levels = c("N","P","Y"))
test$SaleType <- as.factor(test$SaleType)
test$SaleCondition <- as.factor(test$SaleCondition)

factors<-names(Filter(is.factor, train))
numeric<-names(Filter(is.numeric, train))
lapply(factors, count)

factors<-Filter(is.factor, train)
lapply(factors, levels)
lapply(factors, count)
###imputation ####
#decided on using cart method (classification and regression trees) due to it being able to handle non-factor variables
tempData <- mice(test,m=5,maxit=6,meth='cart',seed=500,quickpred(test))
summary(tempData)
test <- complete(tempData,1)
sum(is.na(test))

#imputing into train using cart
ytrain <- train %>% dplyr::select(SalePrice,Id)
temptrain <- train %>% dplyr::select(-SalePrice)
tempData <- mice(temptrain,m=5,maxit=6,meth='cart',seed=500,quickpred(temptrain))
summary(tempData)
temptrain <- complete(tempData,1)
train<-full_join(temptrain,ytrain)
sum(is.na(train))
#no more missing values in test

fulldata<-full_join(train,test)

#revaluing factors

#factor '150' of MSSubclass is not represented in test data
fulldata$MSSubClass<-revalue(fulldata$MSSubClass, c("150" = "180"))
test$MSSubClass<-revalue(test$MSSubClass, c("150" = "180"))
train$MSSubClass<-revalue(train$MSSubClass, c("150" = "180"))

#neighborhood Blueste has only 10 values
#therefore, I am replacing this factor level with a nearby neighborhood
fulldata$Neighborhood<-revalue(fulldata$Neighborhood, c("Blueste" = "Blmngtn"))
test$Neighborhood<-revalue(test$Neighborhood, c("Blueste" = "Blmngtn"))
train$Neighborhood<-revalue(train$Neighborhood, c("Blueste" = "Blmngtn"))


#combining OverallQual factor levels 1, 2, and 3 into one (they represent "poor/very poor/fair")

fulldata$OverallQual<-revalue(fulldata$OverallQual, c("1" = "3","2" = "3"))
test$OverallQual<-revalue(test$OverallQual, c("1" = "3","2"="3"))
train$OverallQual<-revalue(train$OverallQual, c("1" = "3","2"="3"))

#combining OverallCond factor levels 1 and 2 into 3 (they represent "poor/very poor/fair")

fulldata$OverallCond<-revalue(fulldata$OverallCond, c("1" = "3","2" = "3"))
test$OverallCond<-revalue(test$OverallCond, c("1" = "3","2"="3"))
train$OverallCond<-revalue(train$OverallCond, c("1" = "3","2"="3"))

#roofstyle Shed level has only 5 observations, so removing this level
fulldata$RoofStyle<-revalue(fulldata$RoofStyle, c("Shed" = "Gambrel"))
test$RoofStyle<-revalue(test$RoofStyle, c("Shed" = "Gambrel"))
train$RoofStyle<-revalue(train$RoofStyle, c("Shed" = "Gambrel"))

#exterior1st has many factors with low observations, so combining into "other"
fulldata$Exterior1st<-revalue(fulldata$Exterior1st, c("ImStucc" = "Other","Stone" = "Other","CBlock" = "Other","AsphShn" = "Other","BrkComm" = "Other","AsbShng"="Other","Stucco"="Other"))
test$Exterior1st<-revalue(test$Exterior1st, c("ImStucc" = "Other","Stone" = "Other","CBlock" = "Other","AsphShn" = "Other","BrkComm" = "Other","Stucco"="Other","AsbShng"="Other"))
train$Exterior1st<-revalue(train$Exterior1st, c("ImStucc" = "Other","Stone" = "Other","CBlock" = "Other","AsphShn" = "Other","BrkComm" = "Other","Stucco"="Other","AsbShng"="Other"))

#Foundation has many factors with low observations, so combining into "other"
fulldata$Foundation<-revalue(fulldata$Foundation, c("Wood"= "Other","Stone" = "Other","Slab" = "Other"))
test$Foundation<-revalue(test$Foundation, c("Wood"= "Other","Stone" = "Other","Slab" = "Other"))
train$Foundation<-revalue(train$Foundation, c("Wood"= "Other","Stone" = "Other","Slab" = "Other"))

###transformations ####

#before transforming Sale Price, 
ggplot(train) + geom_density(aes(x=(SalePrice)))

#log transformation of saleprice
ggplot(train) + geom_density(aes(x=log(SalePrice)))

#taking log of Saleprice
fulldata$SalePrice<- log(fulldata$SalePrice)



#### simple ols model ####
simple.ols<- lm(SalePrice ~ . - Id, data = fulldata)
summary(simple.ols)


ols.results <- fulldata %>% 
  dplyr::select(Id,SalePrice) %>%
  filter(Id>1460) 

ols.results$SalePrice <-exp(predict(simple.ols, newdata=test))

write.csv(ols.results, "simpleols.csv", row.names=FALSE)
#0.13714


#### forward model ####
full.model<- lm(SalePrice~.,data=fulldata)


# #finding best model 
stepAIC(full.model,direction="forward")


#creating best forward model 
forward.model <- lm(formula = SalePrice ~ OverallQual + Neighborhood + X1stFlrSF + 
                      X2ndFlrSF + OverallCond + RoofMatl + BsmtFinSF1 + YearBuilt + 
                      GarageCars + MSSubClass + HeatingQC + SaleCondition + Functional + 
                      MSZoning + BsmtExposure + ScreenPorch + LotArea + Condition1 + 
                      BsmtFullBath + Exterior1st + Fireplaces + YearRemodAdd + 
                      BsmtUnfSF + BsmtFinSF2 + WoodDeckSF + Heating + LowQualFinSF + 
                      EnclosedPorch + LotConfig + LandSlope + LandContour + HalfBath + 
                      FullBath + CentralAir + GarageArea + PoolArea + KitchenAbvGr + 
                      BsmtQual + OpenPorchSF + GarageType + X3SsnPorch + Foundation + 
                      SaleType + LotShape, data = fulldata)

forward.results <- fulldata %>% 
  dplyr::select(Id,SalePrice) %>%
  filter(Id>1460) 

forward.results$SalePrice <-exp(predict(forward.model, newdata=test))

write.csv(forward.results, "forward.csv", row.names=FALSE)
#0.13526

#removing unused  files
rm(forward.model)
rm(forward.results)
#### backwards model ####
stepAIC(full.model,direction="backward")

backward.model <-lm(formula = SalePrice ~ MSSubClass + MSZoning + LotArea + LotShape + 
                         LandContour + LotConfig + LandSlope + Neighborhood + Condition1 + 
                         OverallQual + OverallCond + YearBuilt + YearRemodAdd + RoofMatl + 
                         Exterior1st + Foundation + BsmtQual + BsmtExposure + BsmtFinSF1 + 
                         BsmtFinSF2 + BsmtUnfSF + Heating + HeatingQC + CentralAir + 
                         X1stFlrSF + X2ndFlrSF + LowQualFinSF + BsmtFullBath + FullBath + 
                         HalfBath + KitchenAbvGr + Functional + Fireplaces + GarageType + 
                         GarageCars + GarageArea + WoodDeckSF + OpenPorchSF + EnclosedPorch + 
                         X3SsnPorch + ScreenPorch + PoolArea + SaleType + SaleCondition, 
                       data = fulldata)
                    

backward.results <- fulldata %>% 
  dplyr::select(Id,SalePrice) %>%
  filter(Id>1460) 

backward.results$SalePrice <-exp(predict(backward.model, newdata=test))

write.csv(backward.results, "backward.csv", row.names=FALSE)
#0.13526 

#removing unused files
rm(backward.results)
rm(backward.model)
####stepwise model ####

stepAIC(full.model,direction="both")

stepwise.model <-lm(formula = SalePrice ~ MSSubClass + MSZoning + LotArea + LotShape + 
                      LandContour + LotConfig + LandSlope + Neighborhood + Condition1 + 
                      OverallQual + OverallCond + YearBuilt + YearRemodAdd + RoofMatl + 
                      Exterior1st + Foundation + BsmtQual + BsmtExposure + BsmtFinSF1 + 
                      BsmtFinSF2 + BsmtUnfSF + Heating + HeatingQC + CentralAir + 
                      X1stFlrSF + X2ndFlrSF + LowQualFinSF + BsmtFullBath + FullBath + 
                      HalfBath + KitchenAbvGr + Functional + Fireplaces + GarageType + 
                      GarageCars + GarageArea + WoodDeckSF + OpenPorchSF + EnclosedPorch + 
                      X3SsnPorch + ScreenPorch + PoolArea + SaleType + SaleCondition, 
                    data = fulldata)

stepwise.results <- fulldata %>% 
  dplyr::select(Id,SalePrice) %>%
  filter(Id>1460) 

stepwise.results$SalePrice <-exp(predict(stepwise.model, newdata=test))

write.csv(stepwise.results, "stepwise.csv", row.names=FALSE)
#0.13526 kaggle

#### ridge model ####
#need to impute dummy into SalePrice to use ridge

tempData <- mice(fulldata,m=5,maxit=6,meth='cart',seed=500,quickpred(fulldata))
summary(tempData)
r.fulldata <- complete(tempData,1)
sum(is.na(r.fulldata))

x_var <- r.fulldata %>% dplyr::select(-SalePrice)
x_var <- data.matrix(r.fulldata)

# Getting the dependent variable
y_var <- data.matrix(r.fulldata$SalePrice)

# Setting the range of lambda values
cv.out <- cv.glmnet(x_var,
                    y_var,
                    nlambda=100,
                    lambda.min.ratio=0.0001)

#finding best lambda
print(best.lambda <- cv.out$lambda.min)

#creating ridge model
best_ridge <- glmnet(x_var, y_var, alpha = 0, lambda = best.lambda)

#predicting ridge model
ridgepred <- exp(predict(best_ridge, s = best.lambda, newx = x_var))

ridge.results <- fulldata %>% 
  dplyr::select(Id,SalePrice) 

ridge.results$SalePrice<-ridgepred
ridge.results <- ridge.results %>% filter(Id>1460) 
write.csv(ridge.results, "ridge.csv", row.names=FALSE)

#0.22636

#### LASSO ####

best_lasso <- glmnet(x_var, y_var, alpha = 1, lambda = best.lambda)
lassopred <- exp(predict(best_lasso, s = best.lambda, newx = x_var))

lasso.results <- fulldata %>% 
  dplyr::select(Id,SalePrice) 

lasso.results$SalePrice<-lassopred
lasso.results <- lasso.results %>% filter(Id>1460) 
write.csv(lasso.results, "lasso.csv", row.names=FALSE)
#0.24356

####ELASTIC####

cv_5 = trainControl(method = "cv", number = 5)

hit_elnet = train(
  SalePrice ~ ., data = r.fulldata,
  method = "glmnet",
  trControl = cv_5
)

#this gives me my best lambda and best alpha
hit_elnet

# Using glmnet function to build the elastic regression in r
fit <- glmnet(x_var, y_var, alpha = 0.55, lambda  = 0.005531061)

elasticpred <- exp(predict(fit , newx = x_var))


elastic.results <- fulldata %>% 
  dplyr::select(Id,SalePrice) 

elastic.results$SalePrice<-elasticpred
elastic.results <- elastic.results %>% filter(Id>1460) 
write.csv(elastic.results, "elastic.csv", row.names=FALSE)
#0.24023

#### SVM Linear Kernels ####

train$SalePrice<- as.integer(train$SalePrice)
names(numeric)

rsvmfit<-svm(SalePrice ~MSSubClass + MSZoning + LotArea + LotShape + 
 LandContour + LotConfig + LandSlope + Neighborhood + Condition1 + 
   OverallQual + OverallCond + YearBuilt + YearRemodAdd + RoofMatl + 
   Exterior1st + Foundation + BsmtQual + BsmtExposure + BsmtFinSF1 + 
   BsmtFinSF2 + BsmtUnfSF + Heating + HeatingQC + CentralAir + 
   X1stFlrSF + X2ndFlrSF + LowQualFinSF + BsmtFullBath + FullBath + 
   HalfBath + KitchenAbvGr + Functional + Fireplaces + GarageType + 
   GarageCars + GarageArea + WoodDeckSF + OpenPorchSF + EnclosedPorch + 
   X3SsnPorch + ScreenPorch + PoolArea + SaleType + SaleCondition, 
             data=train, 
             type="C-classification",
             kernel="linear", 
             cost=10)

summary(svmfit)

#getting the prediction
pred<-predict(svmfit,newdata=test)

#creating dataframe to save results into 
svmlinear.results<- fulldata %>% 
  dplyr::select(Id,SalePrice) %>%
  filter(Id>1460) 

svmlinear.results$SalePrice<-pred

write.csv(svmlinear.results, "svmlinear.csv", row.names=FALSE)
#0.20594

#### SVM Radial Kernels ####

rm(svmfit)
rm(svmlinear.results)
#model based off of features from stepwise, computer can't handle all variables

rsvmfit<-svm(SalePrice ~MSSubClass + MSZoning   
             + Neighborhood + Condition1 + OverallQual + OverallCond + YearBuilt +
               YearRemodAdd +  Exterior1st  + X1stFlrSF + X2ndFlrSF + LowQualFinSF
             + BsmtFullBath + FullBath + HalfBath  + Fireplaces + GarageType + 
               GarageCars +PoolArea + SaleType + SaleCondition, 
             data=train, 
             type="C-classification",
             kernel="radial", 
             cost=10)

# get the summary
summary(svmfit)

# now predict response for the test set
pred<-predict(rsvmfit,newdata=test)




summary(rsvmfit)

svmradial.results<- fulldata %>% 
  dplyr::select(Id,SalePrice) %>%
  filter(Id>1460) 

svmradial.results$SalePrice<-pred

write.csv(svmradial.results, "radialsvm.csv", row.names=FALSE)

#0.20529



#### SVM Polynomial Kernels ####
#model based off features from stepwise, computer can't handle all variables 
psvmfit<-svm(SalePrice ~MSSubClass + MSZoning
             + Neighborhood + Condition1 + OverallQual + OverallCond + YearBuilt + 
               YearRemodAdd + Exterior1st  +  X1stFlrSF + X2ndFlrSF + LowQualFinSF + 
               BsmtFullBath + FullBath +HalfBath  + Fireplaces + GarageType + GarageCars
              +PoolArea + SaleType + SaleCondition,
             data=train,
             kernel="polynomial",
             cost=1,gama=1)

 # get the summary
summary(psvmfit)

# now predict response for the test set
pred<-predict(psvmfit,newdata=test)



svmpoly.results<- fulldata %>% 
  dplyr::select(Id,SalePrice) %>%
  filter(Id>1460) 

svmpoly.results$SalePrice<-pred

write.csv(svmpoly.results, "polysvm.csv", row.names=FALSE)

#### Regression Trees random forest ####

#fixing factors to be same levels for variables i am using
levels(test$MSSubClass) <- levels(train$MSSubClass)
levels(test$MSZoning) <- levels(train$MSZoning)
levels(test$LotShape) <- levels(train$LotShape)
levels(test$LandContour) <- levels(train$LandContour)
levels(test$LotConfig) <- levels(train$LotConfig)
levels(test$LandSlope) <- levels(train$LandSlope)
levels(test$Neighborhood) <- levels(train$Neighborhood)
levels(test$Condition1)<- levels(train$Condition1)
levels(test$BldgType)<- levels(train$BldgType)
levels(test$HouseStyle)<- levels(train$HouseStyle)
levels(test$BldgType)<- levels(train$BldgType)
levels(test$OverallCond)<- levels(train$OverallCond)
levels(test$RoofStyle)<- levels(train$RoofStyle)
levels(test$RoofMatl)<- levels(train$RoofMatl)
levels(test$Exterior1st)<- levels(train$Exterior1st)
levels(test$Exterior2nd)<- levels(train$Exterior2nd)
levels(test$MasVnrType)<- levels(train$MasVnrType)
levels(test$ExterQual)<- levels(train$ExterQual)
levels(test$ExterCond)<- levels(train$ExterCond)
levels(test$Foundation)<- levels(train$Foundation)
levels(test$BsmtQual)<- levels(train$BsmtQual)
levels(test$BsmtCond)<- levels(train$BsmtCond)
levels(test$BsmtExposure)<- levels(train$BsmtExposure)
levels(test$BsmtFinType1)<- levels(train$BsmtFinType1)
levels(test$Heating)<- levels(train$Heating)
levels(test$HeatingQC)<- levels(train$HeatingQC)
levels(test$CentralAir)<- levels(train$CentralAir)
levels(test$Electrical)<- levels(train$Electrical)
levels(test$KitchenQual)<- levels(train$KitchenQual)
levels(test$Functional)<- levels(train$Functional)
levels(test$GarageFinish)<- levels(train$GarageFinish)
levels(test$GarageQual)<- levels(train$GarageQual)
levels(test$GarageCond)<- levels(train$GarageCond)
levels(test$PavedDrive)<- levels(train$PavedDrive)
levels(test$SaleType)<- levels(train$SaleType)
levels(test$SaleCondition)<- levels(train$SaleCondition)
#random forest with log of sale price
rf <- randomForest(
  log(SalePrice) ~ MSSubClass+MSZoning+ LotShape + LandContour+LotConfig+LandSlope+
    Neighborhood+Condition1+BldgType+HouseStyle+OverallQual+OverallCond+RoofStyle+
    RoofMatl+Exterior1st+Exterior2nd+MasVnrType+ExterQual+ExterCond+Foundation+BsmtQual+
    BsmtCond+BsmtExposure+BsmtFinType1+BsmtFinType2+Heating+HeatingQC+CentralAir+Electrical+
    KitchenQual+Functional
  +GarageType+GarageFinish+GarageQual+GarageCond+PavedDrive+SaleType+SaleCondition,
  data=train
)
#predict with exp to reverse taking log of saleprice
pred<-exp(predict(rf,newdata=test,type="response"))

rf.results<- fulldata %>% 
  dplyr::select(Id,SalePrice) %>%
  filter(Id>1460) 

rf.results$SalePrice<-pred

write.csv(rf.results, "rf.csv", row.names=FALSE)

#0.18688


#### random forest with bagging ####
bag1<-randomForest(log(SalePrice) ~.,mtry=69,train,
                   na.action = na.exclude)

pred_bag1<-exp(predict(bag1,newdata=test))

bag.results<-fulldata %>% 
  dplyr::select(Id,SalePrice) %>%
  filter(Id>1460) 


bag.results$SalePrice<-pred_bag1

write.csv(bag.results, "bag.csv", row.names=FALSE)
#0.15295

#### regression trees w boosting ####
boost1<-gbm(log(SalePrice)~.,data=train,distribution="gaussian")

pred_boost1<-exp(predict.gbm(boost1,newdata=test,n.trees = 100))

boost.results<-fulldata %>% 
  dplyr::select(Id,SalePrice) %>%
  filter(Id>1460) 


boost.results$SalePrice<-pred_boost1

write.csv(boost.results, "boost.csv", row.names=FALSE)
#0.15438
