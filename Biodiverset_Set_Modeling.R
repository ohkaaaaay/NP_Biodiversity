### Focus: Biodiversity Data Set
library(caret)
library(ggplot2)
NP_master <- read.csv("species_parks_v2.csv")
NP_master <- NP_master[c(8,14:19)]
# Make "" rows to NA
NP_master[NP_master==""] <- NA
# Remove NA rows
NP_master <- na.omit(NP_master)
str(NP_master)

#############################################################
## Simple Analysis (Without CV)
dum_NP1 <- dummyVars(~.-TRV,data=NP_master, fullRank=T)
NP.d1 <- as.data.frame(predict(dum_NP1, NP_master))
# Remove unnecessary columns
NP.d1$TRV <- NP_master$TRV
str(NP.d1)
# LM model
lm.all <- lm(TRV ~ ., data=NP.d1)
summary(lm.all)

# Remove insignificant features (Category, Occurrence, Nativeness, Conservation.Status)
dum_NP2 <- dummyVars(~Abundance+Seasonality, data=NP_master, fullRank=T)
NP.d2 <- as.data.frame(predict(dum_NP2, NP_master))
NP.d2$TRV <- NP_master$TRV
str(NP.d2)
# LM model modified 
lm.all2 <- lm(TRV ~ ., data=NP.d2)
summary(lm.all2)
# Leave only significant variables
lm.all3 <- lm(TRV ~ AbundanceOccasional+
                AbundanceRare+
                `SeasonalityBreeder, Winter`+
                SeasonalityMigratory+
                `SeasonalityMigratory, Winter`+
                SeasonalityResident+
                SeasonalitySummer+
                SeasonalityVagrant, data=NP.d2)
summary(lm.all3)
# Compare AIC
AIC(lm.all)
AIC(lm.all2) # Best model: Abundance & Seasonality only
AIC(lm.all3)

#############################################################
### Advanced Analysis (With CV)
set.seed(1)
train.index <- sample(nrow(NP_master), nrow(NP_master) * 0.8) # 80% is training data
# Create test and training data frames
# Removed acres
NP.train1 <- NP.d1[train.index,] # Model with this
NP.test1 <- NP.d1[-train.index,] # We don't touch this while training
NP.train2 <- NP.d2[train.index,] # Model with this
NP.test2 <- NP.d2[-train.index,] # We don't touch this while training

################
## Diagnostics
# Identify possible skewed variables
library(psych)
## All variables
skewValues <- apply(NP.train1, 2, skew)
skewSE <- sqrt(6/nrow(NP.train1)) # Standard error of skewness
# Anything over 2 SEs in skew is potentially problematic
abs(skewValues)/skewSE > 2
## Abundance & Seasonality
skewValues <- apply(NP.train2, 2, skew)
skewSE <- sqrt(6/nrow(NP.train2)) # Standard error of skewness
# Anything over 2 SEs in skew is potentially problematic
abs(skewValues)/skewSE > 2
# Visualize data distributions of variables with high skew identified above
#multi.hist(NP.train[,abs(skewValues)/skewSE > 2])

# Identify correlated predictors
library(corrplot)
# All variables
NP.cor1 <- cor(NP.train1)
NP.cor1
## Abundance & Seasonality
NP.cor2 <- cor(NP.train2)
NP.cor2
#corrplot(NP.cor, order="hclust") # Visualize and cluster by high correlation

# Let visualize the variables and their distributions and possible outliers
#boxplot(x = as.list(NP.train))

################
## Modeling
# K-fold CV (k = 10)
ctrl <- trainControl(method="cv", number=10)

# Train the data for all variables
set.seed(1)
np.lm <- train(TRV ~ ., data=NP.train1, method = "lm", trControl=ctrl)
np.lm$results
summary(np.lm)

# Train the data for selected variables (Seasonality & Abundance)
set.seed(1)
np.lm2 <- train(TRV ~ ., data=NP.train2, method = "lm", trControl=ctrl)
np.lm2$results
summary(np.lm2)

# Diagnostic plots
par(mfrow=c(2,1))
#plot(NP.train$TRV ~ predict(np.lm), xlab="Predict", ylab="Actual", main="Actual vs. Predicted")
#plot(resid(np.lm) ~ predict(np.lm), xlab="Predict", ylab="Residuals", main="Predicted Residuals vs. Predicted Values")

# Review what variables were most important
varImp(np.lm)
varImp(np.lm2)

# Lets fit a model that preprocess predictors on each resample fold based on our earlier findings
set.seed(1)
np.lm.pp <- train(TRV ~ ., data=NP.train1,
                 preProcess=c("BoxCox", "scale", "center"),
                 method = "lm", trControl=ctrl)
summary(np.lm.pp)

# Let create a model removing correlated predictors
# Cutoff correlation at >0.75
highlyCorDescr <- findCorrelation(NP.cor1, cutoff=0.75) # No correlation
set.seed(1)
np.lm.cor <- train(TRV ~ ., data=NP.train1[,-highlyCorDescr], 
                  method = "lm", trControl=ctrl)

#############################################################
## Lasso & Ridge Regression
library(glmnet)
# Define as matrices for cv.glm()
x <- as.matrix(NP.train1[-43])
y <- as.matrix(NP.train1[43])

## Ridge Regression
set.seed(1)
cv.rr <- cv.glmnet(x, y, alpha=0) # alpha=0 for ridge regression
# Display the best lambda value
lambda.rr <- cv.rr$lambda.min
# Plot the MSE for each tuning parameter
par(mfrow=c(1,1))
plot(cv.rr)
# Ridge regression with pre-process
np.rr.pp <- train(TRV ~ ., data=NP.train1, method="glmnet", trControl=ctrl,
                     tuneGrid=expand.grid(alpha=0, lambda=lambda.rr),
                     preProcess=c("BoxCox", "scale", "center"))
getTrainPerf(np.rr.pp)

## Lasso
# Find the best lambda using cross-validation
set.seed(1)
cv.lasso <- cv.glmnet(x, y, alpha=1) # alpha=1 for lasso
# Display the best lambda value
lambda.lasso <- cv.lasso$lambda.min
# Plot the MSE for each tuning parameter
plot(cv.lasso)
# Lasso with pre-process
np.lasso.pp <- train(TRV ~ ., data=NP.train1, method="glmnet", trControl=ctrl,
                  tuneGrid=expand.grid(alpha=1, lambda=lambda.lasso),
                  preProcess=c("BoxCox", "scale", "center"))
getTrainPerf(np.lasso.pp)

#############################################################
### PCR & PLS
library(pls)

## PCR with pre-process
# Model is really bad and for good reason (not using a response variable)
set.seed(1)
np.pcr <- train(TRV ~ ., data=NP.train1, method = "pcr", trControl=ctrl,
                preProcess=c("BoxCox", "scale", "center"))
np.pcr # ncomp=1

## PLS with pre-process
set.seed(1)
np.pls <- train(TRV ~ ., data=NP.train1, method = "pls", trControl=ctrl,
                preProcess=c("BoxCox", "scale", "center"))
np.pls # ncomp=3

#############################################################
### Random Forest
library(randomForest)
## Pre-processing not needed
## ntree=5
# Tune length is the max # of parameters
set.seed(1)
np.rf.5 <- train(TRV ~ ., data=NP.train1, method = "rf", trControl=ctrl,
               ntree=5, tuneLength=41)
np.rf.5 # mtry=6
t5 <- np.rf.5$results[5,2] # RMSE

## ntree=10
# Tune length is the max # of parameters
set.seed(1)
np.rf.10 <- train(TRV ~ ., data=NP.train1, method = "rf", trControl=ctrl,
                 ntree=10, tuneLength=41)
np.rf.10 # mtry=9
t10 <- np.rf.10$results[8,2] # RMSE

## ntree=50
# Tune length is 10
set.seed(1)
np.rf.50 <- train(TRV ~ ., data=NP.train1, method = "rf", trControl=ctrl,
                  ntree=50, tuneLength=10)
np.rf.50 # mtry=6
t50 <- np.rf.50$results[2,2] # RMSE

## ntree=100
# Tune length is 10
set.seed(1)
np.rf.100 <- train(TRV ~ ., data=NP.train1, method = "rf", trControl=ctrl,
                  ntree=100, tuneLength=10)
np.rf.100 # mtry=6
t100 <- np.rf.100$results[2,2] # RMSE

# Default value (ntree=500)
# Tune length is 5
set.seed(1)
np.rf.500 <- train(TRV ~ ., data=NP.train1, method = "rf", trControl=ctrl,
               ntree=500, tuneLength=5)
np.rf.500 # mtry=12
t500 <- np.rf.500$results[2,2] # RMSE

## Assess the best RMSE value
ntree <- cbind(c(5, 10, 50, 100, 500), c(t5, t10, t50, t100, t500))
ntree[which.min(ntree[,2])] # ntree=100 has the min. RMSE

# Plot a sample tree from the selected random forest
library(rattle)
getTree(np.rf.500$finalModel)
# Plot the error vs. number of trees
plot(np.rf.500$finalModel, main='')

#############################################################
### GAM Smoothing Spline - Significant Variables Selected
# Check the significant variables in the lm model
summary(np.lm)

# Apply selected categories in GAM model
#gamGrid <-  expand.grid(df = c(1:10))
np.ss.pp <- train(TRV ~ AbundanceCommon+
                    AbundanceOccasional+
                    AbundanceRare+
                    AbundanceUncommon+
                    `SeasonalityBreeder, Winter`+
                    SeasonalityMigratory+
                    `SeasonalityMigratory, Winter`+
                    SeasonalityResident+
                    SeasonalitySummer+
                    SeasonalityVagrant,
                  data=NP.train2, method="gamSpline", trControl=ctrl,
                  preProcess=c("BoxCox", "scale", "center"))
np.ss.pp # df=1

#############################################################
### COMPARE MODEL PERFORMANCE
# Let's compare all of the model resampling performance
# First lets put all trained models in a list object
np.models <- list("LM Base"=np.lm,
                 "LM Selected"=np.lm2,
                 "LM PreProc"=np.lm.pp,
                 "LM Cor Rmved"=np.lm.cor,
                 "Lasso PreProc"=np.lasso.pp,
                 "RR PreProc"=np.rr.pp,
                 "PCR PreProc"=np.pcr,
                 "PLS PreProc"=np.pls,
                 "GAM Smoothing Spline"=np.ss.pp,
                 "Random Forest"=np.rf.100)

NP.resamples <- resamples(np.models)
summary(NP.resamples)

# Plot performances
bwplot(NP.resamples, metric="RMSE")
bwplot(NP.resamples, metric="Rsquared")

# Evaluate differences in model and see if their are statistical significant differnces
NP.diff <- diff(NP.resamples)
summary(NP.diff) # Lower diagonal is p-value of differences

### TEST DATA EVALUATION
# Creating function called testPerformance to run against each model
# To measure test performance
testPerformance <- function(model) {
  postResample(predict(model, NP.train1), NP.train1$TRV)
}

# Apply the test performance function against each of the models in the list
test.mod <- lapply(np.models, testPerformance)

# Variable setup
model_test <- c("LM Base", "LM Selected", "LM PreProc", "LM Cor Rmved",
                "Lasso PreProc", "RR PreProc", "PCR PreProc", "PLS PreProc",
                "GAM Smoothing Spline", "Random Forest")
RMSE_test <- c(test.mod$`LM Base`[[1]], test.mod$`LM Selected`[[1]],
               test.mod$`LM PreProc`[[1]], test.mod$`LM Cor Rmved`[[1]],
               test.mod$`Lasso PreProc`[[1]], test.mod$`RR PreProc`[[1]],
               postResample(predict(np.pcr, NP.train1), NP.train1$TRV)[[1]],
               test.mod$`PLS PreProc`[[1]],
               test.mod$`GAM Smoothing Spline`[[1]],
               test.mod$`Random Forest`[[1]])
R2_test <- c(test.mod$`LM Base`[[2]], test.mod$`LM Selected`[[2]],
             test.mod$`LM PreProc`[[2]], test.mod$`LM Cor Rmved`[[2]],
             test.mod$`Lasso PreProc`[[2]], test.mod$`RR PreProc`[[2]],
             postResample(predict(np.pcr, NP.train1), NP.train1$TRV)[[2]],
             test.mod$`PLS PreProc`[[2]],
             test.mod$`GAM Smoothing Spline`[[2]],
             test.mod$`Random Forest`[[2]])

# Plot training RMSE performance
ggplot(data=data.frame(model_test, RMSE_test),
       aes(x=model_test, y=RMSE_test, fill=model_test)) +
  geom_bar(stat="identity", colour="black") +
  labs(y="RMSE") +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x=element_blank()) +
  labs(fill = "Models") +
  scale_fill_brewer(palette="OrRd")
# Plot training R^2 performance
ggplot(data=data.frame(model_test, R2_test),
       aes(x=model_test, y=R2_test, fill=model_test)) +
  geom_bar(stat="identity", colour="black") +
  labs(y="R^2", x="Model") +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x=element_blank()) +
  labs(fill = "Models") +
  scale_fill_brewer(palette="PuBuGn")
