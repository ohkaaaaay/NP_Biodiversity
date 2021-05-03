### Focus: Species & Acres
## Analyze species count and acres
# This dataset had to be analyzed separately the data would repeat in each row
# Thus adding more weight and significance
library(caret)
psva_count <- read.csv("species_parks_subset.csv")
str(psva_count)

#############################################################
### Simple Analysis (Without CV)
lm.subset <- lm(Visitors ~ Acres+Species, data=psva_count)
summary(lm.subset)
# Remove acres
lm.subset2 <- lm(Visitors ~ Species, data=psva_count)
summary(lm.subset2)
# Interaction effect
lm.subset3 <- lm(Visitors ~ Acres*Species, data=psva_count)
summary(lm.subset3)
# Compare AIC
AIC(lm.subset)
AIC(lm.subset2) # Best model: Species only
AIC(lm.subset3)
# Plot the species vs. visitors
plot(psva_count$Species, psva_count$Visitors)
abline(lm.subset2, col="red")

#############################################################
### Advanced Analysis (With CV)
# Split the data
set.seed(1)
train.index <- sample(nrow(psva_count), nrow(psva_count) * 0.8) # 80% is training data
# Create test and training data frames
# Removed acres
NP.train <- psva_count[train.index, c(2,4)] # Model with this
NP.test <- psva_count[-train.index, c(2,4)] # We don't touch this while training

################
## Diagnostics
# Identify possible skewed variables
library(psych)
skewValues <- apply(NP.train, 2, skew)
skewSE <- sqrt(6/nrow(NP.train)) # Standard error of skewness
# Anything over 2 SEs in skew is potentially problematic
abs(skewValues)/skewSE > 2
## Visualize data distributions of variables with high skew identified above
multi.hist(NP.train[,abs(skewValues)/skewSE > 2])

# Identify correlated predictors
library(corrplot)
NP.cor <- cor(NP.train[,1:2])
NP.cor
corrplot(NP.cor, order="hclust") # Visualize and cluster by high correlation
# No correlation between acres and visitors

# Let visualize the variables and their distributions and possible outliers
boxplot(x = as.list(NP.train)) # Limit has an extremely large range and outliers that could influence things
# Lets remove limit to see the Species distribution better
boxplot(x = as.list(NP.train[1]))

################
## Modeling
# K-fold CV (k = 10)
ctrl <- trainControl(method="cv", number=10)

# Train the data
set.seed(1)
np.lm <- train(Visitors ~ ., data=NP.train, method = "lm", trControl=ctrl)
summary(np.lm)

# Diagnostic plots
par(mfrow=c(2,1))
plot(NP.train$Visitors ~ predict(np.lm), xlab="Predict", ylab="Actual",
     main="Actual vs. Predicted")
plot(resid(np.lm) ~ predict(np.lm), xlab="Predict", ylab="Residuals",
     main="Predicted Residuals vs. Predicted Values")

# Review what variables were most important
varImp(np.lm) # Species is important

# Lets fit a model that preprocess predictors on each resample fold based on our earlier findings
set.seed(1)
np.lm.pp <- train(Visitors ~ ., data=NP.train,
                 preProcess=c("BoxCox", "scale", "center"),
                 method = "lm", trControl=ctrl)
summary(np.lm.pp)

# Let create a model removing correlated predictors
set.seed(1)
# Cutoff correlation at >0.75
highlyCorDescr <- findCorrelation(NP.cor, cutoff=0.75)
highlyCorDescr # No correlation

#############################################################
## kNN
# Set values of k to search through, K 1 to 10
k.grid <- expand.grid(k=1:10)
set.seed(1)
np.knn.pp <- train(Visitors ~ ., data=NP.train, method = "knn", 
                  preProcess=c("BoxCox", "center", "scale"),
                  tuneGrid=k.grid, trControl=ctrl)
np.knn.pp # k=9

#############################################################
## GAM Smoothing Spline
gamGrid <-  expand.grid(df = c(1:10))
set.seed(1)
np.ss.pp <- train(Visitors ~ ., data=NP.train, method="gamSpline", trControl=ctrl,
                  preProcess=c("BoxCox", "scale", "center"),
                  tuneGrid=gamGrid)
np.ss.pp # df=2
# Plot the RMSE over df
plot(np.ss.pp)

#############################################################
### COMPARE MODEL PERFORMANCE
# Let's compare all of the model resampling performance
# First lets put all trained models in a list object
c.models<- list("LM Base"=np.lm, "LM PreProc"=np.lm.pp,
                "kNN PreProc"=np.knn.pp, "GAM Smoothing Spline"=np.ss.pp)

NP.resamples <- resamples(c.models)
summary(NP.resamples)

# Plot performances
bwplot(NP.resamples, metric="RMSE")
# Disregard outliers
bwplot(NP.resamples, metric="Rsquared")

# Evaluate differences in model and see if their are statistical significant differnces
NP.diff <- diff(NP.resamples)
summary(NP.diff) # Lower diagonal is p-value of differences

### TEST DATA EVALUATION
# Creating function called testPerformance to run against each model
# To measure test performance
testPerformance <- function(model) {
  postResample(predict(model, NP.test), NP.test$Visitors)
}

# Apply the test performance function against each of the models in the list
lapply(c.models, testPerformance)
