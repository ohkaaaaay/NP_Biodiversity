#############################################################
### Focus: Biodiversity Data Set

# Read files
NP_master <- read.csv("species_parks_v2.csv")
psva_count <- read.csv("species_parks_subset.csv")

## Linear Regression
lm.all <- lm(TRV ~ Category+Occurrence+Nativeness+Abundance+
               Seasonality+Conservation.Status, data=NP_master)
summary(lm.all)

## Feature selection

## Lasso

## PCA

## Non-linear regression

#############################################################
### Focus: Species & Acres
## Analyze species count and acres
# This dataset had to be analyzed separately the data would repeat in each row
# Thus adding more weight and significance
psva_count
lm.subset <- lm(Visitors ~ Acres+Species, data=psva_count)
summary(lm.subset)
# Remove acres
lm.subset2 <- lm(Visitors ~ Species, data=psva_count)
summary(lm.subset)
# Interaction effect
lm.subset3 <- lm(Visitors ~ Acres*Species, data=psva_count)
summary(lm.subset3)
# Compare AIC
AIC(lm.subset)
AIC(lm.subset2) # Best model: Species only
AIC(lm.subset3)

## Non-linear regression

#############################################################
### Focus: GAM
