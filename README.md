# Table of Contents
**Note**: Refer to the [APA-formatted report](https://github.com/ohkaaaaay/NP_Biodiversity/blob/main/Biodiversity_s_Effect_on_the_Number_of_Visitors_at_National_Parks.pdf) for further instructions on which files to use at each step. LaTex, through Overleaf, was used to generate the report. To view the report's source code go to the [Report_Latex folder](https://github.com/ohkaaaaay/NP_Biodiversity/tree/main/Report_Latex).

## Preprocessing Data
- [NP_Bio_Data_Preproccesing_Part1.ipynb](https://github.com/ohkaaaaay/NP_Biodiversity/blob/main/NP_Bio_Data_Preproccesing_Part1.ipynb)
- [NP_Bio_Data_Preproccesing_Part2.R](https://github.com/ohkaaaaay/NP_Biodiversity/blob/main/NP_Bio_Data_Preproccesing_Part2.R)
- [Annual_NP_Visitation.csv](https://github.com/ohkaaaaay/NP_Biodiversity/blob/main/Annual_NP_Visitation.csv)
- [parks.csv](https://github.com/ohkaaaaay/NP_Biodiversity/blob/main/parks.csv)
- [species.csv](https://github.com/ohkaaaaay/NP_Biodiversity/blob/main/species.csv)
- [species_parks.csv](https://github.com/ohkaaaaay/NP_Biodiversity/blob/main/species_parks.csv)

## Modeling
- [Biodiverset_Set_Modeling.R](https://github.com/ohkaaaaay/NP_Biodiversity/blob/main/Biodiverset_Set_Modeling.R)
- [Species_Acres_Modeling.R](https://github.com/ohkaaaaay/NP_Biodiversity/blob/main/Species_Acres_Modeling.R)
- [species_parks_subset.csv](https://github.com/ohkaaaaay/NP_Biodiversity/blob/main/species_parks_subset.csv)
- [species_parks_v2.csv](https://github.com/ohkaaaaay/NP_Biodiversity/blob/main/species_parks_v2.csv)

# Executive Summary
U.S. National Parks can get up to 91 million recreational visits each year. Other than the beautiful landmarks, a lot of people visit the parks in hope to scout some wildlife. The National Park Service (NPS) keeps a detailed report of all their species along with the number of visitors for each year. With the data taken from Kaggle and the NPS site, I wanted to see if the biodiversity in these parks has an effect on the number of visitors.

The visitor count of each park was filtered for the year 2016 only since the species collection data was taken around that time. There are about 119,000 recorded species with 18 different features. Only seven of these features were further modeled since those qualitative variables had too many categories (at least more than 30) that would add complexity to the model. Also, a subset of data was analyzed as well, which focused on the attributes of each national park. These attributes include the number of species and size in acres as the independent variables and the number of visitors as the dependent variable. The reason why this subset was not incorporated into the species biodiversity dataset is that it would add weight to the number of species attribute when the row is only about one particular species. It was easier to breakdown the model for better analysis and interpretation.

The following models performed on the biodiversity dataset are linear regression, dimensionality reduction (principal component regression (PCR) and partial least squares (PLS)), feature selection (lasso and ridge regression), general additive model (GAM) smoothing splines, and random forest. As for the park subset data, the only models performed are linear regression, kNN, and GAM smoothing spline. After splitting these datasets into training and testing sets, a 10-fold cross validation was performed to find the best parameters to apply on the models in the testing set.

The RMSE and R<sup>2</sup> from the testing set showed linear regression as a good all-around model to use. It even performed fairly well on the biodiversity dataset that had a lot dimensionality. Also, the random forest model, as expected, had a good performance since it can manage non-linear datasets and qualitative variables well. For the park subset data, the kNN unexpectedly, came out to be the best model. The model can be sensitive when choosing the right number of neighbors, but after optimizing the parameter through cross-validation, the model overcame the bias-variance tradeoff.

Focusing on a handful of national parks and incorporating  features other than biodiversity can help determine what influences the number of visitors to these national parks. This knowledge is important for NPS to know so they can protect wildlife, maintain crowd control, and preserve these parks overall.

For these datasets, I used Python and R as my main tools for pre-processing and modeling.
