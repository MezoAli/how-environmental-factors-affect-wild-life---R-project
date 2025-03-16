# Import packages
if (!requireNamespace("survminer", quietly = TRUE)) install.packages("survminer")
if (!requireNamespace("GPArotation", quietly = TRUE)) install.packages("GPArotation")
library(psych)
library(survival)
library(survminer)
library(readr)
library(broom)
library(GPArotation)
library(tidyverse)
## factor analysis
# Load the factor_data.csv
factor_data <- read_csv("factor_data.csv")

# Load the survival_data.csv
survival_data <- read_csv("survival_data.csv")

#create correlation matrix for factor data
cor_factor_data <- cor(factor_data,use = "pairwise.complete.obs")

#what is the most impactaful factor
most_impactful_factor <- "DeforestationRate"

#see how many factor exist using scree function and eigen values
scree(cor_factor_data,factors = F)
val <- eigen(cor_factor_data)
num_factors <- sum(val$values > 1)

#running Explanatory factor analysis and see the loadings
EFA_model <- fa(factor_data,nfactors = 2)

## survival analysis

#gte the names of coulmns
colnames(survival_data)
#running Kaplen Meier plot for visulazation
survival_fit <- survfit(Surv(Survival_Time,Censoring_Status) ~ Habitat,data = survival_data)
plot(survival_fit)

# see the coefs
survival_fit_df <- tidy(survival_fit)

#using surviminer package to plot the survival plot
ggsurvplot(survival_fit)

# which plant has the lowerst survival propability from the plot
low_surv_habitat <- "Savanna"

