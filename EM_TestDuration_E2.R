"
Gaussian Mixture Model for the testDuration in E2
"

library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(tidyr)
library(tidyverse)

#scripts to run EM algorithm
source("C://Users//Christian//Documents//GitHub//EM_GaussianMixtureModel_TaskDurations//main_bivariate_EM.R")
source("C://Users//Christian//Documents//GitHub//EM_GaussianMixtureModel_TaskDurations//prior_kmeans_EM.R")

#Load data from demographics and qualification test Experiment-2
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data_loaders//load_consent_create_indexes_E2.R")
df_E2_ground<- df_consent

df_prior <- prior.df(wait = df_consent$testDuration_minutes)
df_prior

main(wait = df_consent$testDuration_minutes, wait.summary.df=df_prior)
