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

#----------------------------------
#Run for entire data set together
df_prior <- prior.df(wait = df_consent$testDuration_minutes)
m.step <- main(wait = df_consent$testDuration_minutes, wait.summary.df=df_prior)
plot_mixture_models(wait = df_consent$testDuration_minutes,
                    m.step=m.step)


#------------------
#Visualize results
#' Plot a Mixture Component
#' 
#' @param x Input ata.
#' @param mu Mean of component.
#' @param sigma Standard of component.
#' @param lam Mixture weight of component.
plot_mix_comps <- function(x, mu, sigma, lam) {
  lam * dnorm(x, mu, sigma)
}

#' @param wait Input ata.
#' @param m.step approximated mixture models produced by the EM algorithm
plot_mixture_models <- function(wait){ 
  
  data.frame(x = wait) %>%
    ggplot() +
    geom_histogram(aes(x, ..density..), binwidth = 0.5, colour = "black", 
                   fill = "lightblue") +
    theme_minimal()+
    stat_function(geom = "line", fun = plot_mix_comps,
                  args = list(m.step$mu[1], sqrt(m.step$var[1]), 
                              lam = m.step$alpha[1]),
                  colour = "red", lwd = 1.0) +
    stat_function(geom = "line", fun = plot_mix_comps,
                  args = list(m.step$mu[2], sqrt(m.step$var[2]), 
                              lam = m.step$alpha[2]),
                  colour = "darkblue", lwd = 1.0) +
    ylab("Density") +
    xlab("Test Duration (minutes), binwidth=0.5") +
    ggtitle("Gaussian Mixture Model Fit - Test Duration (minutes)")
}

#---------------------------------------------------------------------
#Compute cluster membership probability
compute_Memberships <- function(mstep,df){
  
  datapoints <- df$testDuration_minutes
  mu_1 <-  m.step$mu[1]
  mu_2 <- m.step$mu[2]
  var_1 <- m.step$var[1]
  var_2 <- m.step$var[2]
  
  #probability of seeing datapoints if comming from Gaussian 1
  p_1_vector <- pnorm(datapoints,mu_1,var_1)
  p_2_vector <- pnorm(datapoints,mu_2,var_2)
  
  p_1_membership <- p_1_vector / (p_1_vector + p_2_vector)
  p_2_membership <- p_2_vector / (p_1_vector + p_2_vector)
  
  if(mu_1>mu_2){
    df$testDuration_slow <- p_1_membership
    df$testDuration_fast <- p_2_membership
  }else{
    df$testDuration_slow <- p_2_membership
    df$testDuration_fast <- p_1_membership
  }
  return(df)
}

#---------------------------------------------------------
#INVESTIGATE OUTCOMES
cor.test(df_consent$z1,df_consent$testDuration_minutes,
         alternative = "two.sided", 
         method="pearson")
#Positive correlation = 0.2350389 

cor.test(df_consent$z1,df_consent$testDuration_slow,
         alternative = "two.sided", 
         method="pearson")
#Negative correlation = -0.2183903  

cor.test(df_consent$z1,df_consent$testDuration_fast,
         alternative = "two.sided", 
         method="pearson")
#Negative correlation = 0.2183903 

slow <- df_consent$testDuration_slow
fast <- df_consent$testDuration_fast

model_1_fast <- lm(formula = z1 ~ testDuration_minutes + testDuration_minutes*testDuration_fast, data=df_consent )
model_1_slow <- lm(formula = z1 ~ testDuration_minutes + testDuration_minutes*testDuration_slow, data=df_consent )
"
Does not matter if I use fast or slow in the regression
P-values for the coefficients >0.05. Adjusted R-squared is small 0.06 
"
model_2_fast <- lm(formula = z1 ~ testDuration_minutes + testDuration_fast, data=df_consent )
model_2_slow <- lm(formula = z1 ~ testDuration_minutes + testDuration_slow, data=df_consent )

"
All coefficients are significant (p-value<0.05), but Adjusted R-squared is small 0.06 
Looking at the coefficients, we can see that not only the membership has a significant
effect, but it also affects how the testDuration_minutes influences the score.

Membership: Coefficients(testDuration_minutes, testDuration_fast)
Fast: (0.034505,1.079562)
Slow: (0.034505,-1.079562)
"
model_3 <- lm(formula = z1 ~ testDuration_minutes, data=df_consent )
model_4 <- lm(formula = z1 ~ testDuration_fast, data=df_consent )
model_5 <- lm(formula = z1 ~ testDuration_slow, data=df_consent )
"
Same for these last tow models, but the Adjusted R-squared is smaller 0.05
"
"Hence, overall, it seems that the membership information 
slighlty improved the explainability of the z1 score"


#--------------------------------------
#Hard clustering
df_consent$is_fast <- FALSE
df_consent[df_consent$testDuration_fast>=0.5,]$is_fast <- TRUE
View(df_consent[c("is_fast","testDuration_fast")])

df_consent_slow <- df_consent[!df_consent$is_fast,]

model_2_fast <- lm(formula = z1 ~ testDuration_minutes + testDuration_fast, data=df_consent_slow )
model_2_slow <- lm(formula = z1 ~ testDuration_minutes + testDuration_slow, data=df_consent_slow )
summary(model_2_fast)
summary(model_2_slow)

model_2_fast <- lm(formula = z1 ~ testDuration_minutes + testDuration_fast, data=df_consent_fast )
model_2_slow <- lm(formula = z1 ~ testDuration_minutes + testDuration_slow, data=df_consent_fast )
summary(model_2_fast)
summary(model_2_slow)

model_2_fast <- lm(formula = z1 ~ testDuration_minutes + testDuration_fast, data=df_consent_fast[df_consent_fast$profession=="Programmer",] )
model_2_slow <- lm(formula = z1 ~ testDuration_minutes + testDuration_slow, data=df_consent_fast[df_consent_fast$profession=="Programmer",] )
summary(model_2_fast)
summary(model_2_slow)

df_consent %>% 
  group_by(profession,is_fast) %>% 
  summarize(count = n())

# MOST Subjects fall in the Fast Cluster
# profession            is_fast count
# <fct>                 <lgl>   <int>
# 1 Professional          FALSE      65
# 2 Professional          TRUE      352
# 3 Programmer            FALSE       1
# 4 Programmer            TRUE       48
# 5 Hobbyist              FALSE      64
# 6 Hobbyist              TRUE      420
# 7 Graduate_Student      FALSE      59
# 8 Graduate_Student      TRUE      224
# 9 Undergraduate_Student FALSE      79
# 10 Undergraduate_Student TRUE      364
# 11 Other                 FALSE      11
# 12 Other                 TRUE      101

#----------------------------------------------------------------------
#Compute proportion by profession, because professions have distinct testDuration averages
profession_list <- as.character(unique(df_consent$profession))

for(prof in profession_list){
  print(prof)
  df_selected <- df_consent[df_consent$profession==prof,
                            c("worker_id","testDuration_minutes")]
  df_prior <- prior.df(wait = df_selected$testDuration_minutes)
  m.step <- main(wait = df_selected$testDuration_minutes, wait.summary.df=df_prior)
  df_selected <- compute_Memberships(m.step,df_selected) 
  df_consent <- left_join(df_consent,df_selected,by=c("worker_id"),copy=FALSE)
  df_consent <- rename(df_consent,
         testDuration_minutes = testDuration_minutes.x)
}
