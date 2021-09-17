"
Gaussian Mixture Model for the testDuration in E2
"

library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(tidyr)
library(tidyverse)

#scripts to run EM algorithm
source("C://Users//Christian//Documents//GitHub//EM_GaussianMixtureModel_TaskDurations//util//main_bivariate_EM.R")
source("C://Users//Christian//Documents//GitHub//EM_GaussianMixtureModel_TaskDurations//util//prior_kmeans_EM.R")
source("C://Users//Christian//Documents//GitHub//EM_GaussianMixtureModel_TaskDurations//util//visualize_compute_membership.R")

"
Take-aways.
- Both Slow and Fast memberships are positively correlated with score. The strenght of
the correlation is small and similar to the test_duration. 
- Regression models that try to combine memberships are interaction terms or additive terms,
do not show improvement in explanation of the socre by the test_duration
- For this reason, I decided to only explore the binary membership, if fast_membership>median, 
than the participant was categorized as fast (is_fast==TRUE), otherwise, the participants is part 
of the slow cluster (is_fast==FALSE).


TODO:
- Merge Professional and Programmer (check)
- Remove two outliers test_duration > 20min or >=15 (above whiskers)
- Plot box plots to remove outliers
- Instead of outliers, I would just match the support of the distributions

\begin{center}
\begin{tabular}{ c | c }
 profession & test duration (minutes)\\ [0.5ex] 
 \hline \hline
 Programmer & 25.25025 \\  
 \hline
 Other & 18.06738 \\
 \hline
 Professional & 16.3688\\
 \hline
 Hobbyist & 15.36393 \\
 \hline
 Undergraduate Student & 14.7441 \\
 \hline
 Graduate Student & 14.60075 \\ [0.5ex]
\end{tabular}
\end{center}

"


#---------------------------------------------------------
#----------------------------------
#Load data from demographics and qualification test Experiment-2
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data_loaders//load_consent_create_indexes_E2.R")
df_consent <- load_consent_create_indexes();


#Run for entire data set together
df_consent$testDuration_fastMembership <- NA
df_prior <- prior.df(wait = df_consent$test_duration)
m.step <- main(wait = df_consent$test_duration, wait.summary.df=df_prior)
df_consent <- compute_Memberships(m.step, df_consent)
plot <- plot_mixture_models(df_consent$test_duration,m.step,"All E2")
plot
#---------------------------------------------------------
#INVESTIGATE OUTCOMES
cor.test(df_consent$adjusted_score,df_consent$test_duration,
         alternative = "two.sided", 
         method="pearson")
#Positive correlation =0.2346014   

cor.test(df_consent$adjusted_score,df_consent$testDuration_slowMembership,
         alternative = "two.sided", 
         method="pearson")
#Positive correlation = 0.2334522 

cor.test(df_consent$adjusted_score,df_consent$testDuration_fastMembership,
         alternative = "two.sided", 
         method="pearson")
#Negative correlation = 0.2514127  

#-----------------------------------------------------
#REGRESSION MODELS WITH ADJUSTED SCORE

#With Interactions
model_1_fast <- lm(formula = adjusted_score ~ test_duration + test_duration*testDuration_fastMembership, data=df_consent )
model_1_slow <- lm(formula = adjusted_score ~ test_duration + test_duration*testDuration_slowMembership, data=df_consent )
summary(model_1_fast) 
summary(model_1_slow)
"
REJECTED MODEL
Does not matter if I use fast or slow in the regression
Only testDuration_fast/slowMembership is significant (p-value<0.05)
Adjusted R-squared is very small = 0.06
"
#-------------------------
#Without Interactions
model_2_fast <- lm(formula = adjusted_score ~ test_duration + testDuration_fastMembership, data=df_consent )
model_2_slow <- lm(formula = adjusted_score ~ test_duration + testDuration_slowMembership, data=df_consent )
summary(model_2_fast)
summary(model_2_slow)
"
All the coefficients are significant (p-value<0.05) but Adjusted R-squared is small 0.06 
Looking at the coefficients, we can see that not only the membership has a large effect.
Membership: Coefficients(testDuration, testDuration_X_Membership)
Fast: (0.009346,1.553558)
Slow: (0.009346,-1.553558)
"
#-----------------------------------------
# ONLY fast/slow Membership as predictor

model_3_all <- lm(formula = adjusted_score ~ test_duration, data=df_consent )
model_4_fast <- lm(formula = adjusted_score ~ testDuration_fastMembership, data=df_consent )
model_5_slow <- lm(formula = adjusted_score ~ testDuration_slowMembership, data=df_consent )
summary(model_3_all)
summary(model_4_fast)
summary(model_5_slow)

"
All coefficients are significant (p-value<0.05), but Adjusted R-squared is small 0.04 
Model: Coefficients(testDuration OR testDuration_X_Membership)
model_3_all: (0.11841)
model_4_fast: (4.0755)
model_5_slow: (-1.66675)

"
"Hence, overall, it seems that the membership information 
slighlty improved the explainability of the adjusted_score score
"

#---------------------------------------------
#REGRESSION MODELS WITH ORIGINAL SCORE

#Without Interactions
model_1_all <- lm(formula = qualification_score ~ test_duration, data=df_consent )
model_1_fast <- lm(formula = qualification_score ~ test_duration + testDuration_fastMembership, data=df_consent )
model_1_slow <- lm(formula = qualification_score ~ test_duration + testDuration_slowMembership, data=df_consent )
summary(model_1_all)
summary(model_1_fast)
summary(model_1_slow)
"
All coefficients are significant (p-value<0.05), but Adjusted R-squared is small 0.06 
Looking at the coefficients, we can see that not only the membership has a large effect.
Membership: Coefficients(testDuration, testDuration_X_Membership)
All: (0.11843)
Fast: (0.07582,2.90591)
Slow: (0.07582,-2.90591)
"

"
Adjusted_Score has 60% weaker coefficients than qualification_score for E2
"

#--------------------------------------
#Hard clustering (not using the membership probability. Instead, using a categorical value - is_fast)
df_consent$is_fast <- FALSE
median_fast_membership <- median(df_consent$testDuration_fastMembership);
df_consent[df_consent$testDuration_fastMembership<=median_fast_membership,]$is_fast <- TRUE


#View(df_consent[c("is_fast","testDuration_fastMembership")])

df_consent_slow <- df_consent[!df_consent$is_fast,]
df_consent_fast <- df_consent[df_consent$is_fast,]

model_2_fast <- lm(formula = adjusted_score ~ test_duration, data=df_consent_fast )
model_2_slow <- lm(formula = adjusted_score ~ test_duration,  data=df_consent_slow )
summary(model_2_fast)
summary(model_2_slow)
#(fast/slow, test_duration)
#(fast, 0.26294) <<<<<<<<<<< FAST has a strong positive correlation with score
#(slow, 0.07491) 

model_2_fast <- lm(formula = adjusted_score ~ test_duration + testDuration_fastMembership, data=df_consent_fast )
model_2_slow <- lm(formula = adjusted_score ~ test_duration + testDuration_slowMembership, data=df_consent_slow )
summary(model_2_fast)
summary(model_2_slow)
#(fast/slow, test_duration, testDuration_Membership)
#(fast,  -1.6163,  16.5009)
#(slow,  -0.2780,  11.7301)

"
This analysis does not show much, only that people in the slower cluster have a stronger
association with score, i.e., they are being more thoroug, whereas the fast ones are slopier.
So, w.r.t, interventions, this means that giving more time to faster people might not help....
"

#---------------------------------------------------------

df_consent %>% 
  group_by(profession,is_fast) %>% 
  summarize(count = n())

" The proportions changed with respect to when we computed for all population.
The data is more balanced for all,except Professionals, who none fit in two Gaussians.
"
# MOST Subjects fall in the Fast Cluster
# profession              is_fast    count   %
# <fct>                    <lgl>     <int>  <int>
# 1 Professional           TRUE       184   44%
# 2 Professional           FALSE      233   56%
#                                     417
# 3 Programmer             TRUE        14   29% <<<<<<<<<
# 4 Programmer             FALSE       35   71%
#                                      49  
# 5 Hobbyist               TRUE       216   45%
# 6 Hobbyist               FALSE      268   55$
#                                     484
# 7 Graduate_Student       TRUE       181   64%
# 8 Graduate_Student       FALSE      102   36% <<<<<<<<<
#                                     283
# 9 Undergraduate_Student  TRUE       268   60%
# 10 Undergraduate_Student FALS       175   40%
#                                     443
# 11 Other                 TRUE        46   41%
# 12 Other                 FALSE       66   59%

#----------------------------------------------------------------------
#----------------------------------------------------------------------
#Now compute the membership by each profession, 
#because professions have different mean values for test_duration.

source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data_loaders//load_consent_create_indexes_E2.R")
df_consent <- load_consent_create_indexes();

#Compute proportion by profession, because professions have distinct testDuration averages
profession_list <- as.character(unique(df_consent$profession))

df_consent$testDuration_fastMembership <- NA
df_consent$is_fast <- FALSE

for(profes in profession_list){
  profes <- profession_list[6]
  print(profes)
  df_selected <- df_consent[df_consent$profession==profes,
                            c("worker_id","file_name","test_duration","profession")]
  df_prior <- prior.df(wait = df_selected$test_duration)
  m.step <- main(wait = df_selected$test_duration, wait.summary.df=df_prior)
  df_selected <- compute_Memberships(m.step,df_selected) 
  df_consent$testDuration_fastMembership[which(df_consent$worker_id %in% df_selected$worker_id
                                     &
                                       df_consent$file_name %in% df_selected$file_name 
                                     & 
                                        df_consent$profession %in% df_selected$profession)] <- df_selected$testDuration_fastMembership
 
  df_consent$testDuration_slowMembership[which(df_consent$worker_id %in% df_selected$worker_id
                                               &
                                                 df_consent$file_name %in% df_selected$file_name 
                                               & 
                                                 df_consent$profession %in% df_selected$profession)] <- df_selected$testDuration_slowMembership
  
  
   #Set flag based on fastMembership
  df_selected$is_fast <- FALSE
  median_fast_membership <- median(df_selected$testDuration_fastMembership);
  print(median_fast_membership)
  median_slow_membership <- median(df_selected$testDuration_slowMembership)
  print(median_slow_membership)
  df_selected[df_selected$testDuration_fastMembership>=df_selected$testDuration_slowMembership,]$is_fast <- TRUE
  #Store flag in df_consent
  df_consent$is_fast[which(df_consent$worker_id %in% df_selected$worker_id
                           &
                          df_consent$file_name %in% df_selected$file_name 
                           & 
                          df_consent$profession %in% df_selected$profession)] <- df_selected$is_fast;
  
  
  
  #plot model for the profession
  plot <- plot_mixture_models(df_selected$test_duration,m.step,paste0(profes," E2"))
  plot
}


#Save df_consent to file so we can retrieve the tesDuration_fast
path = "C://Users//Christian//Documents//GitHub//EM_GaussianMixtureModel_TaskDurations//output//"
write.csv(df_consent,paste0(path,"consent_with_testDuration_fastMembership.csv"));

#Check the proportion across professions.
df_consent %>% 
  group_by(profession,is_fast) %>% 
  summarize(count = n())


#   profession            is_fast count    %
#   <fct>                 <lgl>   <int>  <int>
# 1 Professional          FALSE     177   42%
# 1 Professional          TRUE      240   58%
#                                   417  100%
# 2 Programmer            FALSE      24   50%
# 3 Programmer            TRUE       25   50%
#                                        100%
# 4 Hobbyist              FALSE     242   50%    
# 5 Hobbyist              TRUE      242   50%
#                                        100%
# 6 Graduate_Student      FALSE     141   50%
# 7 Graduate_Student      TRUE      142   50%
#                                        100%
# 8 Undergraduate_Student FALSE     175   40%
# 9 Undergraduate_Student TRUE      268   60%
#                                        100%
# 10 Other                FALSE      56   50%
# 11 Other                TRUE       56   50%
#                                        100%

"Distribution now is 50% fast and 50% slow within each profession"
"
\begin{center}
\begin{tabular}{ c | c | c | c }
profession & is fast? & group threshold & % \\ [0.5ex] 
\hline \hline
 Professional&FALSE&177&42% \\  
\hline
Professional&TRUE&240&58% \\
\hline
Programmer&FALSE&17&35% \\
\hline
Programmer&TRUE&32&65% \\
\hline
Hobbyist&FALSE&242&50% \\
\hline
Hobbyist&TRUE&242&50% \\ 
\hline
Graduate_Student&FALSE&141&50% \\ 
\hline
Graduate_Student&TRUE&142&50% \\ 
\hline
Undergraduate_Student&FALSE&175&40% \\ 
\hline
Undergraduate_Student&TRUE&268&60% \\
\hline
Other&FALSE&56&50% \\ 
\hline
Othre&TRUE&56&50% \\ [0.5ex]
\end{tabular}
\end{center}
" 


#---------------
#PLOTS to show this phenomenon

#---------------------
minmax <- function(df_data){
  profession_list <- as.character(unique(df_consent$profession))
  max <- max(df_data$test_duration);
  for(profes in profession_list){
    aux <- max(df_data[df_data$profession==profes,]$test_duration)
    if(aux<max)
      max <- aux
  }
  return(max);
}

maxmin <- function(df_data){
  profession_list <- as.character(unique(df_consent$profession))
  min <- min(df_data$test_duration);
  for(profes in profession_list){
    aux <- min(df_data[df_data$profession==profes,]$test_duration)
    if(aux>min)
      min <- aux
  }
  return(min);
}

#--------------------

df_consent_fast <- df_consent[df_consent$is_fast,]
df_consent_slow <- df_consent[!df_consent$is_fast,]
df_consent_slow <- rbind(df_consent_slow, c(1:32))
df_consent_slow[is.na(df_consent_slow$worker_id),]$profession <- "Professional"
df_consent_slow[is.na(df_consent_slow$worker_id),]$test_duration <- 0.5
df_consent_slow[is.na(df_consent_slow$worker_id),]$adjusted_score <- 0
df_consent_slow[is.na(df_consent_slow$worker_id),]$is_fast <- FALSE

max <- minmax(df_consent)
min <- maxmin(df_consent)
df_support <- df_consent[df_consent$test_duration<=max & 
                           df_consent$test_duration>=min,]

ggplot(df_support, aes(x=test_duration, y=adjusted_score)) + geom_point(aes(colour = profession))+
  stat_smooth(method = 'lm', formula = y ~ x, aes(colour = profession), se= FALSE)+
  theme_minimal()+
  theme(
    legend.position="top",
    legend.justification = "left",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 10),
    plot.title = element_text(size=12),
    axis.text.x = element_text(angle = 0, hjust = 1, size=10)
  ) +
  ylab("Score (Adjusted)") +
  xlab("Test Duration (minutes)") +
  ggtitle("All E2: Duration impact on Score by Profession")


#-----------
#FAST GROUPS

max <- minmax(df_consent_fast)
min <- maxmin(df_consent_fast)
df_support_fast <- df_consent_fast[df_consent_fast$test_duration<=max & 
                                df_consent_fast$test_duration>=min,]


ggplot(df_support_fast, aes(x=test_duration, y=adjusted_score)) + geom_point(aes(colour = profession))+
  stat_smooth(method = 'lm', formula = y ~ x, aes(colour = profession), se= FALSE)+
theme_minimal()+
  theme(
    legend.position="top",
    legend.justification = "left",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 10),
    plot.title = element_text(size=12),
    axis.text.x = element_text(angle = 0, hjust = 1, size=10)
  ) +
  ylab("Score (Adjusted)") +
  xlab("Test Duration (minutes)") +
  ggtitle("Fast E2: Duration impact on Score by Profession")

#------------
# SLOW GROUPS

max <- minmax(df_consent_slow)
min <- maxmin(df_consent_slow)
df_support_slow <- df_consent_slow[df_consent_slow$test_duration<=max & 
                                df_consent_slow$test_duration>=min,]


ggplot(df_support_slow, aes(x=test_duration, y=adjusted_score)) + geom_point(aes(colour = profession))+
  stat_smooth(method = 'lm', formula = y ~ x, aes(colour = profession), se= FALSE)+
  theme_minimal()+
  theme(
    legend.position="top",
    legend.justification = "left",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 10),
    plot.title = element_text(size=12),
    axis.text.x = element_text(angle = 0, hjust = 1, size=10)
  ) +
  ylab("Score (Adjusted)") +
  xlab("Test Duration (minutes)") +
  ggtitle("Slow E2: Duration impact on Score by Profession")


#-----------------------------------------------------------
#-----------------------------------------------------------
#Evaluate how fast and slow can explain adjusted_score score

df_consent <- df_consent[!is.na(df_consent$adjusted_score),]
df_consent_fast <- df_consent[df_consent$is_fast,]
df_consent_slow <- df_consent[!df_consent$is_fast,]

#Compute proportion by profession, because professions have distinct testDuration averages
profession_list <- as.character(unique(df_consent$profession))

df_coeff <- data.frame(matrix(data=NA, nrow=18, ncol=5))
colnames(df_coeff) <- c("profession","group","coefficient","p_value","adj_r_squared");

i <- 1
for(profes in profession_list){
  #AGGREGATED
  #profes <- "Professional"
  print(profes)
  model_all <-  lm(formula = adjusted_score ~ test_duration, 
                   data=df_consent_fast[df_consent_fast$profession==profes,] )
  df_coeff$profession[i] <- profes
  df_coeff$group[i] <- "ALL"
  df_coeff$coefficient[i] <- model_all$coefficients[[2]]
  df_coeff$p_value[i] <-  summary(model_all)$coefficients[2,4]
  df_coeff$adj_r_squared[i] <- summary(model_all)$adj.r.squared
  i <- i+1
  
  #FAST RESPONDERS
  model_fast <- lm(formula = adjusted_score ~ test_duration, 
                   data=df_consent_fast[df_consent_fast$profession==profes,] )
  df_coeff$profession[i] <- profes
  df_coeff$group[i] <- "FAST"
  df_coeff$coefficient[i] <- model_fast$coefficients[[2]]
  df_coeff$p_value[i] <- summary(model_fast)$coefficients[2,4]
  df_coeff$adj_r_squared[i] <- summary(model_fast)$adj.r.squared
  i <- i+1
  
  #SLOW RESPONDERS
  model_slow <- lm(formula = adjusted_score ~ test_duration, 
                   data=df_consent_slow[df_consent_slow$profession==profes,] )
  df_coeff$profession[i] <- profes
  df_coeff$group[i] <- "SLOW"
  df_coeff$coefficient[i] <- model_slow$coefficients[[2]]
  df_coeff$p_value[i] <-summary(model_slow)$coefficients[2,4]
  df_coeff$adj_r_squared[i] <- summary(model_slow)$adj.r.squared
  i <- i+1
}

df_coeff

"
Model 4 says that duration positively impacts score across all professions. However, this is not
true when we look at individual groups discovered by the mixture models.
Looking at Model 2 and Model 3, testDuration has a negative impact for slow group
in the following professsions: Undergrad, Other, Grad. F
Except for Programmer, duration has positive impact on score for the fast group across all other professions.
Programmers have the reverse. The longer the slow group took better the score, whereas the fast group was the opposite.

In conclusion, group membership within duration is a confounder for certain professions, but not others.

              profession group coefficient      p_value adj_r_squared
1               Hobbyist   ALL  0.03013050 7.189049e-01  -0.003623666
2               Hobbyist  FAST  0.03013050 7.189049e-01  -0.003623666
3               Hobbyist  SLOW  0.12698675 1.564844e-03   0.036899962
4  Undergraduate_Student   ALL  0.44329222 1.458968e-08   0.110571498
5  Undergraduate_Student  FAST  0.44329222 1.458968e-08   0.110571498
6  Undergraduate_Student  SLOW  0.02553075 5.754876e-01  -0.003953615
7           Professional   ALL  0.14767861 5.081121e-02   0.011797886
8           Professional  FAST  0.14767861 5.081121e-02   0.011797886
9           Professional  SLOW  0.11377909 4.111188e-03   0.040638900
10      Graduate_Student   ALL  0.30346900 6.380695e-03   0.045159744
11      Graduate_Student  FAST  0.30346900 6.380695e-03   0.045159744
12      Graduate_Student  SLOW  0.04077328 4.571889e-01  -0.003182469
13                 Other   ALL  0.07197411 5.648487e-01  -0.012229633
14                 Other  FAST  0.07197411 5.648487e-01  -0.012229633
15                 Other  SLOW  0.03305288 6.113321e-01  -0.013614300
16            Programmer   ALL  0.28187339 2.892559e-01   0.007311350
17            Programmer  FAST  0.28187339 2.892559e-01   0.007311350
18            Programmer  SLOW -0.06973515 2.892590e-01   0.007734466

\begin{center}
\begin{tabular}{ c | c | c | c | c}
             profession &group& coefficient& p_value&  adj_r_squared\\
             \hline \hline
               Hobbyist &ALL&    0.36& *&    0.08\\
               \hline
               Hobbyist &FAST&   0.15&  *&    0.01\\
               \hline
               Hobbyist &SLOW&   0.11& **&    0.04\\
               \hline
  Undergraduate_Student &ALL&    0.36& **&    0.08\\
  \hline
  Undergraduate_Student &FAST&   0.15&  *&    0.01\\
  \hline
  Undergraduate_Student &SLOW&   0.11& **&    0.04\\
  \hline
           Professional &ALL&    0.36& **&    0.08\\
           \hline
           Professional &FAST&   0.15&  *&    0.01\\
           \hline
           Professional &SLOW&   0.11& **&    0.04\\
           \hline
      Graduate_Student  &ALL&    0.36& **&    0.08\\
      \hline
      Graduate_Student  &FAST&   0.15&  *&    0.01\\
      \hline
      Graduate_Student  &SLOW&   0.11& **&    0.04\\
      \hline
                 Other  & ALL&   0.36& **&    0.08\\
                 \hline
                 Other  &FAST&   0.15&  *&    0.01\\
                 \hline
                 Other  &SLOW&   0.11& **&    0.04\\
                 \hline
            Programmer  & ALL&   0.36& **&    0.08\\
            \hline
            Programmer  &FAST&   0.15&  *&    0.01\\
            \hline
            Programmer  &SLOW&   0.11& **&    0.04\\[0.5ex]
            \hline
\end{tabular}
\end{center}

*=p-value>0.05, **=p-value<0.05
" 

#-----------------------------------------------------------
#CORRELATIONS
#Evaluate how fast and slow can explain adjusted_score score

df_consent <- df_consent[!is.na(df_consent$adjusted_score),]
df_consent_fast <- df_consent[df_consent$is_fast,]
df_consent_slow <- df_consent[!df_consent$is_fast,]

#Compute proportion by profession, because professions have distinct testDuration averages
profession_list <- as.character(unique(df_consent$profession))

df_corr <- data.frame(matrix(data=NA, nrow=18, ncol=4))
colnames(df_corr) <- c("profession","group","tau","p_value");

i <- 1
for(profes in profession_list){
  #AGGREGATED
#  profes="Professional"
  data=df_consent[df_consent$profession==profes,] 
  model<-  cor.test(y=data$adjusted_score, x=data$test_duration, method = c("kendall"))
  df_corr$profession[i] <- profes
  df_corr$group[i] <- "ALL"
  df_corr$tau[i] <- model$estimate
  df_corr$p_value[i] <-  model$p.value
  i <- i+1
  
  #FAST RESPONDERS
  data=df_consent_fast[df_consent_fast$profession==profes,] 
  model <-  cor.test(y=data$adjusted_score, x=data$test_duration, method = c("kendall"))
  df_corr$profession[i] <- profes
  df_corr$group[i] <- "FAST"
  df_corr$tau[i] <- model$estimate
  df_corr$p_value[i] <-  model$p.value
  i <- i+1
  
  #SLOW RESPONDERS
  data=df_consent_slow[df_consent_slow$profession==profes,] 
  model <-  cor.test(y=data$adjusted_score, x=data$test_duration, method = c("kendall"))
  df_corr$profession[i] <- profes
  df_corr$group[i] <- "SLOW"
  df_corr$tau[i] <- model$estimate
  df_corr$p_value[i] <-  model$p.value
  i <- i+1
}

df_corr

"
             profession group         tau      p_value
1               Hobbyist   ALL  0.20407881 1.001840e-10
2               Hobbyist  FAST  0.03023272 4.994611e-01 <<<<<< SLOW==Clueless
3               Hobbyist  SLOW  0.12415854 6.065339e-03 <<<<<< SLOW==Thorough
4  Undergraduate_Student   ALL  0.15420591 2.930439e-06 
5  Undergraduate_Student  FAST  0.21638641 4.610126e-07
6  Undergraduate_Student  SLOW  0.03557003 4.972826e-01 <<<<<< SLOW==Clueless
7           Professional   ALL  0.14398747 2.132846e-05
8           Professional  FAST  0.09218619 3.997745e-02
9           Professional  SLOW  0.12096225 2.098754e-02 <<<<<< SLOW==Thorough
10      Graduate_Student   ALL  0.21342898 2.893049e-07
11      Graduate_Student  FAST  0.15560948 8.567291e-03
12      Graduate_Student  SLOW  0.03294088 5.831662e-01 <<<<<< SLOW==Clueless
13                 Other   ALL  0.14638526 2.719661e-02
14                 Other  FAST  0.07077811 4.580189e-01
15                 Other  SLOW  0.05711839 5.497057e-01 
16            Programmer   ALL  0.03464737 7.347806e-01
17            Programmer  FAST  0.18137083 2.194741e-01
18            Programmer  SLOW -0.28296487 6.489748e-02 <<<<<< SLOW==Clueless 


\paragraph{Analsysi} - For Hobbyists and Professionals, SLOW groups have stronger correlation with score, 
which means that slower responders are being more thorough than faster ones.
Conversely, the reverse happens with students, whose slower responders do not do 
better with more time than the faster ones. This might imply that the fast ones 
are too fast, so any add in time improves their score a lot. 
Interstingly programmers get worse with more time, which is the opposite of
being thorough, i.e., their extra time is sign of being clueless. 
The Others group show not distinction between fast and slow responders, which
could be also a sign of being clueless. 

\paragraph{Implications to design} - The groups who show signs of being clueless
could benefit from interventions like given them hints, a simpler task or 
being able to ask for help. Meanwhile, the groups who show thoroughness when 
being slow responders, could be incentivized to give a second look at their
answers if they are responding too fast.
"

