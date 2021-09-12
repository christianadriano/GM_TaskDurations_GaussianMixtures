"
Gaussian Mixture Model for the test_duration in experiment E1

Use the mixture model to categorize programmers into fast and slow groups 
w.r.t. test_duration

Analyze if students and non-students present the same correlations between
adjusted_score and test_duration

TODO:
- Fix this df_consent <- distinct(df_consent) in load_consent_create_indexex_E1
- Rerun plots for OTHERS
- Rerun correlations for trimmed data for OTHERS


"

#-----------------------------------------------------------------------------------
#scripts to run EM algorithm
source("C://Users//Christian//Documents//GitHub//EM_GaussianMixtureModel_TaskDurations//util//main_bivariate_EM.R")
source("C://Users//Christian//Documents//GitHub//EM_GaussianMixtureModel_TaskDurations//util//prior_kmeans_EM.R")
source("C://Users//Christian//Documents//GitHub//EM_GaussianMixtureModel_TaskDurations//util//visualize_compute_membership.R")

#---------------------------------------------------------
#Load data from demographics and qualification test Experiment-1
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data_loaders//load_consent_create_indexes_E1.R")
df_consent <- load_consent_create_indexes(load_is_student=1)
df_consent <- distinct(df_consent) #remove duplicated lines
dim(df_consent)

#----------------------------------
#ALL
#Run for entire data set together
df_consent$testDuration_fastMembership <- NA
df_prior <- prior.df(wait = df_consent$test_duration)
m.step <- main(wait = df_consent$test_duration, wait.summary.df=df_prior)
df_consent <- compute_Memberships(m.step, df_consent)
plot <- plot_mixture_models(df_consent$test_duration,m.step,"All E1")
plot
#---------------------------------------------------------
#INVESTIGATE OUTCOMES
cor.test(df_consent$adjusted_score,df_consent$test_duration,
         alternative = "two.sided", 
         method="kendall")
#correlation = 0.09240252   

cor.test(df_consent$adjusted_score,df_consent$testDuration_slowMembership,
         alternative = "two.sided", 
         method="kendall")
#correlation = 0.09240252     

cor.test(df_consent$adjusted_score,df_consent$testDuration_fastMembership,
         alternative = "two.sided", 
         method="kendall")
#correlation = 0.09240737 (no correlation <0.15)    

"
No correlation of adjusted_score and membership to speed of response is 
no, 0.09, which is the same as test_duration.

@article{kendall1948rank,
  title={Rank correlation methods.},
  author={Kendall, Maurice George},
  year={1948},
  publisher={Griffin},
 url={https://archive.org/details/rankcorrelationm0000kend}
}


"

#-----------------------------------------------------
#REGRESSION MODELS

#Interaction Model duration*membership
model_1_fast <- lm(formula = adjusted_score ~ test_duration + testDuration_fastMembership + test_duration*testDuration_fastMembership, data=df_consent )
model_1_slow <- lm(formula = adjusted_score ~ test_duration + testDuration_slowMembership + test_duration*testDuration_slowMembership, data=df_consent )
summary(model_1_fast)
summary(model_1_slow)
"
Results. Does not matter if I use fast or slow in the regression
P-values for the coefficients >0.05. Adjusted R-squared is small 0.053
slow: test_duration coef=0.51, slowMembership=0.94, interaction=-1.16
"
#------------------
#No Interaction
model_2_fast <- lm(formula = adjusted_score ~ test_duration + testDuration_fastMembership, data=df_consent )
model_2_slow <- lm(formula = adjusted_score ~ test_duration + testDuration_slowMembership, data=df_consent )
summary(model_2_fast)
summary(model_2_slow)
"
Only test_duration coefficient =0.06 an is significant (p-value<0.05),
whereas slowMembership is not significant 
Adjusted R-squared is smaller than in the interaction model 0.035 

Membership: Coefficients(testDuration_minutes, testDuration_fastMembership)
Fast: (0.066407,0.076484)
Slow: (0.066407,-0.076484)

The size of the coefficients show that membership and test_duration 
only become relevant in the interaction model. 
This hints that membership has more information about score than the duration
alone.
"

#-----------------------------
# Separate features

model_3 <- lm(formula = adjusted_score ~ test_duration, data=df_consent )
model_4 <- lm(formula = adjusted_score ~ testDuration_fastMembership, data=df_consent )
model_5 <- lm(formula = adjusted_score ~ testDuration_slowMembership, data=df_consent )
summary(model_3)
summary(model_4)
summary(model_5)

# Coefficient (value,p-value)
# test_duration (0.067871,<0.05)
# fast_membership (-1.01054,<0.05)
# slow_membership (1.01054,<0.05)

"
This demonstrates that membership carries more information about the score
than the test_duration.
"

#--------------------------------------
# HARD CLUSTERING

df_consent$is_fast <- NA
df_consent[df_consent$testDuration_fastMembership<df_consent$testDuration_slowMembership,]$is_fast <- FALSE
df_consent[df_consent$testDuration_fastMembership>=df_consent$testDuration_slowMembership,]$is_fast <- TRUE
df_consent <- df_consent[!is.na(df_consent$adjusted_score),]
df_consent <- df_consent[!is.na(df_consent$test_duration),]
df_consent <- df_consent[!is.na(df_consent$is_fast),]

df_consent_slow <- df_consent[!df_consent$is_fast,]
df_consent_fast <- df_consent[df_consent$is_fast,]

#summary(df_consent_slow$test_duration)
#summary(df_consent_fast$test_duration)


#ALL
model_2_fast <- lm(formula = adjusted_score ~ test_duration , data=df_consent_fast )
model_2_slow <- lm(formula = adjusted_score ~ test_duration , data=df_consent_slow )
summary(model_2_fast)
summary(model_2_slow)

#(filter,fast,slow)
#(all-aggregated,-0.21653, 0.051223) 
"
Slow people the longer they spend there is a slight improvement only (so they look more clueless)
Fast people, the longer they spend, lower their score (so their first guess will not improve with more data)
" 
cor.test(df_consent_fast$test_duration,df_consent_fast$adjusted_score, method=c("kendall"))
#FAST z = -1.9102, p-value = 0.05611, tau = -0.03500714
cor.test(df_consent_slow$test_duration,df_consent_slow$adjusted_score, method=c("kendall"))
#SLOW 8.6626, p-value < 2.2e-16, tau = 0.1344702 -> no correlation 

"
Looking at the aggregated data, there is no correlation between test_duration and adjusted score.
The aggregated data is being confounded by the is_fast covariate cluster, hence
hidding the association between adjusted_score and test_duration, i.e., Simpsom's paradox.
"

#NON-STUDENTS
model_2_fast <- lm(formula = adjusted_score ~ test_duration, data=df_consent_fast[df_consent_fast$is_student=="0",] )
model_2_slow <- lm(formula = adjusted_score ~ test_duration, data=df_consent_slow[df_consent_slow$is_student=="0",] )
summary(model_2_fast)
summary(model_2_slow)
#(filter,fast,slow)
#(non-students,-0.4283,0.06697) 

"
Fast non-students, the longer they spend, lower their score, showing that this group would not benefit from more time.
Slow non-students the longer they spend, not significant improvement on score, so their are more clueless than thorough
"

cor.test(df_consent_fast[df_consent_fast$is_student=="0",]$test_duration,df_consent_fast[df_consent_fast$is_student=="0",]$adjusted_score, method=c("kendall"))
#FAST z = -1.3791, p-value = 0.1679, tau = -0.0806432 -> no correlation
cor.test(df_consent_slow[df_consent_slow$is_student=="0",]$test_duration,df_consent_slow[df_consent_slow$is_student=="0",]$adjusted_score, method=c("kendall"))
#SLOW z = 5.5661, p-value = 2.606e-08, tau = 0.2531211 -> medium strength \cite{} 

"
Fast non-students, the longer they spend, there is no relevant effect on their score, showing that this group would not benefit from more time.
Slow non-students the longer they spend, there is medium strength improvement on score, so their are more clueless than thorough
"
t.test(df_consent_fast[df_consent_fast$is_student=="0",]$adjusted_score,
       df_consent_slow[df_consent_slow$is_student=="0",]$adjusted_score)

# t = 3.165, df = 348.91, p-value = 0.001687
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.1328920 0.5691434
# sample estimates:
#   mean of x mean of y 
# 2.826320  2.475303 

"fast non-students have higher score than slow ones"

#-------------------

#STUDENTS
model_2_fast <- lm(formula = adjusted_score ~ test_duration, data=df_consent_fast[df_consent_fast$is_student=="1",] )
model_2_slow <- lm(formula = adjusted_score ~ test_duration, data=df_consent_slow[df_consent_slow$is_student=="1",] )
summary(model_2_fast)
summary(model_2_slow)
#(filter,fast,slow)
#(students,-0.9643,0.23791)

"
Fast students, the longer they spend, lower their score, showing that this group would not benefit from more time.
Slow students the longer they spend, higher score, so they are being thorough.
" 
cor.test(df_consent_fast[df_consent_fast$is_student=="1",]$test_duration,df_consent_fast[df_consent_fast$is_student=="1",]$adjusted_score, method=c("kendall"))
#FAST z = -1.1155, p-value = 0.2646, tau = -0.1619848 -> medium correlation
cor.test(df_consent_slow[df_consent_slow$is_student=="1",]$test_duration,df_consent_slow[df_consent_slow$is_student=="1",]$adjusted_score, method=c("kendall"))
#SLOW z = 4.5492, p-value = 5.386e-06, tau = 0.4774625 -> strong correlation

#In cor.test.default(df_consent_fast[df_consent_fast$is_student ==  :
#Cannot compute exact p-value with ties

"
Fast students, the longer they spend, lower their score, showing that this group would not benefit from more time, i.e., clueless.
Slow students the longer they spend, higher score, so they are being thorough.
" 
t.test(df_consent_fast[df_consent_fast$is_student=="1",]$adjusted_score,
       df_consent_slow[df_consent_slow$is_student=="1",]$adjusted_score)
"t = -2.1493, df = 62.05, p-value = 0.03552
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
  -1.03729658 -0.03760625
sample estimates:
  mean of x mean of y 
2.234947  2.772399 

Corroborating the clueless vs thorough interpretation, the fast students present slower
mean score of fast vs thorough of slow, a t-test show statistically 
significant difference (p-value=0.036) between the score of fast students (2.23) 
and the slower students (2.77).
"

#-------------------

#OTHERS
model_2_fast <- lm(formula = adjusted_score ~ test_duration, data=df_consent_fast[is.na(df_consent_fast$is_student),] )
model_2_slow <- lm(formula = adjusted_score ~ test_duration, data=df_consent_slow[is.na(df_consent_slow$is_student),] )
summary(model_2_fast)
summary(model_2_slow)
#(filter,fast,slow)
#(others,0.037149,-0.07966)

"
For Fast and Slow others, there was little effect of duraton on score
" 
cor.test(df_consent_fast[is.na(df_consent_fast$is_student),]$test_duration,df_consent_fast[is.na(df_consent_fast$is_student),]$adjusted_score, method=c("kendall"))
#FAST z = 6.1327, p-value = 8.641e-10, tau = 0.1045339 -> no correlation
cor.test(df_consent_slow[is.na(df_consent_slow$is_student),]$test_duration,df_consent_slow[is.na(df_consent_slow$is_student),]$adjusted_score, method=c("kendall"))
#SLOW z = -0.31522, p-value = 0.7526, tau = -0.006245893 -> no correlation

#In cor.test.default(df_consent_fast[df_consent_fast$is_student ==  :
#Cannot compute exact p-value with ties

"
no correlation between test_duration and adjusted_score for others.
" 
t.test(df_consent_fast[is.na(df_consent_fast$is_student),]$adjusted_score,
       df_consent_slow[is.na(df_consent_slow$is_student),]$adjusted_score)

"t = 2.1652, df = 3054.9, p-value = 0.03045
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
  0.007753662 0.156480458
sample estimates:
  mean of x mean of y 
0.9155045 0.8333874

Fast others higher score than slower ones.

"

#-------------------

df_consent %>% 
  group_by(is_student,is_fast) %>% 
  summarize(count = n())

#       is_student is_fast count  %
#           <int>  <lgl>   <int> <int>
# 1          0     FALSE       253   62%
# 2          0     TRUE        153   38%
#                              406
# 3          1     FALSE        49   64%
# 4          1     TRUE         28   36%
#                               77
# 5         NA     FALSE      1853   58%
# 6         NA     TRUE       1360   42%
#                             3213
#
# TOTAL                      3696

"
Compared with the clusters computed over all participants, 
the proportion of is_fast for non_student changed radically.
This shows the importance of computing this clustering by group. 

Among all professions, the proportions of slow and fast are similar
Non-Students:  Slow 62%, Fast 38%
Students: Slow 64%, Fast 36%
Others: Slow 58%, Fast 42%

This homogeneous proportions across groups is very surprising, 
because Fast and Slow only considered test_duration,
whereas student and non-student only considered age and years_experience
"

#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
# REDO MEMBERSHIP BY GROUP
#
#Now compute the membership by each student, non-student, and others
#because students and non-students have different mean values for test_duration.

#Load data from demographics and qualification test Experiment-1
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data_loaders//load_consent_create_indexes_E1.R")
df_consent <- load_consent_create_indexes(load_is_student=1)
df_consent <- distinct(df_consent)
df_consent$is_student <- as.factor(df_consent$is_student)

#remove rows without is_student label
#df_consent <- df_consent[complete.cases(df_consent$is_student),];
df_consent$testDuration_fastMembership <- NA;

df_consent$profession <- "non-student"
df_consent[df_consent$is_student=="1" & !is.na(df_consent$is_student),"profession"] <- "student"
df_consent[is.na(df_consent$is_student),]$profession <- "other"

#------------
#STUDENTS 
choice <- "student";
df_selected <- df_consent[df_consent$profession==choice,]
#Remove outlier at 14 min
row_to_keep = which(df_selected$test_duration<14)
df_selected <- df_selected[row_to_keep,]
summary(df_selected$test_duration)
barplot(df_selected$test_duration)

#build the EM model
df_prior <- prior.df(wait = df_selected$test_duration)
m.step <- main(wait = df_selected$test_duration, wait.summary.df=df_prior)
df_selected <- compute_Memberships(m.step,df_selected) 
df_consent$testDuration_fastMembership[which(df_consent$worker_id %in% df_selected$worker_id
                                     & 
                                        df_consent$profession %in% df_selected$profession)] <- df_selected$testDuration_fastMembership

#Attribute SLOW for the outlier
df_consent[df_consent$test_duration>14 &df_consent$profession=="student",]$testDuration_fastMembership <- 1.0000 

#plot model for the profession
plot <- plot_mixture_models(df_consent[df_consent$profession=="student",]$test_duration,m.step,"Students E1")

plot

#-------------
#NON-STUDENTS
choice <- "non-student";

df_selected <- df_consent[df_consent$profession==choice,]
summary(df_selected$test_duration)
barplot(df_selected$test_duration)

df_prior <- prior.df(wait = df_selected$test_duration)
m.step <- main(wait = df_selected$test_duration, wait.summary.df=df_prior)
df_selected <- compute_Memberships(m.step,df_selected) 
df_consent$testDuration_fastMembership[which(df_consent$worker_id %in% df_selected$worker_id
                                             & 
                                               df_consent$profession %in% df_selected$profession)] <- df_selected$testDuration_fastMembership

#plot model for the profession
plot <- plot_mixture_models(df_consent[df_consent$profession==choice,]$test_duration,m.step,"Non-Students E1")
plot


#-------------
#OTHER
choice <- "other";

df_selected <- df_consent[df_consent$profession==choice,]
summary(df_selected$test_duration)
barplot(df_selected$test_duration)

df_prior <- prior.df(wait = df_selected$test_duration)
m.step <- main(wait = df_selected$test_duration, wait.summary.df=df_prior)
df_selected <- compute_Memberships(m.step,df_selected) 
df_consent$testDuration_fastMembership[which(df_consent$worker_id %in% df_selected$worker_id
                                             & 
                                               df_consent$profession %in% df_selected$profession)] <- df_selected$testDuration_fastMembership

#plot model for the profession
plot <- plot_mixture_models(df_consent[df_consent$profession==choice,]$test_duration,m.step,"Others E1")
plot

#-----------------------------------------------------------
# WRITE FILE

#Save df_consent to file so we can retrieve the tesDuration_fast
path = "C://Users//Christian//Documents//GitHub//EM_GaussianMixtureModel_TaskDurations//output//"
write.csv(df_consent,paste0(path,"E1_consent_with_testDuration_fastMembership.csv"));

#-----------------------------------------------------------
#Evaluate how fast and slow can explain adjusted_score score
df_consent_fast <- df_consent[df_consent$is_fast,]
df_consent_slow <- df_consent[!df_consent$is_fast,]

model_2_fast <- lm(formula = adjusted_score ~ test_duration , data=df_consent_fast )
model_2_slow <- lm(formula = adjusted_score ~ test_duration , data=df_consent_slow )
summary(model_2_fast)
summary(model_2_slow)

#(filter,fast,slow)
#(all-aggregated, 0.06720, -0.06819) 
"
Fast people the longer they spend, higher score
Slow people, the longer they spend, lower their score.
" 

#------------
#NON-STUDENTS
model_2_fast <- lm(formula = adjusted_score ~ test_duration, data=df_consent_fast[df_consent_fast$is_student==0,] )
model_2_slow <- lm(formula = adjusted_score ~ test_duration, data=df_consent_slow[df_consent_slow$is_student==0,] )
summary(model_2_fast)
summary(model_2_slow)
#(filter,fast,slow)
#(non-students,0.06522,-0.09303) 

"
Fast non-students, very weak correlation
Slow non-students, the more they spent the worse they got
"

#--------
#STUDENTS

model_2_fast <- lm(formula = adjusted_score ~ test_duration, data=df_consent_fast[df_consent_fast$is_student==1,] )
model_2_slow <- lm(formula = adjusted_score ~ test_duration, data=df_consent_slow[df_consent_slow$is_student==1,] )
summary(model_2_fast)
summary(model_2_slow)
#(filter,fast,slow)
#(students,0.18567,0.0760) 

"
Students have a different behavior. 
Fast students the longer they spend, much higher score
Slow students, the longer they spend, a bit higher their score. (not much gain for slow students)
" 
"In conclusion, group membership within duration is a confounder for certain professions, 
but not others.
" 
#---------------
#PLOTS to show this phenomenon

#---
#ALL
ggplot(df_consent, aes(x=test_duration, y=adjusted_score)) + 
  geom_point(aes(colour = group))+
  stat_smooth(method = 'lm', formula = y ~ x, aes(colour = group), se= FALSE)+
  theme_minimal()+
  theme(
    legend.position="top",
    legend.justification = "left",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 12),
    plot.title = element_text(size=14),
    axis.text.x = element_text(angle = 0, hjust = 1, size=12)
  ) +
  ylab("Adjusted score (adjusted_score)") +
  xlab("Test Duration (minutes)") +
  ggtitle("All: Test Duration x Test Score by Student Status")

#----
#FAST
ggplot(df_consent_fast, aes(x=test_duration, y=adjusted_score)) + 
  geom_point(aes(colour = group))+
  stat_smooth(method = 'lm', formula = y ~ x, aes(colour = group), se= FALSE)+
theme_minimal()+
  theme(
    legend.position="top",
    legend.justification = "left",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 12),
    plot.title = element_text(size=14),
    axis.text.x = element_text(angle = 0, hjust = 1, size=12)
  ) +
  ylim(min(df_consent_fast$adjusted_score),max(df_consent_fast$adjusted_score)+1)+
  ylab("Adjusted score (adjusted_score)") +
  xlab("Test Duration (minutes)") +
  ggtitle("Fast: Test Duration x Test Score by Student Status")

#----
#SLOW
ggplot(df_consent_slow, aes(x=test_duration, y=adjusted_score)) + 
  geom_point(aes(colour = group))+
  stat_smooth(method = 'lm', formula = y ~ x, aes(colour = group), se= FALSE)+
  theme_minimal()+
  theme(
    legend.position="top",
    legend.justification = "left",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 12),
    plot.title = element_text(size=14),
    axis.text.x = element_text(angle = 0, hjust = 1, size=12)
  ) +
  #xlim(0,)+
  ylim(min(df_consent_slow$adjusted_score),max(df_consent_slow$adjusted_score)+1)+
  ylab("Adjusted score (adjusted_score)") +
  xlab("Test Duration (minutes)") +
  ggtitle("Slow: Test Duration X Test Score by Student Status")

#---------------------------
students <- df_consent[df_consent$is_student=="1",]$adjusted_score
non_students <- df_consent[df_consent$is_student=="0",]$adjusted_score
t.test(students,non_students)

# Welch Two Sample t-test
# 
# data:  students and non_students
# t = -0.84386, df = 107.46, p-value = 0.4006
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.3921039  0.1579474
# sample estimates:
#   mean of x mean of y 
# 2.576962  2.694040  

"
Diference NOT statistically significant
So, fast non-students are probably as bad as the fast students.
"
#-----------------------------
# Checking only the slow group
students <- df_consent_slow[df_consent_slow$is_student=="1",]$adjusted_score
non_students <- df_consent_slow[df_consent_slow$is_student=="0",]$adjusted_score
t.test(students,non_students)
# Welch Two Sample t-test
# 
# data:  students and non_students
# t = -1.3316, df = 72.793, p-value = 0.1871
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.5600449  0.1114219
# sample estimates:
#   mean of x mean of y 
# 2.265884  2.490195 
"
However, their average scores are NOT statistically significant distinct!
So, slow non-students are probably as bad as the slow students.
"

#------------------------------------------------------------
#TRIMMING TO MATCH SUPPORT OF DISTRIBUTIONS OF TEST DURATION
#What if I cut-off at the max range of students?
#Does the difference in correlation persist?
#Bingo! The relationship reverses. Now, both are positive, 
#the longer they spend, higher the accuracy.

max(df_consent[df_consent$is_student=="1",]$test_duration)
#>[1] 14.98905
max(df_consent[df_consent$is_student=="0",]$test_duration)
#>[1] 32.91568
#However, 14.98 is an outlier, so we trimme at 8 

df_trimmed_ALL <- df_consent[df_consent$test_duration<=8,]


#ALL
ggplot(df_trimmed_ALL, aes(x=test_duration, y=adjusted_score)) + 
  geom_point(aes(colour = group))+
  stat_smooth(method = 'lm', formula = y ~ x, aes(colour = group), se= FALSE)+
  theme_minimal()+
  theme(
    legend.position="top",
    legend.justification = "left",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 12),
    plot.title = element_text(size=14),
    axis.text.x = element_text(angle = 0, hjust = 1, size=12)
  ) +
  ylab("Adjusted score (adjusted_score)") +
  xlab("Test Duration (minutes)") +
  ggtitle("All (Trimmed): Test Duration x Test Score by Student Status")

#-------------------
#SLOW

df_cut <- df_consent_slow[df_consent_slow$test_duration<=2,]

ggplot(df_cut, aes(x=test_duration, y=adjusted_score)) + 
  geom_point(aes(colour = group))+
  stat_smooth(method = 'lm', formula = y ~ x, aes(colour = group), se= FALSE)+
  theme_minimal()+
  theme(
    legend.position="top",
    legend.justification = "left",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 12),
    plot.title = element_text(size=14),
    axis.text.x = element_text(angle = 0, hjust = 1, size=12)
  ) +
  ylab("Adjusted score (adjusted_score)") +
  xlab("Test Duration (minutes)") +
  ggtitle("Slow (Trimmed): Test Duration X Test Score by Student Status")


"
Shows that people who are students, but took the same time as the slow non-students,
are similar to the slow non-students. Similar w.r.t. the longer they took the worse
their score got. So, we would need to distinguish between these two groups of 
students.
"

#------------
#FAST

df_trimmed_FAST <- df_consent_fast[df_consent_fast$test_duration<=16,]

ggplot(df_trimmed_FAST, aes(x=test_duration, y=adjusted_score)) + 
  geom_point(aes(colour = group))+
  stat_smooth(method = 'lm', formula = y ~ x, aes(colour = group), se= FALSE)+
  theme_minimal()+
  theme(
    legend.position="top",
    legend.justification = "left",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 12),
    plot.title = element_text(size=14),
    axis.text.x = element_text(angle = 0, hjust = 1, size=12)
  ) +
  ylab("Adjusted score (adjusted_score)") +
  xlab("Test Duration (minutes)") +
  ggtitle("Fast (Trimmed): Test Duration X Test Score by Student Status")


"
After trimming, there was not change in the signal of the correlation 
between duration and score for fast people
"

#REMOVE OUTLIER STUDENT WITH 14min, SO TRIM AT 8min
df_trimmed_FAST <- df_consent_fast[df_consent_fast$test_duration<=8,]

ggplot(df_trimmed_FAST, aes(x=test_duration, y=adjusted_score)) + 
  geom_point(aes(colour = group))+
  stat_smooth(method = 'lm', formula = y ~ x, aes(colour = group), se= FALSE)+
  theme_minimal()+
  theme(
    legend.position="top",
    legend.justification = "left",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 12),
    plot.title = element_text(size=14),
    axis.text.x = element_text(angle = 0, hjust = 1, size=12)
  ) +
  ylab("Adjusted score (adjusted_score)") +
  xlab("Test Duration (minutes)") +
  ggtitle("Fast (Trimmed outlier): Test Duration X Test Score by Student Status")


"
The difference in slopes became larger.
Trimming at 16min, slope-student ~ 1/3, slope-nonStudent ~ 1/5, so rate of 1.67
Whereas trimming at 8min slope-student ~ 1/2, slope-nonStudent ~ 1/4, so rate of 2
Which corresponds to a relative change in slopes of 20%, i.e., (2-1.67)/1.67 = 0.197 ~ 20%

"

#-----------------------------------------------------------
#CORRELATIONS
#Evaluate how fast and slow can explain adjusted_score score

#-----------------------------
# Checking only the slow group

df_consent <- df_consent[!is.na(df_consent$adjusted_score),]
df_consent_fast <- df_consent[df_consent$is_fast,]
df_consent_slow <- df_consent[!df_consent$is_fast,]

df_corr <- data.frame(matrix(data=NA, nrow=6, ncol=4))
colnames(df_corr) <- c("is_student","group","tau","p_value");

i <- 1
for(student in c("0","1")){
  if(student=="1") 
    profes <- "student"
  else
    profes <- "non-student"
  
  #AGGREGATED
  #  profes="Professional"
  data=df_consent[df_consent$is_student==student,] 
  model<-  cor.test(y=data$adjusted_score, x=data$test_duration, method = c("kendall"))
  df_corr$is_student[i] <- profes
  df_corr$group[i] <- "ALL"
  df_corr$tau[i] <- model$estimate
  df_corr$p_value[i] <-  model$p.value
  i <- i+1
  
  #FAST RESPONDERS
  data=df_consent_fast[df_consent_fast$is_student==student,] 
  model <-  cor.test(y=data$adjusted_score, x=data$test_duration, method = c("kendall"))
  df_corr$is_student[i] <- profes
  df_corr$group[i] <- "FAST"
  df_corr$tau[i] <- model$estimate
  df_corr$p_value[i] <-  model$p.value
  i <- i+1
  
  #SLOW RESPONDERS
  data=df_consent_slow[df_consent_slow$is_student==student,] 
  model <-  cor.test(y=data$adjusted_score, x=data$test_duration, method = c("kendall"))
  df_corr$is_student[i] <- profes
  df_corr$group[i] <- "SLOW"
  df_corr$tau[i] <- model$estimate
  df_corr$p_value[i] <-  model$p.value
  i <- i+1
}

df_corr

"
   is_student group          tau      p_value
1 non-student   ALL  0.223817502 2.818172e-10
2 non-student  FAST  0.260067166 1.716419e-07 
3 non-student  SLOW -0.009866645 8.491032e-01 <<<<<<< flat and non-significant
4     student   ALL  0.339129559 4.384008e-05
5     student  FAST  0.449691390 1.155429e-03 <<<<<<< fast student is must strongly correlated with score
6     student  SLOW  0.039573866 7.164882e-01 <<<<<<< flat and non-significant

Slow students and non-students do not seem to benefit from more time, 
whereas fast responders for both groups seem to benefit from having more time.

"

#-----------------------------------------------------------------------------
# CORRELATIONS ON TRIMMED SUPPORT
#Correlations on the trimmed data to fit supports of students and non-students
#df_consent <- df_consent[!is.na(df_consent$adjusted_score),] #remove incomplete rows

df_trimmed_fast <- df_consent_fast[df_consent_fast$test_duration<=8,]
df_trimmed_slow <- df_consent_slow[df_consent_slow$test_duration<=2,]
df_trimmed_all <- df_trimmed_fast 
  

df_consent_fast <- df_trimmed_fast[df_trimmed_fast$is_fast,]
df_consent_slow <- df_trimmed_slow[!df_trimmed_slow$is_fast,]

df_corr <- data.frame(matrix(data=NA, nrow=6, ncol=4))
colnames(df_corr) <- c("is_student","group","tau","p_value");

i <- 1
for(student in c("0","1")){
  if(student=="1"){ 
    profes <- "student"
  }
  else{
    profes <- "non-student"
  }
  
  #AGGREGATED
  #  profes="Professional"
  data=df_consent[df_consent$is_student==student,] 
  model<-  cor.test(y=data$adjusted_score, x=data$test_duration, method = c("kendall"))
  df_corr$is_student[i] <- profes
  df_corr$group[i] <- "ALL"
  df_corr$tau[i] <- model$estimate
  df_corr$p_value[i] <-  model$p.value
  i <- i+1
  
  #FAST RESPONDERS
  data=df_consent_fast[df_consent_fast$is_student==student,] 
  model <-  cor.test(y=data$adjusted_score, x=data$test_duration, method = c("kendall"))
  df_corr$is_student[i] <- profes
  df_corr$group[i] <- "FAST"
  df_corr$tau[i] <- model$estimate
  df_corr$p_value[i] <-  model$p.value
  i <- i+1
  
  #SLOW RESPONDERS
  data=df_consent_slow[df_consent_slow$is_student==student,] 
  model <-  cor.test(y=data$adjusted_score, x=data$test_duration, method = c("kendall"))
  df_corr$is_student[i] <- profes
  df_corr$group[i] <- "SLOW"
  df_corr$tau[i] <- model$estimate
  df_corr$p_value[i] <-  model$p.value
  i <- i+1
}

df_corr

"
   is_student group          tau      p_value
1 non-student   ALL  0.223817502 2.818172e-10
2 non-student  FAST  0.260067166 1.716419e-07 
3 non-student  SLOW -0.009866645 8.491032e-01 <<<<<<< flat and non-significant
4     student   ALL  0.339129559 4.384008e-05
5     student  FAST  0.449691390 1.155429e-03 <<<<<<< fast student is must strongly correlated with score
6     student  SLOW  0.039573866 7.164882e-01 <<<<<<< flat and non-significant

Slow students and non-students do not seem to benefit from more time, 
whereas fast responders for both groups seem to benefit from having more time.

