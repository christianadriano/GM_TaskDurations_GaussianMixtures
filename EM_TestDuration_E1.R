"
Gaussian Mixture Model for the test_duration in experiment E1

Use the mixture model to categorize programmers into fast and slow groups w.r.t. test_duration


Dependencies:
- depends on categories of programmers as Students and Not Students


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

#----------------------------------
#ALL
#Run for entire data set together
df_consent$testDuration_fastMembership <- NA
df_prior <- prior.df(wait = df_consent$test_duration)
m.step <- main(wait = df_consent$test_duration, wait.summary.df=df_prior)
df_consent <- compute_Memberships(m.step, df_consent)
plot <- plot_mixture_models(df_consent$test_duration,m.step,"All")
plot
#---------------------------------------------------------
#INVESTIGATE OUTCOMES
cor.test(df_consent$adjusted_score,df_consent$test_duration,
         alternative = "two.sided", 
         method="pearson")
#Positive correlation = 0.1882553  

cor.test(df_consent$adjusted_score,df_consent$testDuration_slowMembership,
         alternative = "two.sided", 
         method="pearson")
#Negative correlation = -0.1033927   

cor.test(df_consent$adjusted_score,df_consent$testDuration_fastMembership,
         alternative = "two.sided", 
         method="pearson")
#Positive correlation = 0.1033927  

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
#Hard clustering
df_consent$is_fast <- FALSE
df_consent[df_consent$testDuration_fastMembership>=0.5,]$is_fast <- TRUE

df_consent_slow <- df_consent[!df_consent$is_fast,]
df_consent_fast <- df_consent[df_consent$is_fast,]

model_2_fast <- lm(formula = adjusted_score ~ test_duration , data=df_consent_fast )
model_2_slow <- lm(formula = adjusted_score ~ test_duration , data=df_consent_slow )
summary(model_2_fast)
summary(model_2_slow)

#(filter,fast,slow)
#(all-aggregated, 0.07035, -0.27952) 
"
Fast people the longer they spend, higher score
Slow people, the longer they spend, lower their score.
" 

model_2_fast <- lm(formula = adjusted_score ~ test_duration, data=df_consent_fast[df_consent_fast$is_student==0,] )
model_2_slow <- lm(formula = adjusted_score ~ test_duration, data=df_consent_slow[df_consent_slow$is_student==0,] )
summary(model_2_fast)
summary(model_2_slow)
#(filter,fast,slow)
#(non-students,0.074119,-0.20392)

model_2_fast <- lm(formula = adjusted_score ~ test_duration, data=df_consent_fast[df_consent_fast$is_student==1,] )
model_2_slow <- lm(formula = adjusted_score ~ test_duration, data=df_consent_slow[df_consent_slow$is_student==1,] )
summary(model_2_fast)
summary(model_2_slow)
#(filter,fast,slow)
#(students,0.27250,-0.6462)


"
Fast people the longer they spend, higher score
Slow people, the longer they spend, lower their score.
However, when only looking at students, these coefficients were stronger.

" 

#------------------------------------------------------------
#However, since I am looking at groups, I should redo the 
#fast/slow classification by group.

df_consent %>% 
  group_by(is_student,is_fast) %>% 
  summarize(count = n())

" The proportions changed with respect to when we computed for all population.
The data is more balanced for all,except Professionals, who none fit in two Gaussians.
"
# 61% of Subjects fall in the Fast Cluster 1112, 
#while 715 are slow
#Looking at the subjects who did not provide demographic information,
#57% (1844) are Fast and 42% (1369) are slow.
#Among students and non-students the proportion is similar.
#Non-Students:  Fast 61%, Slow 39%
#Students: Fast 60%, Slow 40%

"
This homogeneous proportions across groups is very surprising, 
because Fast and Slow only considered test_duration,
whereas student and non-student only considered age and years_experience
"
#DATA
#   is_student is_fast count
#         <int> <lgl>   <int>  <%*>
# 1          0  FALSE     618   39%
# 2          0  TRUE      968   61%
# Subtotal               1586  100%                       
# 3          1  FALSE      97   40%
# 4          1  TRUE      144   60%
# Subtotal                241  100% 
# 5         NA  FALSE    1369   43%
# 6         NA  TRUE     1844   57%
# Subtotal               3213  100%
#*rounded
#----------------------------------------------------------------------
#----------------------------------------------------------------------
#Now compute the membership by each student and non-student, 
#because students and non-students have different mean values for test_duration.

#Load data from demographics and qualification test Experiment-1
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data_loaders//load_consent_create_indexes_E1.R")
df_consent <- load_consent_create_indexes(load_is_student=1)

#remove rows without is_student label
df_consent <- df_consent[complete.cases(df_consent$is_student),];
df_consent$testDuration_fastMembership <- NA;

#STUDENTS 
choice <- 1;
df_selected <- df_consent[df_consent$is_student==choice,]
#df_selected <- df_selected[complete.cases(df_selected$is_student),]
#build the EM model
df_prior <- prior.df(wait = df_selected$test_duration)
m.step <- main(wait = df_selected$test_duration, wait.summary.df=df_prior)
df_selected <- compute_Memberships(m.step,df_selected) 
df_consent$testDuration_fastMembership[which(df_consent$worker_id %in% df_selected$worker_id
                                     & 
                                        df_consent$is_student %in% df_selected$is_student)] <- df_selected$testDuration_fastMembership
 
#plot model for the profession
plot <- plot_mixture_models(df_selected$test_duration,m.step,"Students E1")
plot

#-------------
#NON-STUDENTS
choice <- 0;

df_selected <- df_consent[df_consent$is_student==choice,]

df_prior <- prior.df(wait = df_selected$test_duration)
m.step <- main(wait = df_selected$test_duration, wait.summary.df=df_prior)
df_selected <- compute_Memberships(m.step,df_selected) 
df_consent$testDuration_fastMembership[which(df_consent$worker_id %in% df_selected$worker_id
                                             & 
                                            df_consent$is_student==choice)] <- df_selected$testDuration_fastMembership

#plot model for the profession
plot <- plot_mixture_models(df_selected$test_duration,m.step,"Non-Students E1")
plot

#---------------
df_consent$is_fast <- FALSE
df_consent[df_consent$testDuration_fastMembership>=0.5,]$is_fast <- TRUE

#Save df_consent to file so we can retrieve the tesDuration_fast
path = "C://Users//Christian//Documents//GitHub//EM_GaussianMixtureModel_TaskDurations//output//"
write.csv(df_consent,paste0(path,"E1_consent_with_testDuration_fastMembership.csv"));

#Check the proportion across professions.
df_consent %>% 
  group_by(is_student,is_fast) %>% 
  summarize(count = n())
#       is_student is_fast   count  proportion
#           <int>   <lgl>   <int>   %
# 1          0      FALSE     628   40%
# 2          0      TRUE      958   60%
#                   Sutotal  1586  100%
# 3          1      FALSE      63   26%
# 4          1      TRUE      178   74%
#                   Sutotal   241  100%

"Compared with the clusters computed over all participants, 
the proportion of is_fast for non_student changed radically.
This shows the importance of computing this clustering by group. "

#-----------------------------------------------------------
#Evaluate how fast and slow can explain adjusted_score score
df_consent_fast <- df_consent[df_consent$is_fast,]
df_consent_slow <- df_consent[!df_consent$is_fast,]

model_2_fast <- lm(formula = adjusted_score ~ test_duration , data=df_consent_fast )
model_2_slow <- lm(formula = adjusted_score ~ test_duration , data=df_consent_slow )
summary(model_2_fast)
summary(model_2_slow)

#(filter,fast,slow)
#(all-aggregated, 0.080440, -0.22327) 
"
Fast people the longer they spend, higher score
Slow people, the longer they spend, lower their score.
" 

model_2_fast <- lm(formula = adjusted_score ~ test_duration, data=df_consent_fast[df_consent_fast$is_student==0,] )
model_2_slow <- lm(formula = adjusted_score ~ test_duration, data=df_consent_slow[df_consent_slow$is_student==0,] )
summary(model_2_fast)
summary(model_2_slow)
#(filter,fast,slow)
#(non-students,0.071942,-0.2940)

model_2_fast <- lm(formula = adjusted_score ~ test_duration, data=df_consent_fast[df_consent_fast$is_student==1,] )
model_2_slow <- lm(formula = adjusted_score ~ test_duration, data=df_consent_slow[df_consent_slow$is_student==1,] )
summary(model_2_fast)
summary(model_2_slow)
#(filter,fast,slow)
#(students,0.2874,2.1211) 

"
Students have a different behavior. 
Fast students the longer they spend, higher score
Slow students, the longer they spend, much higher their score.
" 
"In conclusion, group membership within duration is a confounder for certain professions, 
but not others.
" 
#---------------
#PLOTS to show this phenomenon

df_consent$is_student <- as.factor(df_consent$is_student)
df_consent_fast <- df_consent[df_consent$is_fast,]
df_consent_slow <- df_consent[!df_consent$is_fast,]
df_consent_slow <- rbind(df_consent_slow, c(1:100))

#Filling NAs
df_consent_slow[is.na(df_consent_slow$worker_id),]$is_student <- "1"
df_consent_slow[is.na(df_consent_slow$worker_id),]$test_duration <- 0.5
df_consent_slow[is.na(df_consent_slow$worker_id),]$adjusted_score <- 0
df_consent_slow[is.na(df_consent_slow$worker_id),]$is_fast <- FALSE


ggplot(df_consent, aes(x=test_duration, y=adjusted_score)) + 
  geom_point(aes(colour = is_student))+
  stat_smooth(method = 'lm', formula = y ~ x, aes(colour = is_student), se= FALSE)+
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
  ggtitle("All: Duration impact on Score by Is_Student")


ggplot(df_consent_fast, aes(x=test_duration, y=adjusted_score)) + 
  geom_point(aes(colour = is_student))+
  stat_smooth(method = 'lm', formula = y ~ x, aes(colour = is_student), se= FALSE)+
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
  ggtitle("Fast speed-cluster: Duration impact on Score by Is_student")

ggplot(df_consent_slow, aes(x=test_duration, y=adjusted_score)) + 
  geom_point(aes(colour = is_student))+
  stat_smooth(method = 'lm', formula = y ~ x, aes(colour = is_student), se= FALSE)+
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
  ggtitle("Slow speed-cluster: Duration impact on Score by Is_Student")

#---------------------------
students <- df_consent[df_consent$is_student=="1",]$adjusted_score
non_students <- df_consent[df_consent$is_student=="0",]$adjusted_score
t.test(students,non_students)

# Welch Two Sample t-test
# 
# data:  students and non_students
# t = -2.1623, df = 324.46, p-value = 0.03133
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.31786057 -0.01500566
# sample estimates:
#   mean of x mean of y 
# 2.527016  2.693449 

"
Eventhoug non-students have statistically significan higher average score, the
slow group, the longer they spend, worse their score becomes.
"
#-----------------------------
# Checking only the slow group
students <- df_consent_slow[df_consent_slow$is_student=="1",]$adjusted_score
non_students <- df_consent_slow[df_consent_slow$is_student=="0",]$adjusted_score
t.test(students,non_students)
# Welch Two Sample t-test
# 
# data:  students and non_students
# t = -0.53882, df = 77.269, p-value = 0.5916
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.3446355  0.1978381
# sample estimates:
#   mean of x mean of y 
# 2.391962  2.465360 
"
However, their average scores are not statistically significant distinct!
So, slow non-students are probably as bad as the slow students.
"

#-------------------------------
#What if I cut-off at the max range of students?
#Does the difference in correlation persist?
#Bingo! The relationship reverses. Now, both are positive, 
#the longer they spend, higher the accuracy.

df_cut <- df_consent_slow[df_consent_slow$test_duration<=0.75,]

ggplot(df_cut, aes(x=test_duration, y=adjusted_score)) + 
  geom_point(aes(colour = is_student))+
  stat_smooth(method = 'lm', formula = y ~ x, aes(colour = is_student), se= FALSE)+
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
  ggtitle("Slow speed-cluster: Duration impact on Score by Is_Student")
