"
Is Test Duration Bimodal in experiment E1?

It is bimodal across all qualification score level, more strongly for Score=100%
TODO:
- Plot bimodality in histogram and density. Explain. Need to zoom in (remove outliers upper_wiskers)
- Separate this in a different file
- Discuss bimodality in E1 as well for different scores
- Prepare to run EM for E1
This will be my first generalization result across experiments E1 and E2!!!


"


library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(tidyr)
library(tidyverse)


#---------------------------------------------------------
#Load data from demographics and qualification test Experiment-1
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data_loaders//load_consent_create_indexes_E1.R")
source("C://Users//Christian//Documents//GitHub//EM_GaussianMixtureModel_TaskDurations//util//multiplot.R")

df_consent <- load_consent_create_indexes(load_is_student=1)

#-------------------------------------------
# TEST DURATION ALL

p0 <- ggplot(df_consent, aes(x=test_duration)) +
  geom_histogram(binwidth = 0.5, colour = "black", 
                 fill = "lightblue") +
  theme_minimal()+
  theme(
    legend.position="top",
    legend.justification = "left",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 12),
    plot.title = element_text(size=14),
    axis.text.x = element_text(angle = 0, hjust = 1, size=12)
  ) +
  ylab("Probability Density") +
  xlab("Test Duration (minutes)") +
  ggtitle("Test Duration All E1")

p1 <- ggplot(df_consent, aes(x=test_duration)) +
  geom_density(colour = "black", 
               fill = "lightblue") +
  theme_minimal()+
  theme(
    legend.position="top",
    legend.justification = "left",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 12),
    plot.title = element_text(size=14),
    axis.text.x = element_text(angle = 0, hjust = 1, size=12)
  ) +
  ylab("Probability Density") +
  xlab("Test Duration (minutes)") +
  ggtitle("Test Duration All E1")

multiplot(p0,p1, cols=1)

#The plot might be slightly bimodal, suggesting that there are two 
#regimes of speed of answering the tests. Next I investigate 
#if this bimodal distribution seems to be present in
#the following subpopulations of the data: qualified vs not qualified,
#scores=2, 3, 4; years_experience (by quartile, below vs above median)

#----------------------------------------------------
# TEST DURATION BY QUALIFIED AND NOT-QUALIFIED
#Score>=2 (qualified

ggplot(df_consent[df_consent$test_duration<=20 & df_consent$qualification_score<2,], aes(x=test_duration)) +
  geom_histogram(binwidth = 0.5, colour = "black", 
                 fill = "lightblue") +
  
 # geom_density(colour = "black", 
#               fill = "lightblue")+
  theme_minimal()+
  theme(
    legend.position="top",
    legend.justification = "left",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 12),
    plot.title = element_text(size=14),
    axis.text.x = element_text(angle = 0, hjust = 1, size=12)
  ) +
  ylab("Probability Density") +
  xlab("Test Duration (minutes)") +
  ggtitle("Test Duration All E1")


#Only who qualified (qualification_score>=2)
p3 <- ggplot(df_consent[df_consent$qualification_score>=2,], 
             aes(x=test_duration)) +
  geom_density(colour = "black", 
               fill = "lightblue") +
  theme_minimal()+
  theme(
    legend.position="top",
    legend.justification = "left",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 12),
    plot.title = element_text(size=14),
    axis.text.x = element_text(angle = 0, hjust = 1, size=12)
  ) +
  ylab("Probability Density") +
  xlab("Test Duration (minutes)") +
  ggtitle("Test Duratio Qualified Participants (score>=50%) E1")

#Only who DID NOT qualified (qualification_score<2)
p4 <- ggplot(df_consent[df_consent$qualification_score<2,], 
             aes(x=test_duration)) +
  geom_density(colour = "black", 
               fill = "lightblue") +
  theme_minimal()+
  theme(
    legend.position="top",
    legend.justification = "left",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 12),
    plot.title = element_text(size=14),
    axis.text.x = element_text(angle = 0, hjust = 1, size=12)
  ) +
  ylab("Probability Density") +
  xlab("Test Duration (minutes)") +
  ggtitle("Test Duration Non-Qualified Participants (score<50%) E1")

multiplot(p1,p2,p3,p4,cols=2)

#-----------------------------------------
#TEST DURATION BY SCORE LEVEL

p0 <- ggplot(df_consent[df_consent$qualification_score==0,], 
             aes(x=test_duration)) +
  #geom_density(colour = "black", 
  #             fill = "lightblue") +
  geom_histogram(binwidth = 0.5, colour = "black", 
                 fill = "lightblue") +
  theme_minimal()+
  theme(
    legend.position="top",
    legend.justification = "left",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 12),
    plot.title = element_text(size=14),
    axis.text.x = element_text(angle = 0, hjust = 1, size=12)
  ) +
  ylab("Probability Density") +
  xlab("Test Duration (minutes)") +
  ggtitle("Test Duration for Score=0 E1")

p1 <- ggplot(df_consent[df_consent$qualification_score==1,], 
             aes(x=test_duration)) +
  #geom_density(colour = "black", 
  #             fill = "lightblue") +
  geom_histogram(binwidth = 0.5, colour = "black", 
                 fill = "lightblue") +
  theme_minimal()+
  theme(
    legend.position="top",
    legend.justification = "left",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 12),
    plot.title = element_text(size=14),
    axis.text.x = element_text(angle = 0, hjust = 1, size=12)
  ) +
  ylab("Probability Density") +
  xlab("Test Duration (minutes)") +
  ggtitle("Test Duration for Score=1 E1")

p2 <- ggplot(df_consent[df_consent$qualification_score==2,], 
             aes(x=test_duration)) +
  #geom_density(colour = "black", 
  #             fill = "lightblue") +
  geom_histogram(binwidth = 0.5, colour = "black", 
                 fill = "lightblue") +
  theme_minimal()+
  theme(
    legend.position="top",
    legend.justification = "left",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 12),
    plot.title = element_text(size=14),
    axis.text.x = element_text(angle = 0, hjust = 1, size=12)
  ) +
  ylab("Probability Density") +
  xlab("Test Duration (minutes)") +
  ggtitle("Test Duration for Score=2 E1")


p3 <- ggplot(df_consent[df_consent$qualification_score==3,], 
             aes(x=test_duration)) +
  #geom_density(colour = "black", 
  #             fill = "lightblue") +
  geom_histogram(binwidth = 0.5, colour = "black", 
                 fill = "lightblue") +
  theme_minimal()+
  theme(
    legend.position="top",
    legend.justification = "left",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 12),
    plot.title = element_text(size=14),
    axis.text.x = element_text(angle = 0, hjust = 1, size=12)
  ) +
  ylab("Probability Density") +
  xlab("Test Duration (minutes)") +
  ggtitle("Test Duration for Score=3 E1")

p4 <- ggplot(df_consent[df_consent$qualification_score==4,], 
             aes(x=test_duration)) +
  #geom_density(colour = "black", 
  #             fill = "lightblue") +
  geom_histogram(binwidth = 0.5, colour = "black", 
                 fill = "lightblue") +
  theme_minimal()+
  theme(
    legend.position="top",
    legend.justification = "left",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 12),
    plot.title = element_text(size=14),
    axis.text.x = element_text(angle = 0, hjust = 1, size=12)
  ) +
  ylab("Probability Density") +
  xlab("Test Duration (minutes)") +
  ggtitle("Test Duration for Score=4 E1")

multiplot(p0,p1,p2,p3,p4,cols=2)


#---------------------------------------------
#HISTOGRAM PLOTS STUDENTS AND NON-STUDENTS

p0 <- ggplot(df_consent[df_consent$is_student==0,], 
             aes(x=test_duration)) +
  #geom_density(colour = "black", 
  #             fill = "lightblue") +
  geom_histogram(binwidth = 0.5, colour = "black", 
                 fill = "lightblue") +
  theme_minimal()+
  theme(
    legend.position="top",
    legend.justification = "left",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 12),
    plot.title = element_text(size=14),
    axis.text.x = element_text(angle = 0, hjust = 1, size=12)
  ) +
  ylab("Probability Density") +
  xlab("Test Duration (minutes)") +
  ggtitle("Test Duration of Non-Students E1")

p1 <- ggplot(df_consent[df_consent$is_student==1,], 
             aes(x=test_duration)) +
  #geom_density(colour = "black", 
  #             fill = "lightblue") +
  geom_histogram(binwidth = 0.5, colour = "black", 
                 fill = "lightblue") +
  theme_minimal()+
  theme(
    legend.position="top",
    legend.justification = "left",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 12),
    plot.title = element_text(size=14),
    axis.text.x = element_text(angle = 0, hjust = 1, size=12)
  ) +
  ylab("Probability Density") +
  xlab("Test Duration (minutes)") +
  ggtitle("Test Duration of Students E1")

multiplot(p0,p1,cols=1)

#-------------------------------------------
#DENSITY PLOTS STUDENTS AND NON-STUDENTS

p0 <- ggplot(df_consent[df_consent$is_student==0,], 
             aes(x=test_duration)) +
  geom_density(colour = "black", 
               fill = "lightblue") +
  #geom_histogram(binwidth = 0.5, colour = "black", 
  #               fill = "lightblue") +
  theme_minimal()+
  theme(
    legend.position="top",
    legend.justification = "left",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 12),
    plot.title = element_text(size=14),
    axis.text.x = element_text(angle = 0, hjust = 1, size=12)
  ) +
  ylab("Probability Density") +
  xlab("Test Duration (minutes)") +
  ggtitle("Test Duration of Non-Students E1")

p1 <- ggplot(df_consent[df_consent$is_student==1,], 
             aes(x=test_duration)) +
  geom_density(colour = "black", 
               fill = "lightblue") +
  #geom_histogram(binwidth = 0.5, colour = "black", 
  #               fill = "lightblue") +
  theme_minimal()+
  theme(
    legend.position="top",
    legend.justification = "left",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 12),
    plot.title = element_text(size=14),
    axis.text.x = element_text(angle = 0, hjust = 1, size=12)
  ) +
  ylab("Probability Density") +
  xlab("Test Duration (minutes)") +
  ggtitle("Test Duration of Students E1")

multiplot(p0,p1,cols=1)

#----------------------------
# Plot on same chart
# Nice tutorial here - http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/

#if(!require(devtools)) install.packages("devtools")
#devtools::install_github("kassambara/ggpubr")
#install.packages("ggpubr")
library(ggpubr)

df_consent$is_student <-  as.factor(df_consent$is_student)
df_consent <- df_consent[!is.na(df_consent$is_student),]

density.p <- gdensity(df_consent$is_student==0,, x = "test_duration", 
                       palette = "jco")

density.p <- ggdensity(df_consent, x = "test_duration", 
                       fill = "is_student", palette = "jco")

#-----------
t.test(df_consent[df_consent$is_student==0,]$test_duration,
       df_consent[df_consent$is_student==1,]$test_duration)
# t = 5.6845, df = 607.02, p-value = 2.039e-08
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.6556541 1.3478138
# sample estimates:
#   mean of x mean of y 
# 3.312319  2.310585 
#is_student (is slower) and not-student (is faster) have statistically 
#significant distinct average test_duration

