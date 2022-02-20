"
Investigate distribution of testDuration_fastMembership E2 and E1 
across professional groups. The idea is to understand how
groups could be split by median or quartile the membership distribution.
"

library(ggplot2)
library(moments)

"Load only Consent data. No data from tasks, only from demographics and qualification test"
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data_loaders//load_consent_create_indexes_E2.R")
df_consent <- load_consent_create_indexes()
df_consent <- rename(df_consent,progr_years=years_programming)
df_consent <- rename(df_consent,test_score=adjusted_score)
df_consent <- rename(df_consent,partic_age=age)

df_selected <-
  dplyr::select(df_consent,
                profession,
                testDuration_fastMembership,
                progr_years,
                partic_age,
                test_duration,
                test_score #outcome variable
  );

####-------------------------------------------------------


ggplot(df_selected, aes(testDuration_fastMembership, profession)) +  # Boxplot with updated labels
  geom_boxplot()
"Looking at the boxplot, the distributions are left skewed
by more extreme values (longer tail) towards the left of the
median. This makes the mean a very non representative
cutpoint. Our suggestion is to either use the median as
cutpoint between fast and slow responders or stratify these
groups by the quartiles."

professions <- unique(df_selected$profession)

df_results <- data.frame(matrix("NA",6,7))
colnames(df_results) <- c("profession","1stQrt","median","mean","3rd_Quartile",
                          "skewness","kurtosis")

row_index=0;
for(prof in professions ){
  row_index=row_index+1;
  df_results[row_index,1] <- prof
  prof_stats <- summary(df_selected[df_selected$profession==prof,]$testDuration_fastMembership)
  
  df_results[row_index,2] <- round(prof_stats[[2]],3)
  df_results[row_index,3] <- round(prof_stats[[3]],3)
  df_results[row_index,4] <- round(prof_stats[[4]],3)
  df_results[row_index,5] <- round(prof_stats[[5]],3)
  df_results[row_index,6] <- round(skewness(df_selected[df_selected$profession==prof,]$testDuration_fastMembership),3)
  df_results[row_index,7] <- round(kurtosis(df_selected[df_selected$profession==prof,]$testDuration_fastMembership),3)
  
}

df_results

"              profession 1stQrt median  mean 3rd_Quartile skewness kurtosis
1              Hobbyist  0.584   0.64 0.614        0.664   -1.326    3.997
2 Undergraduate_Student  0.559  0.632   0.6        0.657   -1.226    3.604
3          Professional  0.595  0.639 0.622        0.655   -1.083    3.136
4      Graduate_Student   0.57  0.638  0.61        0.657   -1.054     3.11
5                 Other  0.552  0.612 0.591         0.64   -1.111    3.558
6            Programmer  0.497  0.608 0.567         0.63   -0.764    2.434
"

"
COMPARING MEDIANS
Note that all of the professions have similar median (Professional=0.6391,
Hobbyist=0.6396, Graduate_Students=0.6376, Undergraduate_Student= 0.6322),
except for Programmer (0.6080) and Other (0.6125)"

"
COMPARING SKEWNESS
Programmer has the larger skewness followed by Hobbyist and undergraduate,
whereas Other, professional and graduate have similar values. This might
be an indication that the first three groups are less homogeneous than the
the second group w.r.t. membership distribution. In practical terms,
this means that it is easier to justify for the more homogenous group a
split in two categories (fast, slow), while for the more heterogeneous 
professions, such split might mix people with very distinct w.r.t. 
to strength of their membership to answer speed.
"

"
In the face of this information, how should we stratify each profession
w.r.t. to categories of fast and slow responders? 

The first option is to not stratify, but instead take the membership as 
a continuous variable that could be used in the regression formula. However,
as we know that some professions have a more heterogeneous 
distribution (larger skewness) than others, doing so would imply fitting 
a line for a data pattern that is potentially non-linear. 

The non-linear model would be better fit with 
a spline, but that would require making assumptions about the anchor points,
for which we do not have any theoretical (data-independent) information. 
This lack of prior-knowledge poses two threats: 
(1) difficulty to validate the fit and (2) high risk of overfitting.
The difficulty to validate is higher for the heterogeneous professions, 
because stronger assumptions about number and position of the anchor points.
Conversely, the risk of overfitting is higher for the homogeneous professions, 
because a linear model would be a more parsimonious approach. 

Future work could rely on the current results as prior-knowledge that could be 
used to compare with various fit models.
"