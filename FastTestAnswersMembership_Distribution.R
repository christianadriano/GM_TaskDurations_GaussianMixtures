"
Investigate distribution of testDuration_fastMembership E2 and E1 
across professional groups. The idea is to understand how
groups could be split by median or quartile the membership distribution.
"

library(ggplot2)

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

"Note that all the professions have similar median (Professional=0.6391,
Hobbyist=0.6396, Graduate_Students=0.6376, Undergraduate_Student= 0.6322),
except for Programmer (0.6080) and Other (0.6125)"

summary(df_selected[df_selected$profession=="Other",]$testDuration_fastMembership)


