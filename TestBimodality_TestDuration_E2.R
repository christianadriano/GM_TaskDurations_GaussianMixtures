"
Test Bimodality and compute their strength.

Rejecting the null-hypothesis indicates some evidence that the 
distribution is at least bi-modal.

"

library(diptest)

prepareData <- function(){
  #Load only Consent data. No data from tasks, only from demographics and qualification test
  source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data_loaders//load_consent_create_indexes_E2.R")
  df_consent <- load_consent_create_indexes()
  df_consent <- rename(df_consent,progr_years=years_programming)
  df_consent <- rename(df_consent,test_score=adjusted_score)
  df_consent <- rename(df_consent,age_years=age)
  df_consent <- rename(df_consent,fast_classif=testDuration_fastMembership)
  return(df_consent)
}

df_consent <- prepareData()

df_selected <-
  dplyr::select(df_consent,
                profession,
                fast_classif,
                progr_years,
                age_years,
                test_duration,
                qualification_score,
                test_score #outcome variable
  );


#-------------------------------------------
# TEST ACROSS PROFESSIONS

professions <- unique(df_consent$profession)

for(prof in professions){
  df_prof <- df_selected[df_selected$profession==prof,]
  result <- dip.test(df_prof$test_duration)
  print(paste("Unimodality test for test_duration of ",prof))
  print(result)
}

"All professions failed to reject the null-hypothesis, so one cannot
accept the alternative hypothesis that the distribution is at least bimodal."

#----------------------------
# TEST ACROSS SCORE LEVELS


#Needs to round because some scores are not integer, because are averages.
df_selected$round_qualification_score <- round(df_selected$qualification_score,digits = 0)
scores <- unique(df_selected$round_qualification_score)

for(score in scores){
  df_score <- df_selected[df_selected$round_qualification_score==score,]
  result <- dip.test(df_score$test_duration)
  print(paste("Unimodality test for test_duration of score ",score))
  print(result)
}
"Only score==0 rejected the null-hypothesis, which signifies that
the has to accept that alternative hypothesis that distribution is 
non-unimodal."