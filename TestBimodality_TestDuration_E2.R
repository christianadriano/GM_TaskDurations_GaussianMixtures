"
Test Bimodality and compute their strength.

Rejecting the null-hypothesis indicates some evidence that the 
distribution is at least bi-modal.

"
library(multimode)
library(diptest)

source("C://Users//Christian//Documents//GitHub//EM_GaussianMixtureModel_TaskDurations//util//bimodality_indices.R")


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
df_selected$round_test_score <- round(df_selected$test_score,digits = 0)
scores <- unique(df_selected$round_test_score)

for(score in scores){
  df_score <- df_selected[df_selected$round_test_score==score,]
  result <- dip.test(df_score$test_duration)
  print(paste("Unimodality test for test_duration of score ",score))
  print(result)
}
"Only score==0 rejected the null-hypothesis, which signifies that
the has to accept that alternative hypothesis that distribution is 
non-unimodal."

#---------------------------------------------------

df_results <- data.frame(matrix("NA",7,36))
colnames(df_results) <- c("prof","score","size","yoe",
                          "coefficient",
                          "amplitude",
                          "ratio")

row_index=0;
for(prof in professions ){
  #all scores aggregate
  row_index=row_index+1;
  df_prof <- df_selected[df_selected$profession==prof,]
  data_dist <- df_prof$test_duration 
  df_results[row_index,1] <- prof
  df_results[row_index,2] <- "all" #score
  df_results[row_index,3] <- length(data_dist) #data points
  df_results[row_index,4] <- mean(df_prof$progr_years) #mean years of experience
  df_results[row_index,5] <- compute_Bimodality_Amplitude(data_dist)
  df_results[row_index,6] <- compute_Bimodality_Coefficient(data_dist)
  df_results[row_index,7] <- compute_Bimodality_Ratio(data_dist)
  
  #by scores
  for(score_value in scores){
    df_score <- df_prof[df_prof$round_test_score==score_value,]
    row_index=row_index+1;
    df_results[row_index,1] <- prof
    df_results[row_index,2] <- score_value
    data_dist <- df_score$test_duration 
    df_results[row_index,3] <- length(data_dist) #data points
    if(length(data_dist)>0){
      df_results[row_index,4] <- mean(df_score$progr_years) #mean years of experience
      df_results[row_index,5] <- compute_Bimodality_Amplitude(data_dist)
      df_results[row_index,6] <- compute_Bimodality_Coefficient(data_dist)
      df_results[row_index,7] <- compute_Bimodality_Ratio(data_dist)
    }else{print(paste(prof,score,length(data_dist)))}
  }
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



