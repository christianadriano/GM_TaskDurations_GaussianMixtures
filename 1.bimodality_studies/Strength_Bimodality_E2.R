"
E2 - Compute the strength of bimodality across profession and scores.

Plots
Compute correlations
Generate students vs non-students

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
#Needs to round because some scores are not integer, because are averages.
df_selected$round_test_score <- round(df_selected$test_score,digits = 0)
scores <- unique(df_selected$round_test_score)

df_results <- data.frame(matrix("NA",36,7))
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
  df_results[row_index,4] <- round(mean(df_prof$progr_years),3) #mean years of experience
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
      df_results[row_index,4] <- round(mean(df_score$progr_years),3) #mean years of experience
      df_results[row_index,5] <- compute_Bimodality_Amplitude(data_dist)
      df_results[row_index,6] <- compute_Bimodality_Coefficient(data_dist)
      df_results[row_index,7] <- compute_Bimodality_Ratio(data_dist)
    }else{print(paste(prof,score,length(data_dist)))}
  }
}

df_results
write.csv(df_results,"C://Users//Christian//Documents//GitHub//EM_GaussianMixtureModel_TaskDurations//output//E2//bimodality_measures_E2.csv", row.names = FALSE)

"
                    prof score size    yoe coefficient amplitude  ratio
1               Hobbyist   all  484  5.501       0.829     0.565  5.649
2               Hobbyist     1   80  3.888        0.24     0.443  1.201
3               Hobbyist     2  139  5.198       0.844     0.566  5.852
4               Hobbyist     4   52  8.827       0.557     0.491  2.049
5               Hobbyist     0   82  2.671       0.963     0.568 22.947
6               Hobbyist     5   72  9.028       0.815     0.533  4.075
7               Hobbyist     3   59  5.102       0.829     0.563  5.451
8  Undergraduate_Student   all  443  2.821       0.935     0.574 11.333
9  Undergraduate_Student     1   99  2.106       0.962     0.537 14.544
10 Undergraduate_Student     2  106  2.566       0.913     0.618  9.784
11 Undergraduate_Student     4   50   3.44       0.978     0.514  9.594
12 Undergraduate_Student     0   65  2.415        0.93     0.608 10.345
13 Undergraduate_Student     5   55  4.236       0.863     0.664  7.242
14 Undergraduate_Student     3   68  3.044       0.933     0.628  5.953
15          Professional   all  417  9.408       0.906     0.544 10.341
16          Professional     1   45  7.267       0.806     0.602   4.73
17          Professional     2   77  8.623        0.71     0.495  2.653
18          Professional     4   97 11.216       0.886     0.547  6.059
19          Professional     0   39  7.103       0.936     0.564 13.495
20          Professional     5   99 11.061       0.824     0.586  4.345
21          Professional     3   60  7.867       0.696     0.513  3.215
22      Graduate_Student   all  283  3.456        0.96     0.549 12.632
23      Graduate_Student     1   56  2.625       0.943     0.655 16.497
24      Graduate_Student     2   73  3.356       0.969     0.593  7.974
25      Graduate_Student     4   22  4.955       0.809     0.517  5.192
26      Graduate_Student     0   54  3.667       0.272     0.502  1.371
27      Graduate_Student     5   35  3.943       0.969      0.52 16.161
28      Graduate_Student     3   43  3.279       0.986     0.545  8.675
29                 Other   all  112  3.964       0.851     0.579  5.795
30                 Other     1   23  2.739       0.961     0.674   7.06
31                 Other     2   35  1.357       0.863     0.558  5.487
32                 Other     4   11 12.636       0.479     0.524  1.917
33                 Other     0   22  0.909       0.998     0.489   7.22
34                 Other     5    6 20.333        0.82     0.794  2.878
35                 Other     3   15    3.5       0.803      0.73  2.683
36            Programmer   all   49 11.092        0.96     0.722  8.404
37            Programmer     1    3  2.833        0.75     0.757  1.212
38            Programmer     2   11  8.273       0.977     0.599  4.596
39            Programmer     4    9 16.778           1     0.932  6.098
40            Programmer     0    9  3.333       0.787     0.787  2.329
41            Programmer     5   13 17.462           1     0.725  5.319
42            Programmer     3    4      9       0.927     0.847  1.647
"

"
Bimodality seems stronger among students, then professional and hobbyists, and weakest
among programmer and others. This trend measured by ratio and coefficient. This
trend generalizes both for the groups that passed the test (score>=3) and
those who did not (score<3).

Amplitude is uniform around 0.5 to 0.6, except Programmers and the Others with high score,
whose amplitude hover above 0.7. Because amplitude measures the relationship
between the two modes. This suggest that for all the left mode (fast answers)
has approximately twice the density of the right mode (slow answers). In the case of
others and programmers, the densities of the left and right are more similar,
the density of left mode (fast answers) being 1.4 times the right mode (slow answers).

"

#-----------------------------------
"Compare students and non-students"

