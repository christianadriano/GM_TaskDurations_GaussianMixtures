"
Label participant test results according to the speed of their answers,
which was determined by the participation on the category 
Fast Answers Membership Probability provided in the field 
testDuration_fastMembership (between 0 and 1)

Categorization can be based on the mean or median. The assumption for using the
mean is that it is representative if the distribution is Gaussian. However,
if the distribution is skewed, which it is (
check script Distribution_FastSlowMembership_Test_E2.R)

"


compute_median_label <- function(df){
  df$is_fast <- FALSE
  profession_list <- unique(df$profession)
  for(prof in profession_list){
    median_fast_prof <- median(df[df$profession==prof, ]$testDuration_fastMembership);
    df[df$profession==prof & df$testDuration_fastMembership>=median_fast_prof,]$is_fast <- TRUE
  }
  return(df)
}

compute_mean_label <- function(df){
  df$is_fast <- FALSE
  profession_list <- unique(df$profession)
  for(prof in profession_list){
    mean <- mean(df[df$profession==prof, ]$testDuration_fastMembership);
    df[df$profession==prof & df$testDuration_fastMembership>=mean_fast_prof,]$is_fast <- TRUE
  }
  return(df)
}
