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


compute_median_label <- function(df,profession){
  professions <- unique(df$profession)
  for(prof in professions){
    median_membership_prof <- median(df_prof[df_prof$profession==prof, ]$testDuration_fastMembership);
    #filter-out all slow for a given profession  
    df$is_fast <- df[df$profession==prof & testDuration_fastMembership<median_membership_prof,]$is_fast

  }
}