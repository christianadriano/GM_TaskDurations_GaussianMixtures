"Utility functions to visualize and compute membership to Mixture Models

These functions are currently used by EM_TestDuration_E1 and EM_TestDuration_E2
"


#------------------
#Visualize results
#' Plot a Mixture Component
#' 
#' @param x Input ata.
#' @param mu Mean of component.
#' @param sigma Standard of component.
#' @param lam Mixture weight of component.
plot_mix_comps <- function(x, mu, sigma, lam) {
  lam * dnorm(x, mu, sigma)
}

#----------------
#' @param wait Input ata.
#' @param m.step approximated mixture models produced by the EM algorithm
plot_mixture_models <- function(wait,m.step,title_prefix){ 
  
  if(m.step$mu[1]>m.step$mu[2]){
    color_1 <- "red"
    color_2 <- "darkblue"
  }else{
    color_1 <- "darkblue"
    color_2 <- "red"
  }
  
  plot <- data.frame(x = wait) %>%
    ggplot() +
    geom_histogram(aes(x, ..density..), binwidth = 0.5, colour = "black", 
                   fill = "lightblue") +
    theme_minimal()+
    stat_function(geom = "line", fun = plot_mix_comps,
                  args = list(m.step$mu[1], sqrt(m.step$var[1]), 
                              lam = m.step$alpha[1]),
                  colour = color_1, lwd = 1.0) +
    stat_function(geom = "line", fun = plot_mix_comps,
                  args = list(m.step$mu[2], sqrt(m.step$var[2]), 
                              lam = m.step$alpha[2]),
                  colour = color_2, lwd = 1.0) +
    ylab("Density") +
    xlab("Test Duration (minutes), binwidth=0.5") +
    ggtitle(paste0(title_prefix,": Gaussian Mixture Model for Test Duration"))
}


#---------------------------------------
#Compute cluster membership probability
compute_Memberships <- function(mstep,df){
  
  datapoints <- df$test_duration
  mu_1 <-  m.step$mu[1]
  mu_2 <- m.step$mu[2]
  var_1 <- m.step$var[1]
  var_2 <- m.step$var[2]
  
  #probability of seeing data points if coming from Gaussian 1
  p_1_vector <- pnorm(datapoints,mu_1,var_1)
  p_2_vector <- pnorm(datapoints,mu_2,var_2)
  
  print(paste0("mu_1:",mu_1))
  print(paste0("mu_2:",mu_2))
  
  #Probability to be fast.
  df$testDuration_fastMembership <- p_1_vector
  #Probability to be slow.
  df$testDuration_slowMembership <- p_2_vector
  
  "if(mu_1<mu_2){
    #smaller mu_1 means faster, so fast membership takes p1 
    df$testDuration_fastMembership <- p_1_vector
    df$testDuration_slowMembership <- 1-p_1_vector
  }else{
    df$testDuration_fastMembership <- 1-p_1_membership
    df$testDuration_slowMembership <- p_1_membership
  }
  "
    
  return(df)
}