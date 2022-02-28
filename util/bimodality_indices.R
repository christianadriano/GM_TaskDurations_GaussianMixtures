"
Utility scripts to compute Biomdal indices for comparing how
strong is the bimodality across strata.

Sources: 
 https://en.wikipedia.org/wiki/Multimodal_distribution#Bimodality_amplitude
 https://r-coder.com/mode-r/
"

library(multimode)
library(moments)

#-------------------------------------------------------------------
" Bimodality amplitude A_B = (A1 - A_antimode)/ A1
, where A1 is the amplitude of the smaller peak and the A_antimode is
 the amplitude of the deepest point between the two peaks
 A_B is always smaller than 1, hence larger values of A_B
indicate more distinct peaks"
compute_Bimodality_Amplitude <-  function(data_vector){
  modes <- locmodes(data_vector,mod0 = 2)
  if(modes$locations[1]<modes$locations[3]){
    A_1 <- modes$fvalue[1]
  }else{
    A_1 <- modes$fvalue[3]
  }
  A_antimode <- modes$fvalue[2]
  
  Bimodal_Amplitude <- (A_1 - A_antimode) /A_1
  return (round(Bimodal_Amplitude,3))
}

#-------------------------------
# Bimodality coefficient B = (skewness^2 + 1)/kurtosis
compute_Bimodality_Coefficient <- function(data_vector){
  sk <- skewness(data_vector)
  kt <- kurtosis(data_vector)
  
  Bimodality_Coefficient <- (sk^2 + 1)/kt
  return(round(Bimodality_Coefficient,3))
}

#------------------------------
# Bimodal ratio R = Amplituted Right Peak / Amplituted left Peak
compute_Bimodality_Ratio <- function(data_vector){
  Bimodality_Ratio = modes$locations[1] / modes$locations[3]
  return(round(Bimodality_Ratio,3)) 
}

#------------------------------------------
# Plotting the distribution with the modes

plot_modes <- function(data_vector,plot_title){
  modes <- locmodes(data_vector,mod0 = 2)
  plot(modes)
  title(main = plot_title)
}