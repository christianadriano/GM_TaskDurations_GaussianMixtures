"
Utility scripts to compute Biomdal indices for comparing how
strong is the bimodality across strata.
"

library(multimode)

#-------------------------------------------------------------------
" Bimodality amplitude A_B = (A1 - A_antimode)/ A1
, where A1 is the amplitude of the smaller peak and the A_antimode is
 the amplitude of the deepest point between the two peaks
 A_B is always smaller than 1, hence larger values of A_B
indicate more distinct peaks"
compute_Bimodal_Amplitude <-  function(data_vector,plot_title){
  modes <- locmodes(data_vector,mod0 = 2)
  plot(modes)
  title(main = plot_title)
  if(modes$locations[1]<modes$locations[3]){
    A_1 <- modes$fvalue[1]
  }else{
    A_1 <- modes$fvalue[3]
  }
  A_antimode <- modes$fvalue[2]
  
  Bimodal_Amplitude <- (A_1 - A_antimode) /A_1
  return (Bimodal_Amplitude)
}
