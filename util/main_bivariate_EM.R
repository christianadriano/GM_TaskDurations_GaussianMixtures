"
Gaussian Mixture model for the test duration.

"


source("C://Users//Christian//Documents//GitHub//EM_GaussianMixtureModel_TaskDurations//e_step_ExpectationMaximization.R")
source("C://Users//Christian//Documents//GitHub//EM_GaussianMixtureModel_TaskDurations//m_step_ExpectationMaximization.R")



main <- function(wait,wait.summary.df) {
  
  for (i in 1:100) {
    if (i == 1) {
      # Initialization
      e.step <- e_step(wait, wait.summary.df[["mu"]], wait.summary.df[["std"]],
                       wait.summary.df[["alpha"]])
      m.step <- m_step(wait, e.step[["posterior.df"]])
      cur.loglik <- e.step[["loglik"]]
      loglik.vector <- e.step[["loglik"]]
    } else {
      # Repeat E and M steps till convergence
      e.step <- e_step(wait, m.step[["mu"]], sqrt(m.step[["var"]]), 
                       m.step[["alpha"]])
      m.step <- m_step(wait, e.step[["posterior.df"]])
      loglik.vector <- c(loglik.vector, e.step[["loglik"]])
      
      loglik.diff <- abs((cur.loglik - e.step[["loglik"]]))
      #print(paste0(e.step[["loglik"]],",",cur.loglik,",",loglik.diff))
      if(loglik.diff < 1e-6) {
        break
      } else {
        cur.loglik <- e.step[["loglik"]]
      }
    }
  }
  return (m.step)
}
