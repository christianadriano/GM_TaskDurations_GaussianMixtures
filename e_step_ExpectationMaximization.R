#Source: https://tinyheero.github.io/2016/01/03/gmm-em.html
#' Expectation Step of the EM Algorithm
#'
#' Calculate the posterior probabilities (soft labels) that each component
#' has to each data point.
#'
#' @param sd.vector Vector containing the standard deviations of each component
#' @param sd.vector Vector containing the mean of each component
#' @param alpha.vector Vector containing the mixing weights  of each component
#' @return Named list containing the loglik and posterior.df
e_step <- function(x, mu.vector, sd.vector, alpha.vector) {
  comp1.prod <- dnorm(x, mu.vector[1], sd.vector[1]) * alpha.vector[1]
  comp2.prod <- dnorm(x, mu.vector[2], sd.vector[2]) * alpha.vector[2]
  sum.of.comps <- comp1.prod + comp2.prod
  comp1.post <- comp1.prod / sum.of.comps
  comp2.post <- comp2.prod / sum.of.comps
  
  sum.of.comps.ln <- log(sum.of.comps, base = exp(1))
  sum.of.comps.ln.sum <- sum(sum.of.comps.ln)
  
  list("loglik" = sum.of.comps.ln.sum,
       "posterior.df" = cbind(comp1.post, comp2.post))
}