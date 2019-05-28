#' Sample age composition data with multinomial or dirichlet error.
#'
#' @param true.comps Matrix of age composition: rows=number of samples, cols=number of age classes (could I get an example of what this looks like?)
#' @param Nsamp Vector of sample sizes of length equal to number of samples
#' @param type Type of error, of type: 'multinomial' or 'dirichlet'
#' @param cpar WHAT IS CPAR??
#'
#' @return OBSERVED AGE COMPOSITIONS? WHAT DIMENSIONS? in numbers??
#' @export
#'
#' @examples
sample_age_comps <- function(true.props, Nsamp, cpar=NULL, seed=NULL) {
  #require(gtools) #will call this at the start of the Run-sablefish-sim.R instead of here
  if(!is.null(seed)) { set.seed(seed) } #Allow code to set the seed for random draw
  
    #Initial Checking
    # cpar - A value of 1 indicates the same standard deviation as a multinomial of the given Nsamp, 2 indicates twice, etc. 
    # Values greater than one indicate overdispersion, and less indicate underdispersion.
  
  #cpar <- 2  #  <- is this for testing??
  #True data
  probs <- as.numeric(true.props/sum(true.props))
  ## If cpar is NA this signifies to use the multinomial 
  if(is.null(cpar)){ #MULTINOMIAL
    obs.comp <- rmultinom(1, size=Nsamp, prob=probs)[,1]/Nsamp  
    effectiveN <- Nsamp
  } else { # use Dirichlet
    lambda <- Nsamp/cpar^2 - 1
    if(lambda<0)
      stop(paste("Invalid Dirichlet parameter: Lambda=", lambda))
    obs.comp <- gtools::rdirichlet(1,probs * lambda)
    ## Use the effective sample size when using Dirichlet
    effectiveN <- Nsamp/cpar^2
  }
  # Return Section
  out <- NULL
  out$obs.comp <- obs.comp
  out$Nsamp <- Nsamp
  out$effectiveN <- effectiveN
  return(out)
}

#testing
# true.props = c(1,10,5,1)
# Nsamp = 100
# cpar = NULL
# sample_age_comps(true.props,Nsamp,cpar=cpar)
