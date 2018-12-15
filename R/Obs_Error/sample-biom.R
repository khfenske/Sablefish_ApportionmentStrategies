#' Sample biomass time series with observation error (RPW index of abundance/biomass)
#'
#' @param true.values Vector of true abundance or biomass.
#' @param sigma Standard deviation of observation error.
#' @param type Functional form of observation error, one of: 'lognorm', 'norm', or 'pois'. Options are log-normal, normal, Poisson.
#' @param seed When specified, the seed dictates consistent random number generation, for use across replicate simulations.
#'
#' @return Observed time series of abundance or biomass with randomly distributed error
#' @export
#'
#' @examples

#true.values <- B

sample_biom <- function(true.values, sigma=NULL, type=NULL, seed=NULL) {
  #Checking for inputs
  #if(type %in% c('lognorm', 'norm', 'pois')) { stop("Please specify type as one of: lognorm, norm, or poisson.") } #commented this out because it was giving an error with it in
  if(is.null(sigma) & type%in%c('lognorm','norm')) { stop("Please specify a level of observation error, as either a CV or sigma.") }
  
  #Set the seed if necessary - only used when same observation process desired across replicates
  if(!is.null(seed)) { set.seed(seed) }
  
  obs.values <- NULL 
        if(type=='lognorm') {
        obs.values <- true.values*exp(rnorm(n=length(true.values), mean=0,sd=sigma)-sigma^2/2) 
          }
        if(type=='norm') {
        obs.values <- true.values*rnorm(n=length(true.values), mean=1,sd=sigma)  #need something to keep the values positive??
          }
        if(type=='pois') {
        obs.values <- rpois(n=length(true.values), true.values)
          }
  return(obs.values)
} #close function

