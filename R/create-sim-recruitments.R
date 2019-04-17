#' Simulate Annual Recruitments
#' Creates an object [N_sims X N_years] of recruitment values
#'
#' @param mu_rec - Log mean recruitment, from extract_pars()
#' @param sigma_rec - Log standard deviation of rectuitment, from: extract_pars()
#' @param rho_rec - Autocorrelation parameter for recruitments. Set to NULL to ignore.
#' @param n.year - Number of years in simulation
#' @param n.sims - Number of simulations (realizations)
#' @param seed - Seed for random number generator
#'
#' @return - Exports matrix recruitments[n.sims, n.year] to global environment.
#' @export
#'
#' @examples
create_sim_recruitments <- function(mu_rec, sigma_rec, rho_rec=NULL, n.year, n.sims, seed=101) {
  ### TESTING ###
  # mu_rec <- 16.5
  # sigma_rec <- 0.5
  # rho_rec <- 0.9
  # # n.year <- 50
  # # n.sims <- 100
  # seed <- 101
  ##############
  
  #NOTE: These recruitments will be common across management strategies to ensure consistency in comparisons. 
  set.seed(seed)
  
  rec <<- array(dim=c(n.sims, n.year))
  
  i <- 1
  for(i in 1:n.sims) {
    devs <- rnorm(n.year, 0, sigma_rec)
    
    #If recruitment is NOT autocorrelated
    if(is.null(rho_rec)) {
      rec[i,] <<- exp(mu_rec)*exp(devs - ((sigma_rec^2)/2)) #Global
    }else { #Recruitment is autocorrelated
      cor.devs <- vector(length=n.year) #Past Correlated deviations Deviations
      y <- 1
      for(y in 1:n.year) {
        if(y==1) {
          cor.devs[y] <- devs[y] #* sqrt(1-rho_rec^2) #Double check.
          rec[i,y] <<- exp(mu_rec)*exp(cor.devs[y] - ((sigma_rec^2)/2)) #Global
        }else{
          cor.devs[y] <- rho_rec*cor.devs[y-1] + devs[y]*sqrt(1-rho_rec^2)
          rec[i,y] <<- exp(mu_rec)*exp(cor.devs[y] - ((sigma_rec^2)/2)) #Global
        }
      } #next y
    }
  } # next i 
  # return(rec) - no return necessary as we save to global env <<-:
}

