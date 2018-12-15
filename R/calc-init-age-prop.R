#' Calculates initial sex-specific proportions of biomass at age, based on unfished equilibrium
#'
#' @return init.prop - matrix of proportions by sex and age
#' @export
#'
#for testing
#bo=16.5
#a=2
#m=2

calc_init_age_prop <- function(bo) {
  #init.prop <- matrix(nrow=n.sex, ncol=n.age, dimnames=list(sexes,ages))
  init.prop <- array(dim=c(n.sex,n.age,n.area), dimnames=list(sexes,ages,areas))
  
  for(m in 1:n.area) {
  for(a in 1:n.age) {
    if(a==1) {
      init.prop[,a,m] <- bo#*mx[,a]
    }else {
      init.prop[,a,m] <- bo*exp(-(a-1)*mx[,a]) #mx is mortality at age
    }
    if(a==A) {
      init.prop[,a,m] <- init.prop[,a,m]/(1-exp(-mx[,a]))
    }
  } #close age
  #init.prop[,,m] <- init.prop[,,m] * wa  
  init.prop[,,m] <- init.prop[,,m]/sum(init.prop[,,m]) #proportion of numbers at age for each area
  } #close area  

  #Multiply by spawning sbpr @ age
  # init.prop <- init.prop

  return(init.prop)
  
} #close function
