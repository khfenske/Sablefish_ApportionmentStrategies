#==================================================================================================
#Project Name: SABLEFISH APPORTIONMENT - Calculate Selectivity
#Creator: Curry James Cunningham, NOAA/NMFS, ABL
#Date: 2.21.18
#
#Purpose: Function to read demographic parameters for operating model
#
#==================================================================================================
#NOTES:
#  a) survey selectivity is not spatial
#  b) fishery selectivity is spatial
#
#==================================================================================================


#' Calculate Selectivity
#'
#' @param fleet string indicating the name of the fleet (i.e. name of survey or fishery)
#' @param type string identifying if this is survey or fishery selectivity
#'
#' @return
#' @export
#'
#' @examples
calc_selectivity <- function(type=NULL, fleet=NULL) {
  
  ### TESTING ###
  # type <- 'fish'
  # type <- 'surv'
  # fleet <- 'USTrawl'
  # fleet <- 'USLongline'
  # fleet <- 'USJPLL'
  # fleet <- 'USfixed_preIFQ'
  # fleet <- 'USfixed_postIFQ'
  # fleet <- 'Foreign'
  
  
  ###############
  
  # Check Inputs ========================================
  if(! type %in% c('surv.name','fish')) {
    stop(paste("type: '",type,"', must be of: 'surv.name' or 'fish'"))
  }
  if(type=='surv.name') {
    if(! fleet %in% surv.name) { stop(paste("name: '", fleet, "' is not a recognized survey")) }
  }else {
    if(! fleet %in% fish) { stop(paste("name: '", fleet, "' is not a recognized fishery")) }
  }
  
  # Calculate Selectivity ==================================
  if(type=='surv.name') {
    #Get select type
    temp.selex.type <- selex.surv.type$Type[selex.surv.type$Fleet==fleet]
    #Create output object
    output <- array(dim=c(n.sex,n.age), dimnames=list(sexes,ages)) 
    #Calculate Selex
    if(temp.selex.type=='logistic') {
      #Female
      a50 <- as.numeric(in.selex.surv %>% filter(Fleet==fleet, Par=='a50') %>% select(Female))
      delta <- as.numeric(in.selex.surv %>% filter(Fleet==fleet, Par=='delta') %>% select(Female))
      output[1,] <- 1/(1+exp(-delta*(ages-a50)))
      #Male
      a50 <- as.numeric(in.selex.surv %>% filter(Fleet==fleet, Par=='a50') %>% select(Male))
      delta <- as.numeric(in.selex.surv %>% filter(Fleet==fleet, Par=='delta') %>% select(Male))
      output[2,] <- 1/(1+exp(-delta*(ages-a50)))
    }
    if(temp.selex.type=='dome') {
      #Female
      delta <- as.numeric(in.selex.surv %>% filter(Fleet==fleet, Par=='delta') %>% select(Female))
      amax <- as.numeric(in.selex.surv %>% filter(Fleet==fleet, Par=='amax') %>% select(Female))
      p <- 0.5*(sqrt(amax^2+4*delta^2)-amax)
      output[1,] <<- ((ages/amax)^(amax/p))*exp((amax-ages)/p)
      #Male
      delta <- as.numeric(in.selex.surv %>% filter(Fleet==fleet, Par=='delta') %>% select(Male))
      amax <- as.numeric(in.selex.surv %>% filter(Fleet==fleet, Par=='amax') %>% select(Male))
      p <- 0.5*(sqrt(amax^2+4*delta^2)-amax)
      output[2,] <<- ((ages/amax)^(amax/p))*exp((amax-ages)/p)
    }
  }else {
    #Get select type
    temp.selex.type <- selex.fish.type$Type[selex.fish.type$Fleet==fleet]
    #Create output object
    output <- array(dim=c(n.area,n.sex,n.age), dimnames=list(1:n.area,sexes,ages))
    #Calculate selex
    a <- 1
    for(a in 1:n.area) {
      if(temp.selex.type=='logistic') {
        #Female
        a50 <- as.numeric(in.selex.fish %>% filter(Fleet==fleet, Area==a, Par=='a50') %>% select(Female))
        delta <- as.numeric(in.selex.fish %>% filter(Fleet==fleet, Area==a, Par=='delta') %>% select(Female))
        output[a,1,] <- 1/(1+exp(-delta*(ages-a50)))
        #Male
        a50 <- as.numeric(in.selex.fish %>% filter(Fleet==fleet, Area==a, Par=='a50') %>% select(Male))
        delta <- as.numeric(in.selex.fish %>% filter(Fleet==fleet, Area==a, Par=='delta') %>% select(Male))
        output[a,2,] <- 1/(1+exp(-delta*(ages-a50)))
      }
      if(temp.selex.type=='dome') {
        #Female
        delta <- as.numeric(in.selex.fish %>% filter(Fleet==fleet, Area==a, Par=='delta') %>% select(Female))
        amax <- as.numeric(in.selex.fish %>% filter(Fleet==fleet, Area==a, Par=='amax') %>% select(Female))
        p <- 0.5*(sqrt(amax^2+4*delta^2)-amax)
        output[a,1,] <- ((ages/amax)^(amax/p))*exp((amax-ages)/p)
        #Male
        delta <- as.numeric(in.selex.fish %>% filter(Fleet==fleet, Area==a, Par=='delta') %>% select(Male))
        amax <- as.numeric(in.selex.fish %>% filter(Fleet==fleet, Area==a, Par=='amax') %>% select(Male))
        p <- 0.5*(sqrt(amax^2+4*delta^2)-amax)
        output[a,2,] <- ((ages/amax)^(amax/p))*exp((amax-ages)/p)
      }
    }#next a
  }
  return(output)
}