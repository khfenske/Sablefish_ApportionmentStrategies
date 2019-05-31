#' Convert Catch (Biomass) to a Fishing Mortality Rate (F_t)
#'  This function will attempt to find an instantaneous fishing mortality rate for a specific fleet or gear
#'    which results in a catch biomass (summed across sexes and ages) equal to the input **catch** argument.
#'
#' @param temp.selex Fishery or gear-specific selectivity at age by sex, as a matrix [sex, age].
#' @param temp.N Numbers at age by sex, as a matrix [sex,age]. This should reference the previous year. 
#' @param wa Weight at age by sex, as a matrix [sex,age].
#' @param catch The target catch (biomass, kg) for a given fleet or gear, which is to be matched by adjusting Fmort
#' @param bisection Boolean describing whether bisection or optimize will be used.
#' @param mx Natural mortality at age by sex, as a matrix [sex,age].
#'
#' @return temp.Fmort - The fishing mortality rate that 
#' @export
#'
#' @examples
#' 
estimate_Fmort4catch <- function(catch, temp.selex, temp.N, wa=wa, mx=mx, bisection=TRUE) {
  
  ### Testing ###
  # temp.area <- 1
  # temp.selex <- va[2,1,,]
  # catch <- (temp.catchnumbiom[1]*1000000) #kg
  # temp.N <- N[,2,,temp.area,1] #Sex, year, age, area, sim
  # bisection <- FALSE
  ###############
  
  if(bisection==TRUE) {
    # Bisection ===================================
    #  Works by iteratively adjusting bounds depending on where
    #   current estimate is relative to the objective (catch)
    range <- vector(length=2)
    range[1] <- 1e-3
    range[2] <- 1

    #Iterate
    i <- 1
    for(i in 1:30) {
      #MIDPOINT
      midpoint <- mean(range)
      
      #Calculate predicted catch for a trial F rate (midpoint of range)
      pred_catch <- convert_Fmort2catch(Fmort=midpoint, 
                                        temp.selex=temp.selex, 
                                        temp.N=temp.N, 
                                        wa=wa, mx=mx)
      if(pred_catch < catch) {
        range[1] <- midpoint
        range[2] <- range[2]
      }else {
        range[1] <- range[1]
        range[2] <- midpoint
      }
      
      # KARI: Uncomment me to see how bisection marches toward success.
       #print(range)
       #print(pred_catch)
    }#next i
    
    #Extract result to return ============
    Fmort <- midpoint
    #Add back to function
    pred_catch <- convert_Fmort2catch(Fmort=midpoint, # Optimized Solution
                                      temp.selex=temp.selex,
                                      temp.N=temp.N,
                                      wa=wa, mx=mx)
  }else {
    # Optimize  ============================
    require(stats)
    
    uni.func <- function(Fmort) {
      pred_catch <- convert_Fmort2catch(Fmort=Fmort, 
                                          temp.selex=temp.selex, 
                                          temp.N=temp.N, 
                                          wa=wa, mx=mx)
      #Difference between pred_catch and target (catch)
      diff <- (pred_catch - catch)^2
      return(diff)
    }
    
    #Test
    # uni.func(Fmort=0.1)
    
    #Do Optimization
    fit <- optimize(f=uni.func, interval=c(1e-3,1), tol=1e-10)

    #Extract result to return ============
    Fmort <- fit$minimum
    #Add back to function
    pred_catch <- convert_Fmort2catch(Fmort=fit$minimum, # Optimized Solution
                                      temp.selex=temp.selex,
                                      temp.N=temp.N,
                                      wa=wa, mx=mx)
    # catch #Target
  }
  
  
  #Return Section
  out <- NULL
  out$Fmort <- Fmort
  out$pred_catch <- pred_catch
  return(out)
}



# TESTING ==========================
# temp.area <- 1
# temp.selex <- selex$fish$USfixed_postIFQ[temp.area,,]
# 
# catch <- 1e7 #kg
# temp.N <- N[,y-1,,temp.area,i] #Sex, year, age, area, sim
# bisection <- TRUE
# 
 estimate_Fmort4catch(catch=catch, temp.selex=temp.selex, 
                      temp.N=temp.N, wa=wa, mx=mx, bisection=bisection)
# 
