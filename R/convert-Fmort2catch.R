#' Convert Instantaneous Fishing Mortality to Catch
#'  Given an input fishing mortality rate, selectivity, and attributes of the population
#'   this function will return the total catch in kilograms.
#'
#' @param Fmort Input instantaneous fishing mortality rate
#' @param temp.selex Fishery or gear-specific selectivity at age by sex, as a matrix [sex, age]. 
#' @param temp.N Numbers at age by sex, as a matrix [sex,age]. This should reference the previous year. 
#' @param wa Weight at age by sex, as a matrix [sex,age].
#' @param mx Natural mortality at age by sex, as a matrix [sex,age].
#'
#' @return Total catch from the population in kilograms
#' @export
#'
#' @examples
convert_Fmort2catch <- function(Fmort, temp.selex, temp.N, wa=wa, mx=mx) {
  
  ### Testing ###
  # Fmort <- 0.1
  # 
  # temp.area <- 1
  # temp.selex <- selex$fish$USfixed_postIFQ[temp.area,,]
  # 
  # temp.N <- N[,y-1,,temp.area,i] #Sex, year, age, area, sim
  ###############
  
  #Fishing mortality rate at age
  F_at_age <- Fmort*temp.selex
  
  #Total instantaneous mortality
  Z_at_age <- (mx + F_at_age)
  
  # c = N*(F/Z)*(1-exp(-Z))
  
  #Catch at age
  C_at_age <- temp.N * (F_at_age/Z_at_age) * (1-exp(-Z_at_age))
  
  #Exploitation rate by sex and age
  # catch_at_age/temp.N
  
  #Calc total catch (in Kg)
  catch <- sum(C_at_age*wa)
  return(catch)
}


# TESTING ==========================
temp.area <- 1
convert_Fmort2catch(Fmort=0.2, temp.selex=selex$fish$USfixed_postIFQ[temp.area,,],
                    temp.N=N[,y-1,,temp.area,i],
                    wa=wa, mx=mx)
  
