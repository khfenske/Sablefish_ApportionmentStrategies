#' Function to create data objects for simulation
#'
#' @return A named list with all of the data structures to be attached after function call
#' @export
#'
create_sim_objects <- function() {
  lz  <<- matrix(1/n.sex,nrow=n.sex,ncol=n.age) 
  za  <<- matrix(0,nrow=n.sex,ncol=n.age)
  
  #Fishery
  qa  <<- array(0,dim=c(n.sex,n.age,n.fish)) 
  pa  <<- array(0,dim=c(n.sex,n.age,n.fish))
  ra  <<- array(0,dim=c(n.sex,n.age,n.fish))
  dlz <<- array(0,dim=c(n.sex,n.age,n.fish))
  
  #========================================================
  #Define Data Structures
  
  sims <<- paste0('sim',c(1:n.sims))
  
  B <<- array(dim=c(n.sex, n.year, n.age, n.area, n.sims), dimnames=list(sexes, years, ages, areas, sims)) #Biomass <- check units
  N <<- array(dim=c(n.sex, n.year, n.age, n.area, n.sims), dimnames=list(sexes, years, ages, areas, sims)) #Numbers <- check units
  C.b <<- array(dim=c(n.sex, n.year, n.age, n.area, n.sims), dimnames=list(sexes, years, ages, areas, sims)) #Catch
  C.n <<- array(dim=c(n.sex, n.year, n.age, n.area, n.sims), dimnames=list(sexes, years, ages, areas, sims)) #Catch 
  harvest.b <<- array(dim=c(n.sex, n.year, n.age, n.fish, n.area, n.sims), dimnames=list(sexes, years, ages, fish, areas, sims))  #Harvest (units) by fish type
  harvest.n <<- array(dim=c(n.sex, n.year, n.age, n.fish, n.area, n.sims), dimnames=list(sexes, years, ages, fish, areas, sims))  #Harvest (units) by gear type
  OM_fixed_catch <<- matrix(nrow=n.year, ncol=n.sims, dimnames=list(years,sims)) #matrix to hold all of the OM fixed gear catch for initial population and simulated years, summed across areas
  OM_trawl_catch <<- matrix(nrow=n.year, ncol=n.sims, dimnames=list(years,sims)) #matrix to hold all of the OM trawl gear catch for initial population and simulated years, summed across areas
  apportioned_C <<- array(dim=c(n.year, n.fish, n.area, n.sims), dimnames=list(years, fish, areas, sims))
  OM_Surv.RPN <<- matrix(nrow=n.year, ncol=n.sims, dimnames=list(years,sims)) #matrix to hold all of the OM survey RPN data, summed across areas
  OM_Fish.RPW <<- matrix(nrow=n.year, ncol=n.sims, dimnames=list(years,sims)) #matrix to hold all of the OM fishery RPW data, sumed across areas
  OM_Fish.RPW.age <<- array(dim=c(n.year, n.age, n.sims),dimnames=list(years, ages, sims)) #array to hold the OM fishery age comps, summed across areas
  OM_Surv.RPN.age <<- array(dim=c(n.year, n.age, n.sims),dimnames=list(years, ages, sims)) #array to hold the OM survey age comps, summed across areas
  ABC_TS <<- array(dim=c(n.year, n.area, n.sims),dimnames=list(years, areas, sims)) #array to hold the apportioned ABC time series

  #Total Instantaneous mortality
  Z.a <<- array(dim=c(n.sex, n.year, n.age, n.area, n.sims), dimnames=list(sexes, years, ages, areas, sims)) 
  F.a <<- array(dim=c(n.sex, n.year, n.age, n.area, n.sims), dimnames=list(sexes, years, ages, areas, sims)) #Age-specific Fishing mortality
  F.mort <<- array(dim=c(n.fish, n.year, n.area, n.sims), dimnames=list(fish, years, areas, sims)) #Annual Fishing mortality
  
  #Continuous
  surv <<- array(dim=c(n.sex, n.year, n.age, n.area, n.sims), dimnames=list(sexes, years, ages, areas, sims))
  mort <<- array(dim=c(n.sex, n.year, n.age, n.area, n.sims), dimnames=list(sexes, years, ages, areas, sims))
  
  
  #Female spawning-stock biomass
  ssb <<- array(dim=c(n.sex, n.age, n.year, n.area, n.sims), dimnames=list(sexes, ages, years, areas, sims)) 
  #Recruitment
  #rec <<- array(dim=c(n.sex, n.year, n.area, n.sims), dimnames=list(sexes, years, areas, sims))
  recruits.area <<- array(dim=c(n.year, n.area, n.sims), dimnames=list(years, areas, sims))
  
  #Assessment data inputs
  # longline survey RPN, 'current' year
  Surv.RPN <<- array(dim=c(n.sex, n.year, n.age, n.area, n.sims), dimnames=list(sexes,years,ages,areas,sims)) #longline survey RPN
  # longline/fixed gear fishery CPUE/RPW
  Fish.RPW <<- array(dim=c(n.sex, n.year, n.age, n.area, n.sims), dimnames=list(sexes,years,ages,areas,sims)) #fixed gear fishery RPW
  # longline/fixed gear fishery age comps
  Fish.AC <<- array(dim=c(n.sex, n.year, n.age, n.area, n.sims), dimnames=list(sexes, years,ages,areas,sims))
  # longline survey age comps
  Surv.AC <<-  array(dim=c(n.sex, n.year, n.age, n.area, n.sims), dimnames=list(sexes, years,ages,areas,sims))
  # longline/fixed gear fishery length comps,
  #Fxfish.LC <<- array(dim=c(n.sex, n.year, n.length, n.area, n.sims), dimnames=list(sexes,years,len,areas,sims))  
  # trawl gear fishery length comps
  #Trfish.LC <<- array(dim=c(n.sex, n.year, n.length, n.area, n.sims), dimnames=list(sexes,years,len,areas,sims))
  # longline survey length comps, 'current' year
  #Fxsurv.LC <<- array(dim=c(n.sex, n.year, n.length, n.area, n.sims), dimnames=list(sexes,years,len,areas,sims))
  
  #Return section  #what does this section do?
   #out <- NULL
   #ut$lz <- lz
   #out$za <- za
   #out$qa <- qa
   #out$pa <- pa
   #out$ra <- ra
   #out$dlz <- dlz
   #out$B <- B
   #out$N <- N
   #out$C.b <- C.b
   #out$C.n <- C.n
   #out$harvest.b <- harvest.b
   #out$harvest.n <- harvest.n
   #out$Z.a <- Z.a
   #out$F.a <- F.a
   #out$Fmort <- Fmort
   #out$surv <- surv
   #out$mort <- mort
   #out$ssb <- ssb
   #out$rec <- rec
   #return(out)
  
}

#testing_file <- create_sim_objects()  #why doesn't this work?
#write.csv(testing_file, "testing_file.csv")
