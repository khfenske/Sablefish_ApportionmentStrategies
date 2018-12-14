#' Function to create data objects for simulation
#'
#' @return A named list with all of the data structures to be attached after function call
#' @export
#'
create_sim_objects <- function() {
  lz  <<- matrix(1/n.sex,nrow=n.sex,ncol=n.age) #what are these??? need names/labels
  za  <<- matrix(0,nrow=n.sex,ncol=n.age)
  
  #Fishery
  qa  <<- array(0,dim=c(n.sex,n.age,n.fish)) #what are these???
  pa  <<- array(0,dim=c(n.sex,n.age,n.fish))
  ra  <<- array(0,dim=c(n.sex,n.age,n.fish))
  dlz <<- array(0,dim=c(n.sex,n.age,n.fish))
  
  #========================================================
  #Define Data Structures
  
  sims <<- paste0('sim',c(1:n.sims))
  
  B <<- array(dim=c(n.sex, n.year, n.age, n.area, n.sims), dimnames=list(sexes, years, ages, areas, sims)) #Biomass (pounds)
  N <<- array(dim=c(n.sex, n.year, n.age, n.area, n.sims), dimnames=list(sexes, years, ages, areas, sims)) #Numbers
  C.b <<- array(dim=c(n.sex, n.year, n.age, n.area, n.sims), dimnames=list(sexes, years, ages, areas, sims)) #Catch (lbs)
  C.n <<- array(dim=c(n.sex, n.year, n.age, n.area, n.sims), dimnames=list(sexes, years, ages, areas, sims)) #Catch (number)
  harvest.b <<- array(dim=c(n.sex, n.year, n.age, n.fish, n.area, n.sims), dimnames=list(sexes, years, ages, fish, areas, sims))  #Harvest (lbs) by fish type
  harvest.n <<- array(dim=c(n.sex, n.year, n.age, n.fish, n.area, n.sims), dimnames=list(sexes, years, ages, fish, areas, sims))  #Harvest (number) by gear type
  
  #Total Instantaneous mortality
  Z.a <<- array(dim=c(n.sex, n.year, n.age, n.area, n.sims), dimnames=list(sexes, years, ages, areas, sims)) 
  F.a <<- array(dim=c(n.sex, n.year, n.age, n.area, n.sims), dimnames=list(sexes, years, ages, areas, sims)) #Age-specific Fishing mortality
  Fmort <<- array(dim=c(n.fish, n.year, n.area, n.sims), dimnames=list(fish, years, areas, sims)) #Annual Fishing mortality
  
  #Continuous
  surv <<- array(dim=c(n.sex, n.year, n.age, n.area, n.sims), dimnames=list(sexes, years, ages, areas, sims))
  mort <<- array(dim=c(n.sex, n.year, n.age, n.area, n.sims), dimnames=list(sexes, years, ages, areas, sims))
  
  #Recruitment
  ssb <<- array(dim=c(n.sex, n.age, n.year, n.area, n.sims), dimnames=list(sexes, ages, years, areas, sims)) #Female spawning-stock biomass
  rec <<- array(dim=c(n.sex, n.year, n.area, n.sims), dimnames=list(sexes, years, areas, sims))

  #Assessment data inputs
  # longline survey RPN, 'current' year, for 6 areas then combine to 3
  Surv.RPN <<- array(dim=c(n.sex, n.year, n.age, n.area, n.sims), dimnames=list(sexes,years,ages,areas,sims)) #longline survey RPN
  # longline/fixed gear fishery CPUE/RPW, lagged 1 year, for 6 areas then combine to 3
  Fish.RPW <<- array(dim=c(n.sex, n.year, n.age, n.area, n.sims), dimnames=list(sexes,years,ages,areas,sims)) #fixed gear fishery RPW
  # longline/fixed gear fishery age comps, lagged 1 year, for 6 areas then combine to 3, single sex
  Fish.AC <<- array(dim=c(n.sex, n.year, n.age, n.area, n.sims), dimnames=list(sexes,years,ages,areas,sims))
  # longline survey age comps, lagged 1 year, for 6 areas then combine to 3, single sex
  Surv.AC <<-  array(dim=c(n.sex, n.year, n.age, n.area, n.sims), dimnames=list(sexes,years,ages,areas,sims))
  # longline/fixed gear fishery length comps, lagged 1 year, for 6 areas then combine to 3, two sexes
  Fxfish.LC <<- array(dim=c(n.sex, n.year, n.length, n.area, n.sims), dimnames=list(sexes,years,len,areas,sims))  
  # trawl gear fishery length comps, lagged 1 year, for 6 areas then combine to 3, two sexes
  Trfish.LC <<- array(dim=c(n.sex, n.year, n.length, n.area, n.sims), dimnames=list(sexes,years,len,areas,sims))
  # longline survey length comps, 'current' year, for 6 areas then combine to 3, two sexes
  Fxsurv.LC <<- array(dim=c(n.sex, n.year, n.length, n.area, n.sims), dimnames=list(sexes,years,len,areas,sims))
  
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