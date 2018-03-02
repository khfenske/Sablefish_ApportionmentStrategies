#KHF 11-24-17
#Apportionment functions

#master list of things called in all the following functions (in case we do one big switch)
#ABC.total - total ABC summed across n regions
#n.areas - number of OM or management areas
#fixed_prop - vector of the fixed apportionment values (since 2012/2013) as proportions (1-n.areas)
#equilib_prop - vector of the equilibrium of movement matrix (stationary distribution of movement matrix), from 1 to n.areas
#SKIP THIS, not using a catch based method...catch.data - a matrix of catch data with nrows=length of your modeled catch, ncol =n.areas
#SKIP THIS, not using a catch based method...catch.years - the number of years you want to average for your apportionment (such as 3 or 5), it pulls that number of the most recent catches
#biom.data - a matrix of biomass data (such as from a survey) with nrows=length of your survey data, ncol=n.areas
#fish.data - a matris of fishery CPUE (logbook?) data with nrows=years of data, and ncol=n.areas
#fixed_values - x1 and x2, input two values for apportionment proportion for Bering Sea (value 1) and Aleutian Islands (value 2)
#penalty - a value indicating the max allowed increase or decrease between ABC in terminal year and next projected year (e.g. input as 0.05 for 5% allowed change)
#lastYr_ABCs - vector of the most recent ABC (lenght=n.areas), for calculation of percent difference between current and next projected ABC
#LLlencomp <-longline survey length comps in numbers for n.Lbins and n.areas
#L50_mat <- row # in LLlencomp which contains the length cutoff for length based apportionment.
#biom.cv.data <- survey biomass CV data

##NOTE - all apportionment options will need to have the 95:5 hook and line:trawl split for WY and EY/SEO built in prior to use in the simulations

#some temporary things to make sure coded functions work...
ABC <-  as.vector(c(40,40,20)) #vector with number of dimensions equal to the number of areas in the EM
ABC.total.test <- sum(ABC) #summed abc from projection for all EM areas (may be same as ABC in an EM with 1 area)
n.areas.test <-  6 #number of areas used for management of the species


#7 - Equal apportionment to each of the n management areas
equal_apportionment <- function(ABC.total,n.areas) {
  ABC.EM <- vector(length=n.areas)
  for (a in 1:n.areas) {
    ABC.EM[a]<-ABC.total/n.areas
  }
  return(ABC.EM)
}

equal_apportionment(ABC.total.test,n.areas.test)


#13 - fixed at current fixed values
current.test <- c(0.10,0.13,0.11,0.34,0.11,0.21) #rough spatial apportionment proportions after whales, BS-AI-WG-CG-WY-EY
#probably should use the before whale correction values in the real simulation
fixed_apportionment <- function(ABC.total,n.areas,fixed_prop) {
  ABC.EM <- vector(length=n.areas)
  for (a in 1:n.areas) {
    ABC.EM[a]<-ABC.total*fixed_prop[a]
  }
  return(ABC.EM)
}

fixed_apportionment(ABC.total.test,n.areas.test,current.test)


#14 - equilibrium (stationary movement matrix)
equilib.test <- c(0.092315344,0.136703284,0.13052393,0.268275823,0.137707199,0.234474419) #stationary distribution of movement matrix, BS-AI-WG-CG-WY-EY
#will need to come up with a better way to split Ey and WY from EG - because right now it's a kludge. (or just lump them)
equilib_apportionment <- function(ABC.total,n.areas,equilib_prop) {
  ABC.EM <- vector(length=n.areas)
  for (a in 1:n.areas) {
    ABC.EM[a]<-ABC.total*equilib_prop[a]
  }
  return(ABC.EM)
}

equilib_apportionment(ABC.total.test,n.areas.test,equilib.test)


#12 - catch based, 3 (or other) year average of each area
#make up catch data for each of n areas for now
#THIS ONE IS NOT GOING TO BE DONE IN OUR SIMULATION WORK, KEPT FOR POSTERITY
catch.n.years <- 10
catch.test.data <- matrix(data=NA,nrow=catch.n.years,ncol=n.areas.test)
catch.test.data[,1] <- c(1000*abs(rnorm(catch.n.years,0.06,2.0)))  
catch.test.data[,2] <- c(1000*abs(rnorm(catch.n.years,1.0,2.0))) 
catch.test.data[,3] <- c(1000*abs(rnorm(catch.n.years,1.4,1.5))) 
catch.test.data[,4] <- c(1000*abs(rnorm(catch.n.years,2.5,0.2))) 
catch.test.data[,5] <- c(1000*abs(rnorm(catch.n.years,1.1,0.8))) 
catch.test.data[,6] <- c(1000*abs(rnorm(catch.n.years,2.0,0.4))) 

catchbased_apportionment <- function(ABC.total,n.areas,catch.data,catch.years) { 
  ABC.EM <- vector(length=n.areas)
  catch.sum <- vector(length=n.areas)
  catch.prop <- vector(length=n.areas)
  temp.years <- length(catch.data[,1])-(catch.years-1)
  for (a in 1:n.areas) {
    catch.sum[a] <- sum(catch.data[temp.years:length(catch.data[,1]),a]) #the sum of catches for area n for years of interest for apportionment
  }
  for (a in 1:n.areas) {
    catch.prop[a] <- catch.sum[a]/sum(catch.sum)
    ABC.EM[a]<-ABC.total*catch.prop[a]
  }
  return(ABC.EM)
  #return(catch.sum)
  #return(catch.prop)
}

catchbased_apportionment(ABC.total.test,n.areas.test,catch.test.data,3)


#5 - terminal year survey biomass proportions
#make up survey biomass data for each of n areas for now
#will need to deal with alternating survey for BS and AI
biomass.n.years <- 10
biomass.test.data <- matrix(data=NA,nrow=biomass.n.years,ncol=n.areas.test)
biomass.test.data[,1] <- c(1000*abs(rnorm(biomass.n.years,0.06,2.0)))  
biomass.test.data[,2] <- c(1000*abs(rnorm(biomass.n.years,1.0,2.0))) 
biomass.test.data[,3] <- c(1000*abs(rnorm(biomass.n.years,1.4,1.5))) 
biomass.test.data[,4] <- c(1000*abs(rnorm(biomass.n.years,2.5,0.2))) 
biomass.test.data[,5] <- c(1000*abs(rnorm(biomass.n.years,1.1,0.8))) 
biomass.test.data[,6] <- c(1000*abs(rnorm(biomass.n.years,2.0,0.4))) 

biomassbased_apportionment <- function(ABC.total,n.areas,biom.data) { 
  ABC.EM <- vector(length=n.areas)
  biom <- vector(length=n.areas)
  biom.prop <- vector(length=n.areas)
  biom.yrs <- length(biom.data[,1])
  for (a in 1:n.areas) {
    biom[a] <- biom.data[biom.yrs,a] 
  }
  for (a in 1:n.areas) {
    biom.prop[a] <- biom[a]/sum(biom)
    ABC.EM[a]<-ABC.total*biom.prop[a]
  }
  return(ABC.EM)
  #return(biom.sum)
  #return(biom.prop)
}

biomassbased_apportionment(ABC.total.test,n.areas.test,biomass.test.data)



#1 - status quo apportionment (5 year exponential weighting of survey and fishery)
#survey data is given 2x the weight of fishery data
#generate some fake CPUE index data for a fishery and for a survey
#area order should be BS - AI - WG - CG - EG (or if 6 areas, WY - EYSEO instead of EG)
fishery.n.years <- 10
fish.test.data <- matrix(data=NA,nrow=fishery.n.years,ncol=n.areas.test) #fake fishery RPW data
fish.test.data[,1] <- c(1000*abs(rnorm(fishery.n.years,0.06,2.0)))  
fish.test.data[,2] <- c(1000*abs(rnorm(fishery.n.years,1.0,2.0))) 
fish.test.data[,3] <- c(1000*abs(rnorm(fishery.n.years,1.4,1.5))) 
fish.test.data[,4] <- c(1000*abs(rnorm(fishery.n.years,2.5,0.2))) 
fish.test.data[,5] <- c(1000*abs(rnorm(fishery.n.years,1.1,0.8))) 
fish.test.data[,6] <- c(1000*abs(rnorm(fishery.n.years,2.0,0.4))) 

biomass.n.years <- 10
biomass.test.data <- matrix(data=NA,nrow=biomass.n.years,ncol=n.areas.test)
biomass.test.data[,1] <- c(1000*abs(rnorm(biomass.n.years,0.06,2.0)))  #fake longline survey RPN data (how to reconcile between RPN and RPW? Do I need to?)
biomass.test.data[,2] <- c(1000*abs(rnorm(biomass.n.years,1.0,2.0))) 
biomass.test.data[,3] <- c(1000*abs(rnorm(biomass.n.years,1.4,1.5))) 
biomass.test.data[,4] <- c(1000*abs(rnorm(biomass.n.years,2.5,0.2))) 
biomass.test.data[,5] <- c(1000*abs(rnorm(biomass.n.years,1.1,0.8))) 
biomass.test.data[,6] <- c(1000*abs(rnorm(biomass.n.years,2.0,0.4))) 


NPFMC_apportionment <- function(ABC.total,n.areas,fish.data,biom.data) { 
  ABC.EM <- vector(length=n.areas) #creating the output vector to hold apportioned ABCs
  fish.data.prop <- matrix(data=NA, ncol=n.areas,nrow=5)
  fish.data.prop.wt <- matrix(data=NA, ncol=n.areas,nrow=5)
  fish.prop.sum <- vector(length=n.areas)
  biom.data.prop <- matrix(data=NA, ncol=n.areas,nrow=5)
  biom.data.prop.wt <- matrix(data=NA, ncol=n.areas,nrow=5)
  biom.prop.sum <- vector(length=n.areas)
  wts <- c(0.0625, 0.0625, 0.125, 0.25, 0.5) #the weighting values
  EAprop<-24632/(21820+24632) #proportion of western aleutians to easter aleutians, applied to fishery RPW because
  #the fishery fishes farther along the aleutians than the survey or than the AI management area??
  fish.data[,2] <- fish.data[,2]*(c(rep(EAprop,times=length(fish.data[,2])))) # multiply the fishery data by the E-W aleutians proportion
  
  for (i in (length(fish.data[,1])-4):length(fish.data[,1])) {
    for (a in 1:n.areas) {
      m <- i-(length(fish.data[,1])-5)
      fish.data.prop[m,a] <- fish.data[i,a]/sum(fish.data[i,]) #calc proportion by year across areas for fishery data
      biom.data.prop[m,a] <- biom.data[i,a]/sum(biom.data[i,]) #calc proportion by year across areas for survey data
    }
  }
  for (i in 1:length(fish.data.prop[,1])) {
    for (a in 1:n.areas) {    
      fish.data.prop.wt[i,a] <- fish.data.prop[i,a]*wts[i]
      biom.data.prop.wt[i,a] <- biom.data.prop[i,a]*wts[i]    
    }
  }  
  fish.prop.sum <- colSums(fish.data.prop.wt)
  biom.prop.sum <- colSums(biom.data.prop.wt)  
  for (a in 1:n.areas) {
    ABC.EM[a] <- ABC.total * ((fish.prop.sum[a]+(2*biom.prop.sum[a]))/3)  #weighting 2x suvery:1x fishery happens here
  }
  return(ABC.EM)
}

NPFMC_apportionment(ABC.total.test,n.areas.test,fish.test.data, biomass.test.data)



#2 - Survey 5 year exponential weighting
biomass.n.years <- 10
biomass.test.data <- matrix(data=NA,nrow=biomass.n.years,ncol=n.areas.test)
biomass.test.data[,1] <- c(1000*abs(rnorm(biomass.n.years,0.06,2.0)))  #fake longline survey RPN data (how to reconcile between RPN and RPW? Do I need to?)
biomass.test.data[,2] <- c(1000*abs(rnorm(biomass.n.years,1.0,2.0))) 
biomass.test.data[,3] <- c(1000*abs(rnorm(biomass.n.years,1.4,1.5))) 
biomass.test.data[,4] <- c(1000*abs(rnorm(biomass.n.years,2.5,0.2))) 
biomass.test.data[,5] <- c(1000*abs(rnorm(biomass.n.years,1.1,0.8))) 
biomass.test.data[,6] <- c(1000*abs(rnorm(biomass.n.years,2.0,0.4))) 


expsurvwt_apportionment <- function(ABC.total,n.areas,biom.data) { 
  ABC.EM <- vector(length=n.areas) #creating the output vector to hold apportioned ABCs
  biom.data.prop <- matrix(data=NA, ncol=n.areas,nrow=5)
  biom.data.prop.wt <- matrix(data=NA, ncol=n.areas,nrow=5)
  biom.prop.sum <- vector(length=n.areas)
  wts <- c(0.0625, 0.0625, 0.125, 0.25, 0.5) #the weighting values
  
  for (i in (length(biom.data[,1])-4):length(biom.data[,1])) {
    for (a in 1:n.areas) {
      m <- i-(length(biom.data[,1])-5)
      biom.data.prop[m,a] <- biom.data[i,a]/sum(biom.data[i,]) #calc proportion by year across areas for survey data
    }
  }
  for (i in 1:length(biom.data.prop[,1])) {
    for (a in 1:n.areas) {    
      biom.data.prop.wt[i,a] <- biom.data.prop[i,a]*wts[i]    
    }
  }  
  biom.prop.sum <- colSums(biom.data.prop.wt)  
  for (a in 1:n.areas) {
    ABC.EM[a] <- ABC.total * biom.prop.sum[a]  
  }
  return(ABC.EM)
}

expsurvwt_apportionment(ABC.total.test,n.areas.test,biomass.test.data)




#3 - fishery 5 year exponential weighting
#generate some fake CPUE index data for a fishery and for a survey
#area order should be BS - AI - WG - CG - EG (or if 6 areas, WY - EYSEO instead of EG)
fishery.n.years <- 10
fish.test.data <- matrix(data=NA,nrow=fishery.n.years,ncol=n.areas.test) #fake fishery RPW data
fish.test.data[,1] <- c(1000*abs(rnorm(fishery.n.years,0.06,2.0)))  
fish.test.data[,2] <- c(1000*abs(rnorm(fishery.n.years,1.0,2.0))) 
fish.test.data[,3] <- c(1000*abs(rnorm(fishery.n.years,1.4,1.5))) 
fish.test.data[,4] <- c(1000*abs(rnorm(fishery.n.years,2.5,0.2))) 
fish.test.data[,5] <- c(1000*abs(rnorm(fishery.n.years,1.1,0.8))) 
fish.test.data[,6] <- c(1000*abs(rnorm(fishery.n.years,2.0,0.4))) 

expfishwt_apportionment <- function(ABC.total,n.areas,fish.data) { 
  ABC.EM <- vector(length=n.areas) #creating the output vector to hold apportioned ABCs
  fish.data.prop <- matrix(data=NA, ncol=n.areas,nrow=5)
  fish.data.prop.wt <- matrix(data=NA, ncol=n.areas,nrow=5)
  fish.prop.sum <- vector(length=n.areas)
  wts <- c(0.0625, 0.0625, 0.125, 0.25, 0.5) #the weighting values
  EAprop<-24632/(21820+24632) #proportion of western aleutians to easter aleutians, applied to fishery RPW because
  #the fishery fishes farther along the aleutians than the survey or than the AI management area??
  fish.data[,2] <- fish.data[,2]*(c(rep(EAprop,times=length(fish.data[,2])))) # multiply the fishery data by the E-W aleutians proportion
  
  for (i in (length(fish.data[,1])-4):length(fish.data[,1])) {
    for (a in 1:n.areas) {
      m <- i-(length(fish.data[,1])-5)
      fish.data.prop[m,a] <- fish.data[i,a]/sum(fish.data[i,]) #calc proportion by year across areas for fishery data
    }
  }
  for (i in 1:length(fish.data.prop[,1])) {
    for (a in 1:n.areas) {    
      fish.data.prop.wt[i,a] <- fish.data.prop[i,a]*wts[i]
    }
  }  
  fish.prop.sum <- colSums(fish.data.prop.wt)
  for (a in 1:n.areas) {
    ABC.EM[a] <- ABC.total * fish.prop.sum[a]  
  }
  return(ABC.EM)
}

expfishwt_apportionment(ABC.total.test,n.areas.test,fish.test.data)



#4 - non-exponentially weighted 5 year moving average of survey and fishery, survey weight is 2x fishery still
#(so, each year of the past has equal weight instead of exponential)
#survey data is given 2x the weight of fishery data
#generate some fake CPUE index data for a fishery and for a survey
#area order should be BS - AI - WG - CG - EG (or if 6 areas, WY - EYSEO instead of EG)
fishery.n.years <- 10
fish.test.data <- matrix(data=NA,nrow=fishery.n.years,ncol=n.areas.test) #fake fishery RPW data
fish.test.data[,1] <- c(1000*abs(rnorm(fishery.n.years,0.06,2.0)))  
fish.test.data[,2] <- c(1000*abs(rnorm(fishery.n.years,1.0,2.0))) 
fish.test.data[,3] <- c(1000*abs(rnorm(fishery.n.years,1.4,1.5))) 
fish.test.data[,4] <- c(1000*abs(rnorm(fishery.n.years,2.5,0.2))) 
fish.test.data[,5] <- c(1000*abs(rnorm(fishery.n.years,1.1,0.8))) 
fish.test.data[,6] <- c(1000*abs(rnorm(fishery.n.years,2.0,0.4))) 

biomass.n.years <- 10
biomass.test.data <- matrix(data=NA,nrow=biomass.n.years,ncol=n.areas.test)
biomass.test.data[,1] <- c(1000*abs(rnorm(biomass.n.years,0.06,2.0)))  #fake longline survey RPN data (how to reconcile between RPN and RPW? Do I need to?)
biomass.test.data[,2] <- c(1000*abs(rnorm(biomass.n.years,1.0,2.0))) 
biomass.test.data[,3] <- c(1000*abs(rnorm(biomass.n.years,1.4,1.5))) 
biomass.test.data[,4] <- c(1000*abs(rnorm(biomass.n.years,2.5,0.2))) 
biomass.test.data[,5] <- c(1000*abs(rnorm(biomass.n.years,1.1,0.8))) 
biomass.test.data[,6] <- c(1000*abs(rnorm(biomass.n.years,2.0,0.4))) 


nonexp_apportionment <- function(ABC.total,n.areas,fish.data,biom.data) { 
  ABC.EM <- vector(length=n.areas) #creating the output vector to hold apportioned ABCs
  fish.data.prop <- matrix(data=NA, ncol=n.areas,nrow=5)
  fish.data.prop.wt <- matrix(data=NA, ncol=n.areas,nrow=5)
  fish.prop.sum <- vector(length=n.areas)
  biom.data.prop <- matrix(data=NA, ncol=n.areas,nrow=5)
  biom.data.prop.wt <- matrix(data=NA, ncol=n.areas,nrow=5)
  biom.prop.sum <- vector(length=n.areas)
  wts <- c(0.2, 0.2, 0.2, 0.2, 0.2) #the weighting values
  EAprop<-24632/(21820+24632) #proportion of western aleutians to easter aleutians, applied to fishery RPW because
  #the fishery fishes farther along the aleutians than the survey or than the AI management area??
  fish.data[,2] <- fish.data[,2]*(c(rep(EAprop,times=length(fish.data[,2])))) # multiply the fishery data by the E-W aleutians proportion
  
  for (i in (length(fish.data[,1])-4):length(fish.data[,1])) {
    for (a in 1:n.areas) {
      m <- i-(length(fish.data[,1])-5)
      fish.data.prop[m,a] <- fish.data[i,a]/sum(fish.data[i,]) #calc proportion by year across areas for fishery data
      biom.data.prop[m,a] <- biom.data[i,a]/sum(biom.data[i,]) #calc proportion by year across areas for survey data
    }
  }
  for (i in 1:length(fish.data.prop[,1])) {
    for (a in 1:n.areas) {    
      fish.data.prop.wt[i,a] <- fish.data.prop[i,a]*wts[i]
      biom.data.prop.wt[i,a] <- biom.data.prop[i,a]*wts[i]    
    }
  }  
  fish.prop.sum <- colSums(fish.data.prop.wt)
  biom.prop.sum <- colSums(biom.data.prop.wt)  
  for (a in 1:n.areas) {
    ABC.EM[a] <- ABC.total * ((fish.prop.sum[a]+(2*biom.prop.sum[a]))/3)  #weighting 2x suvery:1x fishery happens here
  }
  return(ABC.EM)
}

nonexp_apportionment(ABC.total.test,n.areas.test,fish.test.data, biomass.test.data)


#9 - partially fixed apportionment (BS and AI fixed at some value, rest use status quo; proportions 
#for WG through EY/SEO are based on those areas only, excluding BS and AI)
#survey data is given 2x the weight of fishery data
#generate some fake CPUE index data for a fishery and for a survey
#area order should be BS - AI - WG - CG - EG (or if 6 areas, WY - EYSEO instead of EG)
fishery.n.years <- 10
fish.test.data <- matrix(data=NA,nrow=fishery.n.years,ncol=n.areas.test) #fake fishery RPW data
fish.test.data[,1] <- c(1000*abs(rnorm(fishery.n.years,0.06,2.0)))  
fish.test.data[,2] <- c(1000*abs(rnorm(fishery.n.years,1.0,2.0))) 
fish.test.data[,3] <- c(1000*abs(rnorm(fishery.n.years,1.4,1.5))) 
fish.test.data[,4] <- c(1000*abs(rnorm(fishery.n.years,2.5,0.2))) 
fish.test.data[,5] <- c(1000*abs(rnorm(fishery.n.years,1.1,0.8))) 
fish.test.data[,6] <- c(1000*abs(rnorm(fishery.n.years,2.0,0.4))) 

biomass.n.years <- 10
biomass.test.data <- matrix(data=NA,nrow=biomass.n.years,ncol=n.areas.test)
biomass.test.data[,1] <- c(1000*abs(rnorm(biomass.n.years,0.06,2.0)))  #fake longline survey RPN data (how to reconcile between RPN and RPW? Do I need to?)
biomass.test.data[,2] <- c(1000*abs(rnorm(biomass.n.years,1.0,2.0))) 
biomass.test.data[,3] <- c(1000*abs(rnorm(biomass.n.years,1.4,1.5))) 
biomass.test.data[,4] <- c(1000*abs(rnorm(biomass.n.years,2.5,0.2))) 
biomass.test.data[,5] <- c(1000*abs(rnorm(biomass.n.years,1.1,0.8))) 
biomass.test.data[,6] <- c(1000*abs(rnorm(biomass.n.years,2.0,0.4))) 

fixed_values <- c(0.1, 0.1) #input two values for apportionment proportion for Bering Sea (value 1) and Aleutian Islands (value 2)

partfixed_apportionment <- function(ABC.total,n.areas,fish.data,biom.data, fixed_values) { 
  ABC.EM <- vector(length=n.areas) #creating the output vector to hold apportioned ABCs
  fish.data.prop <- matrix(data=NA, ncol=n.areas,nrow=5)
  fish.data.prop.wt <- matrix(data=NA, ncol=n.areas,nrow=5)
  fish.prop.sum <- vector(length=n.areas)
  biom.data.prop <- matrix(data=NA, ncol=n.areas,nrow=5)
  biom.data.prop.wt <- matrix(data=NA, ncol=n.areas,nrow=5)
  biom.prop.sum <- vector(length=n.areas)
  wts <- c(0.0625, 0.0625, 0.125, 0.25, 0.5) #the exponential weighting values to weight over past five years
  fixedmult <- 1-(sum(fixed_values))
  #EAprop<-45632/(21820+24632) #proportion of western aleutians to easter aleutians, applied to fishery RPW because
  #the fishery fishes farther along the aleutians than the survey or than the AI management area??
  #fish.data[,2] <- fish.data[,2]*(c(rep(EAprop,times=length(fish.data[,2])))) # multiply the fishery data by the E-W aleutians proportion
  
  for (i in (length(fish.data[,1])-4):length(fish.data[,1])) {
    for (a in 3:n.areas) {
      m <- i-(length(fish.data[,1])-5)
      fish.data.prop[m,a] <- fixedmult*fish.data[i,a]/sum(fish.data[i,3:n.areas]) #calc proportion by year across areas for fishery data for areas 3:n.areas
      biom.data.prop[m,a] <- fixedmult*biom.data[i,a]/sum(biom.data[i,3:n.areas]) #calc proportion by year across areas for survey data for areas 3:n.areas
    }  }
  for (i in (length(fish.data[,1])-4):length(fish.data[,1])) {  #fill in the fixed values for BS and AI
    for (a in 1:2) {
      m <- i-(length(fish.data[,1])-5)
      fish.data.prop[m,a] <- fixed_values[a]
      biom.data.prop[m,a] <- fixed_values[a]
    }  }
  
  for (i in 1:length(fish.data.prop[,1])) {
    for (a in 1:n.areas) {    
      fish.data.prop.wt[i,a] <- fish.data.prop[i,a]*wts[i]
      biom.data.prop.wt[i,a] <- biom.data.prop[i,a]*wts[i]    
    }  }  
  fish.prop.sum <- colSums(fish.data.prop.wt)
  biom.prop.sum <- colSums(biom.data.prop.wt)  
  for (a in 1:n.areas) {
    ABC.EM[a] <- ABC.total * ((fish.prop.sum[a]+(2*biom.prop.sum[a]))/3)  #weighting 2x suvery:1x fishery happens here
  }
  return(ABC.EM)
}

partfixed_apportionment(ABC.total.test,n.areas.test,fish.test.data, biomass.test.data, fixed_values)


#10 - penalized apportionment (individual area can't change by more than x% per year)
#survey data is given 2x the weight of fishery data
#generate some fake CPUE index data for a fishery and for a survey
#area order should be BS - AI - WG - CG - EG (or if 6 areas, WY - EYSEO instead of EG)
fishery.n.years <- 10
fish.test.data <- matrix(data=NA,nrow=fishery.n.years,ncol=n.areas.test) #fake fishery RPW data
fish.test.data[,1] <- c(1000*abs(rnorm(fishery.n.years,0.06,2.0)))  
fish.test.data[,2] <- c(1000*abs(rnorm(fishery.n.years,1.0,2.0))) 
fish.test.data[,3] <- c(1000*abs(rnorm(fishery.n.years,1.4,1.5))) 
fish.test.data[,4] <- c(1000*abs(rnorm(fishery.n.years,2.5,0.2))) 
fish.test.data[,5] <- c(1000*abs(rnorm(fishery.n.years,1.1,0.8))) 
fish.test.data[,6] <- c(1000*abs(rnorm(fishery.n.years,2.0,0.4))) 

biomass.n.years <- 10
biomass.test.data <- matrix(data=NA,nrow=biomass.n.years,ncol=n.areas.test)
biomass.test.data[,1] <- c(1000*abs(rnorm(biomass.n.years,0.06,2.0)))  #fake longline survey RPN data (how to reconcile between RPN and RPW? Do I need to?)
biomass.test.data[,2] <- c(1000*abs(rnorm(biomass.n.years,1.0,2.0))) 
biomass.test.data[,3] <- c(1000*abs(rnorm(biomass.n.years,1.4,1.5))) 
biomass.test.data[,4] <- c(1000*abs(rnorm(biomass.n.years,2.5,0.2))) 
biomass.test.data[,5] <- c(1000*abs(rnorm(biomass.n.years,1.1,0.8))) 
biomass.test.data[,6] <- c(1000*abs(rnorm(biomass.n.years,2.0,0.4))) 

test.penalty <- 0.2
lastYr_ABCs.test <- c(20,12,16,30,20,15)
#ABC.EM<- c(16.6,14.2,12.4,25.4,7.6,23.8) #remove later!
#penalty <- 0.2 #remove later
#n.areas <- 6 #remove later

penalized_apportionment <- function(ABC.total,n.areas,fish.data,biom.data,penalty,lastYr_ABCs) { 
  ABC.EM <- vector(length=n.areas) #creating the output vector to hold apportioned ABCs
  fish.data.prop <- matrix(data=NA, ncol=n.areas,nrow=5)
  fish.data.prop.wt <- matrix(data=NA, ncol=n.areas,nrow=5)
  fish.prop.sum <- vector(length=n.areas)
  biom.data.prop <- matrix(data=NA, ncol=n.areas,nrow=5)
  biom.data.prop.wt <- matrix(data=NA, ncol=n.areas,nrow=5)
  biom.prop.sum <- vector(length=n.areas)
  pctdiff <- vector(length=n.areas)
  pen_ABC <- vector(length=n.areas) #holds the penalized ABC values
  wts <- c(0.0625, 0.0625, 0.125, 0.25, 0.5) #the weighting values
  EAprop<-24632/(21820+24632) #proportion of western aleutians to easter aleutians, applied to fishery RPW because
  #the fishery fishes farther along the aleutians than the survey or than the AI management area??
  fish.data[,2] <- fish.data[,2]*(c(rep(EAprop,times=length(fish.data[,2])))) # multiply the fishery data by the E-W aleutians proportion
  
  for (i in (length(fish.data[,1])-4):length(fish.data[,1])) {
    for (a in 1:n.areas) {
      m <- i-(length(fish.data[,1])-5)
      fish.data.prop[m,a] <- fish.data[i,a]/sum(fish.data[i,]) #calc proportion by year across areas for fishery data
      biom.data.prop[m,a] <- biom.data[i,a]/sum(biom.data[i,]) #calc proportion by year across areas for survey data
    }
  }
  for (i in 1:length(fish.data.prop[,1])) {
    for (a in 1:n.areas) {    
      fish.data.prop.wt[i,a] <- fish.data.prop[i,a]*wts[i] #add the exponential weighting
      biom.data.prop.wt[i,a] <- biom.data.prop[i,a]*wts[i]    
    }
  }  
  fish.prop.sum <- colSums(fish.data.prop.wt)
  biom.prop.sum <- colSums(biom.data.prop.wt)  
  for (a in 1:n.areas) {
    ABC.EM[a] <- ABC.total * ((fish.prop.sum[a]+(2*biom.prop.sum[a]))/3)  #weighting 2x suvery:1x fishery happens here
  }
  #figure out change in ABC since previous year
  pctdiff<- (ABC.EM - lastYr_ABCs)/ABC.EM
  #then apply penalty, if needed  
  for (a in 1:n.areas){
    ifelse(abs(pctdiff[a])>penalty,ifelse(pctdiff[a]<=0,pen_ABC[a] <- (1-penalty)*lastYr_ABCs[a],pen_ABC[a] <- (1+penalty)*lastYr_ABCs[a]), pen_ABC[a] <- ABC.EM[a])  
    #pen_ABC[a] <- (1-penalty)*lastYr_ABCs[a]
    #pen_ABC[a] <- (1+penalty)*lastYr_ABCs[a]
  }
  return(pen_ABC)
  return(pctdiff) #will R only 'return' one set of things?
  return(ABC.EM)
  return(lastYr_ABCs)
  
}

penalized_apportionment(ABC.total.test,n.areas.test,fish.test.data, biomass.test.data,test.penalty,lastYr_ABCs.test)



#8 - length based apportionment
#calculate the mean (or median?) length at 50% female maturity - then calculate the proportion of females of that 
#length in each of the n.areas based on the LL survey data most recent year data
L50_mat <- 13 #input row number for length at 50% maturation or greater (data as aggregate across all Alaska areas; 
#made up value for now 65 and over)

n.Lbins <- 30
test.data <- matrix(data=NA,nrow=n.Lbins,ncol=6) #fake n at age data
test.data[,1] <- c(1000*abs(rnorm(n.Lbins,0.5,0.5)))
test.data[,2] <- c(1000*abs(rnorm(n.Lbins,1.0,1.0))) 
test.data[,3] <- c(1000*abs(rnorm(n.Lbins,1.0,2.0))) 
test.data[,4] <- c(1000*abs(rnorm(n.Lbins,1.0,2.0))) 
test.data[,5] <- c(1000*abs(rnorm(n.Lbins,2.0,4.0))) 
test.data[,6] <- c(1000*abs(rnorm(n.Lbins,3.0,6.0))) 

test.data[1:10,1] <- sort(test.data[1:10,1],decreasing=FALSE)
test.data[11:30,1] <- sort(test.data[11:30,1],decreasing=TRUE)
test.data[1:13,2] <- sort(test.data[1:13,2],decreasing=FALSE)
test.data[14:30,2] <- sort(test.data[14:30,2],decreasing=TRUE)
test.data[1:15,3] <- sort(test.data[1:15,3],decreasing=FALSE)
test.data[16:30,3] <- sort(test.data[16:30,3],decreasing=TRUE)
test.data[1:15,4] <- sort(test.data[1:15,4],decreasing=FALSE)
test.data[16:30,4] <- sort(test.data[16:30,4],decreasing=TRUE)
test.data[1:15,5] <- sort(test.data[1:15,5],decreasing=FALSE)
test.data[16:30,5] <- sort(test.data[16:30,5],decreasing=TRUE)
test.data[1:15,6] <- sort(test.data[1:15,6],decreasing=FALSE)
test.data[16:30,6] <- sort(test.data[16:30,6],decreasing=TRUE)

agebased_apportionment <- function(ABC.total,n.areas,LLlencomp,L50_mat) { 
  ABC.EM <- vector(length=n.areas) #creating the output vector to hold apportioned ABCs
  natage.prop1 <- vector(length=n.areas) #creating output vector for prop at L50 (or larger)
  natage.prop2 <- vector(length=n.areas) #creating output vector apportionment based on natage.prop1
  for (a in 1:n.areas){
    natage.prop1[a] <- sum(LLlencomp[L50_mat:length(LLlencomp[,a]),a])/sum(LLlencomp[,a])
  }
  for (a in 1:n.areas) {
    natage.prop2[a] <- natage.prop1[a]/sum(natage.prop1)
  }
  return(natage.prop2)
}

agebased_apportionment(ABC.total,n.areas,test.data,L50_mat)



#6 - Random effects model apportionment
## move into RE directory
path<-getwd()
setwd(paste0(path,"/r/re/"))
####
#make up survey biomass data for each of n areas for now
biomass.n.years <- 10
biomass.years<-seq(1,11) # so you can have prediction with error for next year
biomass.test.data <- matrix(data=NA,nrow=biomass.n.years,ncol=n.areas.test)
biomass.test.data[,1] <- c(1000*abs(rnorm(biomass.n.years,0.06,2.0)))  
biomass.test.data[,2] <- c(1000*abs(rnorm(biomass.n.years,1.0,2.0))) 
biomass.test.data[,3] <- c(1000*abs(rnorm(biomass.n.years,1.4,1.5))) 
biomass.test.data[,4] <- c(1000*abs(rnorm(biomass.n.years,2.5,0.2))) 
biomass.test.data[,5] <- c(1000*abs(rnorm(biomass.n.years,1.1,0.8))) 
biomass.test.data[,6] <- c(1000*abs(rnorm(biomass.n.years,2.0,0.4))) 
biomass.cv.test.data <- matrix(data=NA,nrow=biomass.n.years,ncol=n.areas.test)
biomass.cv.test.data[,1] <- rep(0.05,biomass.n.years)
biomass.cv.test.data[,2] <- rep(0.05,biomass.n.years)
biomass.cv.test.data[,3] <- rep(0.05,biomass.n.years)
biomass.cv.test.data[,4] <- rep(0.05,biomass.n.years)
biomass.cv.test.data[,5] <- rep(0.05,biomass.n.years)
biomass.cv.test.data[,6] <- rep(0.05,biomass.n.years)


srv_re<-data.frame(matrix(nrow=n.areas,ncol=6))
srv_recv<-data.frame(matrix(nrow=n.areas,ncol=6))
biomassbased_apportionment <- function(ABC.total,n.areas,biom.data) { 
  srv_re<-data.frame(matrix(nrow=n.areas,ncol=max(biomass.years)-min(biomass.years)+1))
  srv_recv<-data.frame(matrix(nrow=na.areas,ncol=max(biomass.years)-min(biomass.years)+1))
  re_apportionment <- function(ABC.total,n.areas,biom.data,biom.cv.data) { 
    
    for(i in 1:6) {
      try(
        {styr <-min(biomass.years)
        endyr <-max(biomass.years)
        nobs<-biomass.n.years
        yrs_srv<-seq(1,biomass.n.years)
        srv_est<-biomass.test.data[,i]
        srv_cv<-biomass.cv.test.data[,i]
        srv_est<-biom.data[,i]
        srv_cv<-biom.cv.data[,i]
        cat(styr,"\n",endyr,"\n",nobs,"\n",yrs_srv,"\n",as.numeric(srv_est),"\n",as.numeric(srv_cv),"\n",sep=" ",file="re.dat")
        system("re.exe")
        all_yrs<-seq(styr,endyr)
        srv_re[i,]<-scan(file="rwout.rep",nlines=1,skip=11)
        srv_recv[i,]<-scan(file="rwout.rep",nlines=1,skip=21)
        }
        ,TRUE) }
    
    ABC.EM <- vector(length=n.areas)
    biom <- vector(length=n.areas)
    biom.prop <- vector(length=n.areas)
    biom.yrs <- length(biom.data[,1])
    for (a in 1:n.areas) {
      biom[a] <- srv_re[a,biomass.n.years] 
    }
    for (a in 1:n.areas) {
      biom.prop[a] <- biom[a]/sum(biom)
      ABC.EM[a]<-ABC.total*biom.prop[a]
    }
    return(ABC.EM)
  }
}
  

  ### clean up ADMB mess
  flist=list.files(pattern="*.*")
  flist<-flist[-grep(".exe",flist)]
  file.remove(flist)
  ### go back to project path
  setwd(path)
  
  ## Lentgh-based apportionment options
  
  expmaturerpn_apportionment <- function(ABC.total,n.areas,biom.data) { 
    ABC.EM <- vector(length=n.areas) #creating the output vector to hold apportioned ABCs
    biom.data.prop <- matrix(data=NA, ncol=n.areas,nrow=5)
    biom.data.prop.wt <- matrix(data=NA, ncol=n.areas,nrow=5)
    biom.prop.sum <- vector(length=n.areas)
    wts <- c(0.0625, 0.0625, 0.125, 0.25, 0.5) #the weighting values
    
    for (i in (length(biom.data[,1])-4):length(biom.data[,1])) {
      for (a in 1:n.areas) {
        m <- i-(length(biom.data[,1])-5)
        biom.data.prop[m,a] <- biom.data[i,a]/sum(biom.data[i,]) #calc proportion by year across areas for survey data
      }
    }
    for (i in 1:length(biom.data.prop[,1])) {
      for (a in 1:n.areas) {    
        biom.data.prop.wt[i,a] <- biom.data.prop[i,a]*wts[i]    
      }
    }  
    biom.prop.sum <- colSums(biom.data.prop.wt)  
    for (a in 1:n.areas) {
      ABC.EM[a] <- ABC.total * biom.prop.sum[a]  
    }
    return(ABC.EM)
  }
  
  expmaturerpn_apportionment(ABC.total.test,n.areas.test,biomass.test.data)
  