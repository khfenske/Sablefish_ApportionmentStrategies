#==================================================================================================
#Project Name: SABLEFISH APPORTIONMENT - Data Reading Function
#Creator: Curry James Cunningham, NOAA/NMFS, ABL
#Date: 2.21.18
#
#Purpose: Function to read demographic parameters for operating model
#
#==================================================================================================
#NOTES:
#  a) 
#
#==================================================================================================



extract_pars <- function(input.file="Sablefish_Input.xlsx") {
  require(tidyverse)
  require(readxl)
  require(xlsx)
  
  #### TESTING
  # input.file <- "Sablefish_Input.xlsx"
  ####
  
  
  # Define Workflow Paths ==============================================================
  # *Assumes you are working from the Sablefish_ApportionmentStrategies R project
  wd <- getwd()
  dir.output <- file.path(wd,"output")
  dir.data <- file.path(wd,"data")
  
  
  
  # Extract: General Demographic Parameters ==============================================================
  in.general <- read.xlsx(file=file.path(dir.data, input.file), sheetName='General')
  n.age <<- as.numeric(in.general$Value[in.general$Par=='n.age'])
  ages <<- 1:n.age #ages
  n.sex <<- as.numeric(in.general$Value[in.general$Par=='n.sex']) 
  age.rec <<- as.numeric(in.general$Value[in.general$Par=='age.rec'])
  A <<- as.numeric(in.general$Value[in.general$Par=='A']) #Plus Age
  n.area <<- as.numeric(in.general$Value[in.general$Par=='n.area']) #Number of Areas
  areas <<- 1:n.area #areas
  n.length <<- as.numeric(in.general$Value[in.general$Par=='n.length'])
  
  # Extract: Simulation Parameters ==============================================================
  in.sim <- read.xlsx(file=file.path(dir.data, input.file), sheetName='Sim')
  n.year <<- as.numeric(in.sim$Value[in.sim$Par=='n.year'])
  years <<- 1:n.year
  Bstart <<- as.numeric(in.sim$Value[in.sim$Par=='Bstart'])
  n.sims <<- as.numeric(in.sim$Value[in.sim$Par=='n.sims'])
  
  # Extract: Growth Parameters ==============================================================
  in.growth <- read.xlsx(file=file.path(dir.data, input.file), sheetName='Growth')
  linf <- as.numeric(in.growth[in.growth$Par=='linf',(2:3)])
  vbk <- as.numeric(in.growth[in.growth$Par=='vbk',(2:3)])
  to <- as.numeric(in.growth[in.growth$Par=='to',(2:3)])
  
  #Determine names for sexes
  sexes <<- names(in.growth)[2:3] # Error here
  
  la <<- array(dim=c(n.sex, n.age), dimnames=list(sexes,c(1:n.age))) #Length @ Age
  
  # alpha <- as.numeric(in.growth[in.growth$Par=='a',(2:3)])
  # beta <- as.numeric(in.growth[in.growth$Par=='b',(2:3)])
  ln_wa_par1 <- as.numeric(in.growth[in.growth$Par=='ln_wa_par1',(2:3)])
  ln_wa_par2 <- as.numeric(in.growth[in.growth$Par=='ln_wa_par2',(2:3)])
  ln_wa_par3 <- as.numeric(in.growth[in.growth$Par=='ln_wa_par3',(2:3)])
  ln_wa_par4 <- as.numeric(in.growth[in.growth$Par=='ln_wa_par4',(2:3)])
  
  wa <<- array(dim=c(n.sex, n.age), dimnames=list(sexes, c(1:n.age))) #Weight @ Age
  ln.wa <<- array(dim=c(n.sex, n.age), dimnames=list(sexes, c(1:n.age))) #Log Weight @ Age
  
  a <- 1
  for(a in 1:n.age) {
    la[,a] <<- linf*(1-exp(-1*vbk*(ages[a]+to)))
    # wa[a,] <<- alpha[1]*(la[a,1])^beta[1]
    ln.wa[,a] <<- log(ln_wa_par1) + ln_wa_par2*log(1-exp(-1*ln_wa_par3*(a+ln_wa_par4)))
    wa[,a] <<- exp(ln.wa[,a])
  }
  

  
  # Extract: Maturity Parameters ==============================================================
  #Note: This should be updated to take maturity parameters
  in.mature <- read.xlsx(file=file.path(dir.data, input.file), sheetName='Maturity')
  ahat <- as.numeric(in.mature[in.mature$Par=='ahat',(2:3)]) #Location parameter
  ghat <- as.numeric(in.mature[in.mature$Par=='ghat',(2:3)]) #Scale parameter
  
  #Calculate Maturity @ Age
  ma <<- array(dim=c(n.sex, n.age), dimnames=list(sexes, c(1:n.age)))  
  s <- 1
  for(s in 1:n.sex) {
    # Ma[,s] <- 1/(1+exp(-(ages - ahat[s])/ghat[s]) #Halibut Model
    ma[s,] <<- 1/(1+exp(-ghat[s]*(ages - ahat[s]))) #Sablefish 2017 Assessment
  }#next s
  
  # Extract: Length Bins and Maturity @ Length ================================================
  #Length bins
  lengths <<- as.vector(read.xlsx(file=file.path(dir.data, input.file), sheetName='Length_Bins'))
  if(length(lengths)!=n.length) { stop("Number of length bins in consistent with input bins")}
  
  # ml_par1 <- as.numeric(in.mature[in.mature$Par=='ml_par1',(2:3)]) #Location parameter
  # ml_par2 <- as.numeric(in.mature[in.mature$Par=='ml_par2',(2:3)]) #Scale parameter
  
  #Calculate Maturity @ Length
  # ml <<- array(dim=c(n.sex, n.length), dimnames=list(sexes, lengths))  
  
  # l <- 1
  # for(l in 1:n.length) {
  #   ml[,l] <<- 1/(1+exp(-ml_par1*(lengths[,l]-ml_par2)))
  # }
  
  #Use Length bins
  
  # Calculate Fecundity @ Age
  
  # Extract: Natural Mortality Parameters ==============================================================
  in.mort <- read.xlsx(file=file.path(dir.data, input.file), sheetName='Mortality')
  m <- as.numeric(in.mort[in.mort$Par=='m',(2:3)])
  k <- as.numeric(in.mort[in.mort$Par=='k',(2:3)])
  c <- as.numeric(in.mort[in.mort$Par=='c',(2:3)])
  
  #Calculate Natural Mortality at Age
  t1 <- array(dim=c(n.sex, n.age), dimnames=list(sexes, c(1:n.age)))
  t2 <- array(dim=c(n.sex, n.age), dimnames=list(sexes, c(1:n.age)))
  mx <<- array(dim=c(n.sex, n.age), dimnames=list(sexes, c(1:n.age))) #Mortality @ Age
  # lx <<- array(dim=c(n.age, n.sex), dimnames=list(c(1:n.age), sexes)) #Survivorship to Age
  
  s <- 1
  for(s in 1:n.sex) {
    a <- 1
    for(a in 1:n.age) {
      t1[s,a] <- exp(k[s]*(a+1))-1
      t2[s,a] <- exp(k[s]*(a+2))-1
    }#next a
    mx[s,] <<- m[s] * ((log(t2[s,]) - log(t1[s,]))/k[s] )^c[s]
  }#next s
  
  #Note: Might consider adding survivorship to age (lx)
  
  
  # Extract: Recruitment Parameters ==============================================================
  in.rec <- read.xlsx(file=file.path(dir.data, input.file), sheetName='Recruitment')
  mu_rec <<- in.rec$Value[in.rec$Par=='mu_rec']
  sigma_rec <<- in.rec$Value[in.rec$Par=='sigma_rec']

  # Selectivity: Survey ==========================================================================
  in.selex.surv <<- read.xlsx(file=file.path(dir.data, input.file), sheetName='SurveySelectivity')
  
  #Record Type and Number
  surv <<- unique(in.selex.surv$Fleet)
  n.surv <<- length(surv)
  
  selex.surv <<- list()
  #US Longline
  selex.surv$USLongline$a50[1:2] <<- as.numeric(in.selex.surv[in.selex.surv$Fleet=='USLongline' & in.selex.surv$Par=='a50',(3:4)])
  selex.surv$USLongline$delta[1:2] <<- as.numeric(in.selex.surv[in.selex.surv$Fleet=='USLongline' & in.selex.surv$Par=='delta',(3:4)])
  selex.surv$USLongline$amax[1:2] <<- as.numeric(in.selex.surv[in.selex.surv$Fleet=='USLongline' & in.selex.surv$Par=='amax',(3:4)])
  selex.surv$USLongline$plusAge[1:2] <<- as.numeric(in.selex.surv[in.selex.surv$Fleet=='USLongline' & in.selex.surv$Par=='plusAge',(3:4)])
  
  #Joint US-Japanese Longline
  selex.surv$USJPLL$a50[1:2] <<- as.numeric(in.selex.surv[in.selex.surv$Fleet=='USJPLL' & in.selex.surv$Par=='a50',(3:4)])
  selex.surv$USJPLL$delta[1:2] <<- as.numeric(in.selex.surv[in.selex.surv$Fleet=='USJPLL' & in.selex.surv$Par=='delta',(3:4)])
  selex.surv$USJPLL$amax[1:2] <<- as.numeric(in.selex.surv[in.selex.surv$Fleet=='USJPLL' & in.selex.surv$Par=='amax',(3:4)])
  selex.surv$USJPLL$plusAge[1:2] <<- as.numeric(in.selex.surv[in.selex.surv$Fleet=='USJPLL' & in.selex.surv$Par=='plusAge',(3:4)])
  
  #Survey Select Function Definition 
  selex.surv.type <<- read.xlsx(file=file.path(dir.data, input.file), sheetName='SurveySelexType')

  # Selectivity: Fishery ==========================================================================
  # NOTE: At present fisheries with the same selectivity across areas are specified by using repeated values in the input file.
  #By region
  
  in.selex.fish <<- read.xlsx(file=file.path(dir.data, input.file), sheetName='FisherySelectivity')
  selex.fish <<- list()
  
  #Record Type and Number
  fish <<- unique(in.selex.fish$Fleet)
  n.fish <<- length(fish)
  
  #Loop through areas
  a <- 1
  for(a in 1:n.area) {
    
    #US Fixed Gear - Pre IFQ
    selex.fish$USfixed_preIFQ$a50[[a]] <<- as.vector(in.selex.fish[in.selex.fish$Fleet=='USfixed_preIFQ' & 
                                                                 in.selex.fish$Par=='a50' & 
                                                                 in.selex.fish$Area==a, (4:5)])
    #US Fixed Gear - Post IFQ
    selex.fish$USfixed_postIFQ$a50[[a]] <<- as.vector(in.selex.fish[in.selex.fish$Fleet=='USfixed_postIFQ' & 
                                                                    in.selex.fish$Par=='a50' & 
                                                                    in.selex.fish$Area==a, (4:5)])
    #US Trawl
    selex.fish$USTrawl$a50[[a]] <<- as.vector(in.selex.fish[in.selex.fish$Fleet=='USTrawl' & 
                                                                     in.selex.fish$Par=='a50' & 
                                                                     in.selex.fish$Area==a, (4:5)])
    #Foreign
    selex.fish$Foreign$a50[[a]] <<- as.vector(in.selex.fish[in.selex.fish$Fleet=='Foreign' & 
                                                             in.selex.fish$Par=='a50' & 
                                                             in.selex.fish$Area==a, (4:5)])
  }#next a
  
  #Fishery Select Function Definition 
  selex.fish.type <<- read.xlsx(file=file.path(dir.data, input.file), sheetName='FisherySelexType')
  
  # Catchability =======================================================
  catchability <<- list() 
  catchability$survey <<- read.xlsx(file=file.path(dir.data, input.file), sheetName='qSurvey')
  catchability$fishery <<- read.xlsx(file=file.path(dir.data, input.file), sheetName='qFishery')
}

# extract_pars(input.file="Sablefish_Input.xlsx")
