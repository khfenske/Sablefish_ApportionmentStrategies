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
  age.rec <<- as.numeric(in.general$Value[in.general$Par=='age.rec'])
  n.sex <<- as.numeric(in.general$Value[in.general$Par=='n.sex']) 
  A <<- as.numeric(in.general$Value[in.general$Par=='A']) #Plus Age
  n.area <<- as.numeric(in.general$Value[in.general$Par=='n.area']) #Number of Areas
  areas <<- 1:n.area #areas
  n.length <<- as.numeric(in.general$Value[in.general$Par=="n.length"]) #number of length bins
  first.len <<- as.numeric(in.general$Value[in.general$Par=="first.len"]) #smalled length bin size
  len.incr <<- as.numeric(in.general$Value[in.general$Par=="len.incr"]) #increment for length bins (e.g. '2' for odd or even size bins)
  len <<- as.numeric(c(seq(from=first.len,length.out=n.length,by=len.incr)))
  move.type <<-as.numeric(in.general$Value[in.general$Par=="move.type"]) # indicator for dimensions of movement data (currently only 1 is operational or defined)
   
  # Extract: Simulation Parameters ==============================================================
  in.sim <- read.xlsx(file=file.path(dir.data, input.file), sheetName='Sim')
  n.year <<- as.numeric(in.sim$Value[in.sim$Par=='n.year'])
  years <<- 1:n.year
  Bstart <<- as.numeric(in.sim$Value[in.sim$Par=='Bstart'])
  n.sims <<- as.numeric(in.sim$Value[in.sim$Par=='n.sims'])
  
  # Extract: Init_pop values
  #these are the 1979 numbers at age for each area, sex
  #for sablefish, ages 2-31 are modeled (and entered), but the model indexes these as ages 1-30
  in.init_pop <- read.xlsx(file=file.path(dir.data, input.file), sheetName='Init_pop')
  init_year_N <<- as.data.frame(in.init_pop)
  #this reads in the recruitment values to seed our initial population (1979-2018), values from 2018 SAFE
  in.init_pop_Rec <- read.xlsx(file=file.path(dir.data, input.file), sheetName='Init_pop_Rec', rowIndex=c(1:41),colIndex=c(1:2))
  init_pop_rec <<- as.data.frame(in.init_pop_Rec)
  #this reads in the proportion of recruits by area, using the proportion of 2 years olds by area from the survey
  in.init_rec_prop <- read.xlsx(file=file.path(dir.data, input.file), sheetName='Init_rec_prop',rowIndex=c(1:2),colIndex = c(1:6))
  init_rec_proportion <<- as.data.frame(in.init_rec_prop)
  #this reads in the catch history for 1979-2018, will be used to estimate F rates to set up initial pop
  #note that currently the 2018 catch is probably estimated and not the complete catch total
  #catch by area for 1991 onward is from AKFIN and MAY BE CONFIDENTIAL!!!!*********
  #catch by area for 1979-1990 is based on 1991 onward catch by area proportions, applied to total catch for those early years
  in.init_pop_catch <<- read.xlsx(file=file.path(dir.data, input.file), sheetName='Init_pop_Catch')
  init_pop_catch <<- as.data.frame(in.init_pop_catch)
  #mean age comps from fishery over years 1991-2014 for 3 areas BSAIWG, CG, EG. 
  #in.init_fishAC <<- read.xlsx(file=file.path(dir.data, input.file), sheetName='Init_fishAC')
  #init_fishAC <<- as.data.frame(in.init_fishAC)
  
  
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
  
  ml_par1 <- as.numeric(in.mature[in.mature$Par=='ml_par1',(2:3)]) #Location parameter
  ml_par2 <- as.numeric(in.mature[in.mature$Par=='ml_par2',(2:3)]) #Scale parameter
  
  #Calculate Maturity @ Length  THIS SHOULD BE AGE NOT LENGTH??
  ml <<- array(dim=c(n.sex, n.age), dimnames=list(sexes, c(1:n.age)))  
  
  s <- 1
  for(s in 1:n.sex) {
    # Ma[,s] <- 1/(1+exp(-(ages - ahat[s])/ghat[s]) #Halibut Model
    ma[s,] <<- 1/(1+exp(-ghat[s]*(ages - ahat[s]))) #Sablefish 2017 Assessment
  }#next s
  
  a <- 1  #is this converting from age to length?
  for(a in 1:n.age) {
    ml[,a] <<- 1/(1+exp(-ml_par1*(la[,a]-ml_par2)))
  }
  
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
  
  #Movement
  in.movement <<- read.xlsx(file=file.path(dir.data, input.file), sheetName='Movement')
    phi <<- as.matrix(in.movement) #should I have better labels on these columns and rows?
} #closes function


# extract_pars(input.file="Sablefish_Input.xlsx")
