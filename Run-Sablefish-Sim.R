#==================================================================================================
#Project Name: SABLEFISH APPORTIONMENT - Run Simple Simulation
#Creator: Curry James Cunningham, NOAA/NMFS, ABL
#Date: 3.10.18
#
#Purpose: Read in Parameters and Conduct Simple Simulation/
#
#==================================================================================================
#NOTES:
#  a) 
#
#==================================================================================================

# Define Workflow ================================================
wd <- getwd() #Project Directory

dir.data <- file.path(wd,"data")
dir.figs <- file.path(wd,"figs")
dir.output <- file.path(wd,"output")
dir.admb <- file.path(wd,"admb")
dir.R <- file.path(wd,"R")
dir.x <- file.path("C:/Repositories/hidden files with conf data") #change this path to whatever place you have the confidential catch data files stored. DO NOT LOAD TO GITHUB.

# Call all packages and libraries once, up front ===================
# for Run_Sim_data_plots.R
require(dplyr)
require(reshape2)
require(tidyverse)
require(ggplot2)
#for extract-pars.R
require(tidyverse)
require(readxl)
require(xlsx)
# for sample-age-comps.R
require(gtools) 
#for .dat file building
require(PBSmodelling)
        
# Source Necessary Files =========================================
source(file.path(dir.R,'extract-pars.R'))
source(file.path(dir.R,'extract-catch.R'))

source(file.path(dir.R,'calc-selectivity.R'))
source(file.path(dir.R,'calc-init-age-prop.R'))

source(file.path(dir.R,'create-sim-objects.R'))
source(file.path(dir.R,'create-sim-recruitments.R')) #Simulate Recruitment across years and sims
source(file.path(dir.R,'create-cond-catch.R')) #creates an array of catch by year,age,sex,area,fish 
#the catch values in the actual catch spreadsheet are confidential, so dummy values are in place now.

source(file.path(dir.R,'spatial-rec.R')) #Apportion Recruitment Among Regions

source(file.path(dir.R,'convert-Fmort2catch.R')) #Calculate total catch from F
source(file.path(dir.R,'estimate-Fmort4catch.R')) #Estimate F that provides for a given catch

source(file.path(dir.R,'sample-biom-abund.R')) #sample biomass/numbers of OM population
source(file.path(dir.R,'sample-age-comps.R')) #sample age comps of OM population
source(file.path(dir.R,'aggregate-agcomps.R')) #sample age comps of OM population

source(file.path(dir.R,'sablefish-conditioning-datfile-builder.R'))
source(file.path(dir.R,'sablefish-datfile-builder.R'))

source(file.path(dir.R,'read-movement-rates.R')) #Function to read in movement rates

        
# Extract Parameters =============================================
extract_pars(input.file="Sablefish_Input.xlsx")
extract_catch(input.file="catch_input_conditioning.xlsx") #using a separate function for this because catch by gear and area has
# confidential data, catch is in kt, change this function to read in the dummy spreadsheet (fake catch data) for running this for now
 
# Read in Movement Rates =========================================
prob.move <- read_movement_rates(input.file="Sablefish_Input.xlsx")

#prob.move2 is for testing to make sure I have movement code right.
prob.move2 <- array(data=NA, dim=c(n.area,n.area,n.age), dimnames=list(areas,areas,ages))
for (m in 1:n.age) {
  prob.move2[1,,m] <- c(1,0,0,0,0,0)
  prob.move2[2,,m] <- c(0,1,0,0,0,0)
  prob.move2[3,,m] <- c(0,0,1,0,0,0)
  prob.move2[4,,m] <- c(0,0,0,1,0,0)
  prob.move2[5,,m] <- c(0,0,0,0,1,0)
  prob.move2[6,,m] <- c(0,0,0,0,0,1)
}

# Calculate Selectivity ==========================================

selex <- list() #Selectivity List
#NOTE: You can also include as multidimensional arrays in place of list named, better for looping, harder to look up. Tradeoffs.
# selex$surv <- array(dim=c(n.surv,n.sex,n.age), dimnames=list(surv,sexes,ages))
# selex$fish <- array(dim=c(n.fish,n.area,n.sex,n.age), dimnames=list(fish,1:n.area,sexes,ages))

#Proceed with list format
#Surveys
selex$surv$USLongline <- calc_selectivity(type='surv', fleet='USLongline')
selex$surv$USJPLL <- calc_selectivity(type='surv', fleet='USJPLL')

#Fisheries
selex$fish$USfixed_preIFQ <- calc_selectivity(type='fish', fleet='USfixed_preIFQ')
selex$fish$USfixed_postIFQ <- calc_selectivity(type='fish', fleet='USfixed_postIFQ')
selex$fish$USTrawl <- calc_selectivity(type='fish', fleet='USTrawl')
selex$fish$Foreign <- calc_selectivity(type='fish', fleet='Foreign')

#Calculate Vulnerability @ Age
va <- array(dim=c(n.fish,n.area,n.sex,n.age), dimnames=list(fish,1:n.area,sexes,ages))
i <- 1
for(i in 1:n.fish) {
  va[i,,,] <- calc_selectivity(type='fish', fleet=fish[i])
}


# Create Simulation Objects =======================================
#NOTE: Currently calculates data for n areas, where n is defined in the input spreadsheet (n.areas)  
create_sim_objects() #sets up all the spatial arrays to hold simulated data
#set up an array to hold the N array during movement code
N_hold <- array(dim=c(n.sex, n.year, n.area, n.age, n.sims), dimnames=list(sexes, years, areas, ages, sims))

# Simulate Annual Recruitments ====================================
#setup_years <- 39 #number of years to run the loop setting up the initial population and building initial dat file
create_sim_recruitments(mu_rec=mu_rec, sigma_rec=sigma_rec, rho_rec=NULL, 
                        n.year=n.year, n.sims=n.sims, seed=101) #Creates rec object 

# divide annual recruitments into areas - the values for area.props are from the proportions of age-2 fish by area from the LL survey, average across all years.
# see the excel file in the repository (in data folder)
rec.by.area.props <- c(0.14,0.07,0.14,0.43,0.14,0.09)
for(i in 1:n.sims){
for(y in 1:n.year){
  recruits.area[y,,i] <- spatial_rec(rec[i,y],area.props=rec.by.area.props, ss=100, seed=1)      # sexes combined, recruitment in Numbers (I think)
}}

# ==================================================================================
# Initialize Population - Conditioning the simulations (year 1, or 1976) ===========
N.by.area.props <- as.vector(c(0.123525838,0.138168043,0.112731838,0.401168634,0.102462464,0.121943183)) #mean proportion by area for n.areas from survey RPN (1979-2018) (from SurveyRPN.xlsx)
#init.prop <- calc_init_age_prop(bo=mu_rec) #setting up proportions at age for 1976
# instead of above funtion, read in proportions at age for males and females from 1976 estimated N from the single are EM, which are the same for both sexes so read in once here 
Nprop.by.age <- as.vector(c(0.01499621,0.012601811,0.031930367,	0.030601254,	0.249981823,	0.051333394,	0.023219615,	0.009110259,	0.024604504,
                           0.025654192,	0.014809429,	0.013695526,	0.012304411,	0.010897499,	0.009556439,	0.008318243,	0.007200544,
                           0.006206813,	0.005333111,	0.004572241,	0.403269678,	0.003351983,	0.002870484,	0.002458885,	0.002107328,
                           0.001806924,	0.001549864,	0.001330032,	0.001142024,	0.013185113)) #for 1:n.ages
#get catch at age in numbers (millions) or biomass (kt) from single area EM
cond_catch_at_age <- array(data=NA, dim=c(43,n.fish,n.area,n.sex,n.age),dimnames=list(1:43,fish,1:n.area,sexes,ages))
cond_catch_at_age <- cond_catch_AA(cond.catch, va, Ctype=2) #Ctype 1= biomass (kt), 2=numbers (millions)
#sum across ages and sexes
temp.catchnumbiom <- array(data=NA, dim=c(43,n.fish,n.area),dimnames=list(1:43,fish,1:n.area))
temp.catchnumbiom <- apply(cond_catch_at_age,1:3,sum)

### set up N samples and sigmas for sampling
LLsurvRPNsigma <- 0.2
LLfishRPWsigma <- 0.4
LLsurvAC_sampsize <- 20
LLfishAC_sampsize <- 20

##### Condition year 1 (aka 1976):
# since we track numbers in this model, I will initialize things in numbers instead of biomass
i <- 1
m <- 1
a <- 1
for(i in 1:n.sims) {
  for(m in 1:n.area) {
    for(a in 1:n.age) {
    N[,1,a,m,] <- Nstart * 0.5 * (Nprop.by.age[a]) #Nstart is in millions (check units), 0.5 is to divide equally between sexes 
    #N (dim=c(n.sex, n.year, n.age, n.area, n.sims), dimnames=list(sexes, years, ages, areas, sims)) 
    } #close age
  } #close area
}#next i
#weight initial proportions at age by areas weights
for(m in 1:n.area) {N[,1,,m,] <- N[,1,,m,] * N.by.area.props[m]} #N by age and area, in millions of fish
#now calculate biomass
for(a in 1:n.age) {B[,1,a,,] <- N[,1,a,,] * wa[,a]}  #in kt


# ==============Condition years 2-43 (aka 1977-2018)
#set up initial population and dat file
i <- 1
y <- 2 
for(i in 1:n.sims) {
  print(paste('Sim:',i,'of',n.sims))
  for(y in 2:43) {
    m <- 1
    for(m in 1:n.area) {
      
      f <- 1
      for(f in 1:n.fish) {
      #catch <- temp.catchnumbiom[y-1] #y-1 gives us 1976 catch when y=2
      
      # Find Fishing Mortality Rate for Apportioned Catch Level ------------------------ 
      temp.Fmort <- estimate_Fmort4catch(catch=temp.catchnumbiom[y-1,f,m], 
      temp.selex=va[f,m,,],
      temp.N=N[,y-1,,m,i], 
      wa=wa, mx=mx, 
      bisection=TRUE)$Fmort
      F.mort[f,y-1,m,i] <- temp.Fmort  #0.1
      } #next g 
      
    a <- 1
    for(a in 1:n.age) {
      #Update Numbers and Biomass Matrix
      if(a==1) { #Age 1
        #cond.rec are values read in from an excel file in the extract-pars.R function - they are estimated recruitments for 1977-2018 from the single area EM
        N[,y,a,m,i] <- 0.5*(cond.rec$Recruitment[y-1]) #multiplying by 0.5 to split evenly between sexes, y-1 calls the first year of cond.rec list 
        B[,y,a,m,i] <- N[,y,a,m,i] * wa[,a]
        # N[,y,a] <- rec[,y-1]/wa[,a]
        # B[,y,a] <- rec[,y-1]
        ssb[,a,y,m,i] <- ma[,a]*wa[,a]*N[,y,a,m,i] #units?
        
        ## add movement for age 1 here ## (ignore B and ssb for now since they are derived quantities)
        #for (m in 1:n.area) {
          #for (h in 1:n.sex){
          #N_hold[h,y,,a,i]<-N[h,y,a,,i]
          #N_hold[h,y,,a,i]<- t(prob.move[,,a])%*% N_hold[h,y,,a,i]
          #N[h,y,a,,i]<-N_hold[h,y,,a,i]
        #}#}
        
      }else {
        h <- 1
        for(h in 1:n.sex) {
          #Instantaneous Version
          F.a[h,y-1,a-1,m,i] <- sum(F.mort[,y-1,m,i]*va[,m,h,a-1]) #va dim = n.fish,n.area,n.sex,n.age
          Z.a[h,y-1,a-1,m,i] <- F.a[h,y-1,a-1,m,i] + mx[h,a-1]  #Natural mortality is NOT time-varying
          
          #Continuous
          surv[h,y-1,a-1,m,i] <- exp(-Z.a[h,y-1,a-1,m,i])
          mort[h,y-1,a-1,m,i] <- 1-surv[h,y-1,a-1,m,i]
          
          #Update
          N[h,y,a,m,i] <- N[h,y-1,a-1,m,i]*surv[h,y-1,a-1,m,i] 
          # B[h,y,a,i] <- B[h,y-1,a-1,i]*surv[h,y-1,a-1,i]
          B[h,y,a,m,i] <- N[h,y,a,m,i]*wa[h,a]
          ssb[,,y,m,i] <- ma*wa*N[,y,,m,i] #ssb dims = n.sex, n.age, n.year, n.area, n.sims; N dims = n.sex, n.year, n.age, n.area, n.sims
          #Total Catch (should this be y or y-1?)
          C.n[h,y,a-1,m,i] <- N[h,y,a-1,m,i] * (F.a[h,y-1,a-1,m,i]/Z.a[h,y-1,a-1,m,i]) * (1-exp(-1*Z.a[h,y-1,a-1,m,i])) #Catch in number 
          C.b[h,y,a-1,m,i] <- C.n[h,y,a-1,m,i] * wa[h,a-1]
          
          f <- 1
          for(f in 1:n.fish) {
            temp.F <- F.mort[f,y-1,m,i]*va[f,m,h,a-1]
            # temp.Z <- temp.F + mx[h,a-1]
            temp.Z <- sum(F.mort[,y-1,m,i]*va[,m,h,a-1]) + mx[h,a-1]
            
            harvest.n[h,y-1,a-1,f,m,i] <- N[h,y-1,a-1,m,i] * (temp.F/temp.Z) * (1-exp(-1*temp.Z)) #this is fleet specific catch, catch is fleets combined
            
            harvest.b[h,y,a-1,f,m,i] <- harvest.n[h,y-1,a-1,f,m,i] * wa[h,a-1]
          }#next gear
        }#next sex
        ## add movement for ages 
        #for (m in 1:n.area) {
        #for (h in 1:n.sex){
          #N_hold[h,y,,a,i]<-N[h,y,a,,i]
          #N_hold[h,y,,a,i]<-t(prob.move[,,a]%*%N_hold[h,y,,a,i])
          #N[h,y,a,,i]<-N_hold[h,y,,a,i]
        #}#}
      }
      
      if(a==n.age) {
        h <- 1
        for(h in 1:n.sex) {
          #Fish in Plus Group
          F.a[h,y-1,a,m,i] <- sum(F.mort[,y-1,m,i]*va[,m,h,a])
          Z.a[h,y-1,a,m,i] <- F.a[h,y-1,a,m,i] + mx[h,a]  #Natural mortality is NOT time-varying        
          
          #Continuous
          surv[h,y-1,a,m,i] <- exp(-Z.a[h,y-1,a,m,i])
          mort[h,y-1,a,m,i] <- 1-surv[h,y-1,a,m,i]
          
          #Update
          N[h,y,a,m,i] <- N[h,y,a,m,i] + N[h,y-1,a,m,i]*surv[h,y-1,a,m,i] #New Entrants (calculated above), plus existing plus group occupants.
          # B[h,y,a,i] <- B[h,y,a,i] + B[h,y-1,a,i]*surv[h,y-1,a,i] 
          B[h,y,a,m,i] <- N[h,y,a,m,i] * wa[h,a]
          ssb[,,y,m,i] <- ma*wa*N[,y,,m,i] #ssb dims = n.sex, n.age, n.year, n.area, n.sims; N dims = n.sex, n.year, n.age, n.area, n.sims
          
          #Total Catch
          C.n[h,y-1,a,m,i] <- N[h,y-1,a,m,i] * (F.a[h,y-1,a,m,i]/Z.a[h,y-1,a,m,i]) * (1-exp(-1*Z.a[h,y-1,a,m,i])) #Catch in number of halibut
          C.b[h,y-1,a,m,i] <- C.n[h,y-1,a,m,i] * wa[h,a]
          
          f <- 1
          for(f in 1:n.fish) {
            temp.F <- F.mort[f,y-1,m,i]*va[f,m,h,a]
            # temp.Z <- temp.F + mx[h,a]
            temp.Z <- sum(F.mort[,y-1,m,i]*va[,m,h,a]) + mx[h,a]
            # 
            harvest.n[h,y-1,a,f,m,i] <- N[h,y-1,a,m,i] * (temp.F/temp.Z) * (1-exp(-1*temp.Z))
            harvest.b[h,y-1,a,f,m,i] <- harvest.n[h,y-1,a,f,m,i] * wa[h,a]
          }#next gear
          
        }#next sex
        #add movement for plus group here
        #for (h in 1:n.sex){
          #N_hold[h,y,,a,i]<-N[h,y,a,,i]
          #N_hold[h,y,,a,i]<-t(prob.move[,,a])%*%N_hold[h,y,,a,i]
          #N[h,y,a,,i]<-N_hold[h,y,,a,i]
        #}
      }# If plus age group
     }#next age  
   } #close area
  } #close year
}#next i sim


#save a few things for reference for checking movement code
N.ref <- N
B.ref <- B
F.mort.ref <- F.mort
harvest.n.ref <- harvest.n
Fish.AC.ref <- Fish.AC

N[1,2,,,1]
N[1,1,,,1]
N.ref[1,2,,,1]

prob.move[,,1]



    ##### Sample the conditioning period population/data: ######
    #since this section is deterministic, all the sampling can be done at the end, not within the main loop.
i <- 1
y <- 2 
for(i in 1:n.sims) {
  ###first sample abundance and fishery indices for all years
  for(y in 2:43) {
    m <- 1
    for(m in 1:n.area) {
      #write a function that makes it easy to specify (by area) the yield ratio
      
      # sample longline survey RPN, 'current' year  -- check units
      Surv.RPN[,y,,m,i] <- sample_biom_abund(N[,y,,m,i],sigma=LLsurvRPNsigma, type='lognorm', seed=12345) #need to create a more sophisticated seed higher in the code
      #(we'd talked about concatonating 'sim # + year' for seed)
      
      # longline/fixed gear fishery CPUE/RPW  -- check units
      Fish.RPW[,y,,m,i] <- sample_biom_abund(B[,y,,m,i], sigma=LLfishRPWsigma, type='lognorm', seed=333)
    } #next area m
  } #close year 
  
  ###then sample age comps for a different set of years for each set of comps
  # longline/fixed gear post-IFQ fishery age comps in numbers 
  y <- 24  
  for(y in 24:42) { #for 1999-2017 (a one year lag in having age comps accessible)
    m <- 1
    for(m in 1:n.area) {
      h<-1
      for(h in 1:n.sex){
        # longline/fixed gear post-IFQ fishery age comps in numbers 
        sample_age_comps(harvest.n[h,y,,2,m,i], Nsamp=LLsurvAC_sampsize, cpar=NULL) 
        Fish.AC[h,y,,m,i] <- obs.comp
      } #next sex
    } #next area m
  } #close year 
  
  # longline survey age comps in numbers
  y <- 21  
  for(y in 21:42) { #for 1996-2017
    m <- 1
    for(m in 1:n.area) {
      h<-1
      for(h in 1:n.sex){
        # longline survey age comps in numbers
        sample_age_comps(N[h,y,,m,i], Nsamp=LLfishAC_sampsize, cpar=NULL) #true.props, Nsamp, cpar
        Surv.AC[h,y,,m,i] <- obs.comp
      } #next sex
    } #next area m
  } #close year 

  y <- 2  
  for(y in 2:43) {
    m <- 1
    for(m in 1:n.area) {
    ######### aggregate OM data across age, sex and/or areas for EM and track over time
    #sum catch at age to a single catch (actually, probably harvest) for year y, summed over areas and sexes, for each gear
    #fish 1 - US fixed gear pre-IFQ, 2 - US fixed post-IFQ, 3 - US trawl, 4 - foreign fixed gear
    OM_fixed_catch[y,i] <- sum(harvest.b[,y,,2,,i]) #2 - US fixed post-IFQ
    OM_trawl_catch[y,i] <- sum(harvest.b[,y,,3,,i]) #3 - US trawl
    
    #sum RPN to one value for the year and sim (sum over age, sex area) - do these need to be weighted in some way?
    OM_Surv.RPN[y,i] <- sum(Surv.RPN[,y,,,i])
    OM_Fish.RPW[y,i] <- sum(Fish.RPW[,y,,,i])
    
    #call the function to aggregate age comps for fishery and survey across areas (weight by catch/harvest at age in each area, sum across areas, then spit out a vector or age comps)
    #for sure need to aggregate over sex and areas, may not need to weight by catch in areas
    #OM_Fish.RPW.age[,y,,i] <- aggr_agcomp(Fish.AC[,y,,,i], cond_catch_at_age[y,,,]) #is harvest.n the right one to use?
    #OM_Surv.RPN.age[,y,,i] <- aggr_agcomp(Surv.AC[,y,,,i], harvest.num)
    } #next area m
  } #close year
}#next i sim 

    ## Build the data: read in a .dat file, advance #years, year counts, add data generated in current year to matrices/arrays  
    ## then generate the updated .dat file to be pushed to the EM
    build_conditioning_datfile()  #note this is mostly done, but needs testing/validation once the age comp sampling and aggregating code is done
    





#=====================================================================================
    # Forward Simulation =============================================================
    # Temporarily no movement, need to add that in in the future
      
#Loop order: sim, year, area, age, sex.
# Order - calculate F associated with catch permitted from previous year's assessment and apportionment, 
#recruit new cohort, F and M occur, movement occurs, sample the 
#(moved) OM population, add OM data to .dat file, run EM, apply apportionment [end of single 'year cycle']
# ...then start over at beginning with a new year
    
#specify 2018 catch by area, gear, in biomass (kt) check units
dim(C.b)
dim(cond_catch_at_age)

  for (m in 1:n.area) {
    for(a in 1:n.age) {
      for (h in 1:n.sex) {
      C.b[h,43,a,m,] <- cond_catch_at_age[42,m,h,a]
      }}}

for(i in 1:n.sims) {
  for (m in 1:n.area) {
    #for(a in 1:n.age) {
      for (f in 1:n.fish) {
        for (h in 1:n.sex) {
temp.Fmort <- estimate_Fmort4catch(catch=C.b[h,43,,m,i], 
                                   temp.selex=va[f,m,h,],
                                   temp.N=N[h,y-1,,m,i], 
                                   wa=wa, mx=mx, 
                                   bisection=TRUE)$Fmort
F.mort[f,y,m,i] <- temp.Fmort 
      }}}}#}




area <- 1
i <- 1
for(i in 1:n.sims) {
  print(paste('Sim:',i,'of',n.sims))
  y <- 44  
  for(y in 44:n.year) {
    # somewhere up here, set a bunch of seeds...
    
    m <- 1
    for (m in 1:n.area) {

    #take most recent (2018) assessment and outputted ABC apportionment and calculate F 
    #f <- 1
    #for(f in 1:n.fish) {
      # UPDATE: Instead of setting a fixed F, we can now set a fixed catch
      #  biomass and find the F for the specific fishery
      #  that will match that catch. This is so we can input the catch from the EM projection and apportionment output and then calculate the associated F.
      # need to think about how we deal with trawl vs longline F here - do we specify these catches currently?
      # Here lets specify an arbitrary fixed catch (kg) -- will need to eventually read it in for the first year here (based on the most recent assessment 
      #and harvest control rule output)...future year's catch will come from the EM/projectiom model and apportionment application, so this section still needs work
      #catch <- 1e7 # 10 million kg or 10,000 metric tons
      
      # Find Fishing Mortality Rate for Apportioned Catch Level ------------------------  #right now F is a single value - probably need to have spatial F?
      #temp.Fmort <- estimate_Fmort4catch(catch=catch, 
                                           #temp.selex=va[f,area,,],
                                           #temp.N=N[,y-1,,m,i], 
                                           #wa=wa, mx=mx, 
                                           #bisection=TRUE)$Fmort
      #Fmort[f,y,m,i] <- temp.Fmort  #0.1
    #}#next g 
        
    a <- 1
    for(a in 1:n.age) {
      #Update Numbers and Biomass Matrix
      if(a==1) { #Age-1
        N[,y,a,m,i] <- 0.5*recruits.area[y-1,m,i] #multiplying by 0.5 to split evenly between sexes
        B[,y,a,m,i] <- 0.5*recruits.area[y-1,m,i]*wa[,a]
        # N[,y,a] <- rec[,y-1]/wa[,a]
        # B[,y,a] <- rec[,y-1]
        ssb[,,y,m,i] <- ma*wa*N[,y,,m,i] #ssb dims = n.sex, n.age, n.year, n.area, n.sims; N dims = n.sex, n.year, n.age, n.area, n.sims
        
        ## add movement for age 1 here ##
        
      }else {
        h <- 1
        for(h in 1:n.sex) {
          #Instantaneous Version
          F.a[h,y-1,a-1,m,i] <- sum(F.mort[,y,m,i]*va[,m,h,a-1]) #va dim = n.fish,n.area,n.sex,n.age
          Z.a[h,y-1,a-1,m,i] <- F.a[h,y-1,a-1,m,i] + mx[h,a-1]  #Natural mortality is NOT time-varying
          
          #Continuous
          surv[h,y-1,a-1,m,i] <- exp(-Z.a[h,y-1,a-1,m,i])
          mort[h,y-1,a-1,m,i] <- 1-surv[h,y-1,a-1,m,i]
          
          #Update
          N[h,y,a,m,i] <- N[h,y-1,a-1,m,i]*surv[h,y-1,a-1,m,i] 
          # B[h,y,a,i] <- B[h,y-1,a-1,i]*surv[h,y-1,a-1,i]
          B[h,y,a,m,i] <- N[h,y,a,m,i]*wa[h,a]
          ssb[,,y,m,i] <- ma*wa*N[,y,,m,i] #ssb dims = n.sex, n.age, n.year, n.area, n.sims; N dims = n.sex, n.year, n.age, n.area, n.sims
          #Total Catch
          C.n[h,y-1,a-1,m,i] <- N[h,y-1,a-1,m,i] * (F.a[h,y-1,a-1,m,i]/Z.a[h,y-1,a-1,m,i]) * (1-exp(-1*Z.a[h,y-1,a-1,m,i])) #Catch in number 
          C.b[h,y-1,a-1,m,i] <- C.n[h,y-1,a-1,m,i] * wa[h,a-1]
          
          f <- 1
          for(f in 1:n.fish) {
            temp.F <- F.mort[f,y,m,i]*va[f,m,h,a-1]
            # temp.Z <- temp.F + mx[h,a-1]
            temp.Z <- sum(F.mort[,y,m,i]*va[,m,h,a-1]) + mx[h,a-1]
            
            harvest.n[h,y-1,a-1,f,m,i] <- N[h,y-1,a-1,m,i] * (temp.F/temp.Z) * (1-exp(-1*temp.Z)) #this is fleet specific catch, catch is fleets combined
            
            harvest.b[h,y-1,a-1,f,m,i] <- harvest.n[h,y-1,a-1,f,m,i] * wa[h,a-1]
          }#next gear
        }#next sex
        ## add movement for ages 
      }
      
      if(a==n.age) {
        h <- 1
        for(h in 1:n.sex) {
          #Fish in Plus Group
          F.a[h,y-1,a,m,i] <- sum(F.mort[,y,m,i]*va[,m,h,a])
          Z.a[h,y-1,a,m,i] <- F.a[h,y-1,a,m,i] + mx[h,a]  #Natural mortality is NOT time-varying        
          
          #Continuous
          surv[h,y-1,a,m,i] <- exp(-Z.a[h,y-1,a,m,i])
          mort[h,y-1,a,m,i] <- 1-surv[h,y-1,a,m,i]
          
          #Update
          N[h,y,a,m,i] <- N[h,y,a,m,i] + N[h,y-1,a,m,i]*surv[h,y-1,a,m,i] #New Entrants (calculated above), plus existing plus group occupants.
          # B[h,y,a,i] <- B[h,y,a,i] + B[h,y-1,a,i]*surv[h,y-1,a,i] 
          B[h,y,a,m,i] <- N[h,y,a,m,i] * wa[h,a]
          ssb[,,y,m,i] <- ma*wa*N[,y,,m,i] #ssb dims = n.sex, n.age, n.year, n.area, n.sims; N dims = n.sex, n.year, n.age, n.area, n.sims
          
          #Total Catch
          C.n[h,y-1,a,m,i] <- N[h,y-1,a,m,i] * (F.a[h,y-1,a,m,i]/Z.a[h,y-1,a,m,i]) * (1-exp(-1*Z.a[h,y-1,a,m,i])) #Catch in number of halibut
          C.b[h,y-1,a,m,i] <- C.n[h,y-1,a,m,i] * wa[h,a]
          
          f <- 1
          for(f in 1:n.fish) {
            temp.F <- F.mort[g,y,m,i]*va[f,m,h,a]
            # temp.Z <- temp.F + mx[h,a]
            temp.Z <- sum(F.mort[,y,m,i]*va[,m,h,a]) + mx[h,a]
            # 
            harvest.n[h,y-1,a,f,m,i] <- N[h,y-1,a,m,i] * (temp.F/temp.Z) * (1-exp(-1*temp.Z))
            harvest.b[h,y-1,a,f,m,i] <- harvest.n[h,y-1,a,f,m,i] * wa[h,a]
          }#next gear
        }#next sex
        #add movement for plus group here
      }# If plus age group
      
    }#next age  
    } #next area
    
    ######Sample population for age comps, survey index, etc. 
    ##### Generate EM Data: ######
    m <- 1
    for (m in 1:n.area) {
        # observed catch (based on what for F?), 'current' year, for 6 areas then combine to 3 and to 1 
          #write a function that makes it easy to specify (by area) the yield ratio
      
        # longline survey RPN, 'current' year  -- check units
        Surv.RPN[,y,,m,i] <- sample_biom_abund(N[,y,,m,i],sigma=0.2, type='lognorm', seed=12345) #need to create a more sophisticated seed higher in the code
        #(we'd talked about concatonating 'sim # + year' for seed)
      
        # longline/fixed gear fishery CPUE/RPW  -- check units
        Fish.RPW[,y,,m,i] <- sample_biom_abund(B[,y,,m,i], sigma=0.4, type='lognorm', seed=333)
        
        for(h in 1:n.sex){
        # longline/fixed gear fishery age comps in numbers (not proportions yet) 
        Fish.AC[h,y,,m,i] <- sample_age_comps(N[h,y,,m,i], Nsamp=20, cpar=NULL) 
        
        # longline survey age comps in numbers (not proportions yet) single sex
        Surv.AC[h,y,,m,i] <- sample_age_comps(N[h,y,,m,i], Nsamp=20, cpar=NULL) #true.props, Nsamp, cpar
        }
    } #next area m
    
    ######### aggregate OM data across age, sex and/or areas for EM and track over time
    #sum catch at age to a single catch (actually, probably harvest) for year y, summed over areas and sexes, for each gear
        #fish 1 - US fixed gear pre-IFQ, 2 - US fixed post-IFQ, 3 - US trawl, 4 - foreign fixed gear
        OM_fixed_catch[y,i] <- sum(harvest.b[,y,,2,,i]) #2 - US fixed post-IFQ
        OM_trawl_catch[y,i] <- sum(harvest.b[,y,,3,,i]) #3 - US trawl
        
    #sum RPN to one value for the year and sim (sum over age, sex area) - do these need to be weighted in some way?
        OM_Surv.RPN[y,i] <- sum(Surv.RPN[,y,,,i])
        OM_Fish.RPW[y,i] <- sum(Fish.RPW[,y,,,i])
        
    #call the function to aggregate age comps for fishery and survey across areas (weight by catch/harvest at age in each area, sum across areas, then spit out a vector or age comps)
        #OM_Fish.RPW.age[,y,,i] <- aggr_agecomp(Fish.AC, harvest.num) #is harvest.n the right one to use?
        #OM_Surv.RPN.age[,y,,i] <- aggr_agecomp(Surv.AC, harvest.num)
    
    ## Build the data: read in a .dat file, advance #years, year counts, add data generated in current year to matrices/arrays  
    ## then generate the updated .dat file to be pushed to the EM
        #build_datfile()  #note this is mostly done, but needs testing/validation once the age comp sampling and aggregating code is done
    
        #calculate the moving average ratio between gear types (F ratio)    CODE THIS
        
    #=============================================================
    #### Conduct Assessment #### 
    #2) Call ADMB Model
    #add code here
        #Dana think about the code in the EM here between gear types for distibuting F across selectivities
    #=============================================================
    #### Determine SPR ####
    # extract SPR (read in a report file)
    # this will come out of my EM  CHECK IF THERE ARE CODE CHANGES IN THE EM ABOUT F RATIO
    #=============================================================
    #### Set Harvest Limits & apply apportionment method we are testing ####
    # call the apportionment method here
    # output spatial catch limit (by gear?)
        
    
    ### side notes:
    # No Recruitment relationship  Can we change this so it reads in a rec value from a separate file which draws N simulations * N years worth of rec values all 
    # at once so the same recruitment can be applied to single and spatial models? Could make it so that if SSB is 0, 0 rec is used instead so we 
    # don't spontaneously generate fish if the pop crashes.
    #rec[,y-1,m,i] <- 0.5 * exp(mu_rec*(1+mx[,1]) + rnorm(1, 0, sigma_rec)) #Note this has the addition to account for assessment model predicting recruitment to age 2. 
        #Ricker
    # rec[,y-1] <- 0.5 * ricker_recruit(ssb[y-1], steep, bo)
    #Beverton-Holt
    # rec[,y-1,i] <-  0.5 * beverton_holt_recruit(sum(ssb[,,y-1,i]), steep, bo=ro) * exp(rnorm(1,0,sigma_rec) - ((sigma_rec^2)/2))

  }#next y
  
}#next i



#write.table(N[1,,,1,1], file="male A1 sim1.csv")

