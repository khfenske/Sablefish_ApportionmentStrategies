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
dir.admb <- file.path(wd,"admb/Single_area")
dir.R <- file.path(wd,"R")
dir.x <- file.path("C:/Repositories/hidden files with conf data") #this is for Kari's file paths - Curry and Dana, comment this out and 
# then un-comment-out the following line to use the dummy file
#dir.x <- dir.data #change this path to whatever place you have 
#the confidential catch data files stored. DO NOT LOAD TO GITHUB.

EM_name <- "tem"

# Call all packages and libraries once, up front ===================
# for Run_Sim_data_plots.R
require(dplyr)
require(reshape2)
require(tidyverse)
require(ggplot2)
#for extract-pars.R
require(tidyverse)
#require(readxl)
require(xlsx)
# for sample-age-comps.R
require(gtools) 
#for .dat file building
require(PBSmodelling)
require(R2admb)
        
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
source(file.path(dir.R,'copy-admb-sim.R')) #Function to copy files to the new sim folders

# Compile ADMB Code ==============================================  uncomment for MAC users or if Kari ever gets her path fixed
#setwd(dir.admb)
#os<-.Platform$OS.type
#if(os=="unix") {system("admb tem") }
#if(os=="windows"){shell("admb tem") }
#setwd(wd)

# Extract Parameters =============================================
#extract_pars(input.file="Sablefish_Input_matchMGMTqselex.xlsx")
extract_pars(input.file="Sablefish_Input.xlsx")
extract_catch(dir.x,input.file="catch_input_conditioning.xlsx") #using a separate function for this because catch by gear and area has
#confidential data, catch is in kt, change this function to read in the dummy spreadsheet (fake catch data) for running this for now
 
# Read in Movement Rates =========================================
move.type = 1 #1 is base movement, 2 is no movement, all move

if (move.type==1) {
  prob.move <- read_movement_rates(input.file="Sablefish_Input.xlsx")
  }
if (move.type==2) {
  prob.move <- array(data=NA, dim=c(n.area,n.area,n.age), dimnames=list(areas,areas,ages))
  for (m in 1:n.age) {
  prob.move[1,,m] <- c(1,0,0,0,0,0)
  prob.move[2,,m] <- c(0,1,0,0,0,0)
  prob.move[3,,m] <- c(0,0,1,0,0,0)
  prob.move[4,,m] <- c(0,0,0,1,0,0)
  prob.move[5,,m] <- c(0,0,0,0,1,0)
  prob.move[6,,m] <- c(0,0,0,0,0,1)
  }}
if (move.type==3) {
  prob.move <- array(data=NA, dim=c(n.area,n.area,n.age), dimnames=list(areas,areas,ages))
  for (m in 1:n.age) {
  prob.move[1,,m] <- c(1/6,1/6,1/6,1/6,1/6,1/6)
  prob.move[2,,m] <- c(1/6,1/6,1/6,1/6,1/6,1/6)
  prob.move[3,,m] <- c(1/6,1/6,1/6,1/6,1/6,1/6)
  prob.move[4,,m] <- c(1/6,1/6,1/6,1/6,1/6,1/6)
  prob.move[5,,m] <- c(1/6,1/6,1/6,1/6,1/6,1/6)
  prob.move[6,,m] <- c(1/6,1/6,1/6,1/6,1/6,1/6)
  }}

# Calculate Selectivity ==========================================

selex <- list() #Selectivity List

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

va_surv <- array(dim=c(n.surv,n.sex,n.age), dimnames=list(surv,sexes,ages))
  i <- 1
  for(i in 1:n.surv) {
    va_surv[i,,] <- calc_selectivity(type='surv', fleet=surv[i])
  }
# Create Simulation Objects =======================================
#NOTE: Currently calculates data for n areas, where n is defined in the input spreadsheet (n.areas)  
create_sim_objects() #sets up all the spatial arrays to hold simulated data
#set up an array to hold the N array during movement code
N_hold <- array(dim=c(n.sex, n.year, n.area, n.age, n.sims), dimnames=list(sexes, years, areas, ages, sims))

# Simulate Annual Recruitments ====================================
#setup_years <- 39 #number of years to run the loop setting up the initial population and building initial dat file
create_sim_recruitments(mu_rec=mu_rec, sigma_rec=sigma_rec, rho_rec=NULL, 
                        n.year=n.year, n.sims=n.sims, seed=111) #Creates rec object 

# divide annual recruitments into areas - the values for area.props are from the proportions of age-2 fish by area 
#from the LL survey, average across all years.
#see the excel file in the repository (in data folder)
rec.by.area.props <- c(0.14,0.07,0.14,0.43,0.14,0.09)
for(i in 1:n.sims){
for(y in 1:n.year){
  recruits.area[y,,i] <- spatial_rec(rec[i,y],area.props=rec.by.area.props, ss=100, seed=1)/1000000      # sexes combined, recruitment in millions
}}

#get catchability into a different format 
catchability$fishery
q_fish <- matrix(data=NA, nrow=n.fish, ncol=n.area)
for(m in 1:n.area){
  q_fish[1,m] <- catchability$fishery$value[m]
  q_fish[2,m] <- catchability$fishery$value[m+6]
  q_fish[3,m] <- catchability$fishery$value[m+12]
  q_fish[4,m] <- catchability$fishery$value[m+18]
}
 
catchability$survey
q_surv <- matrix(data=NA, nrow=2, ncol=n.area)
for(m in 1:n.area){
  q_surv[1,m] <- catchability$survey$value[m]
  q_surv[2,m] <- catchability$survey$value[m+6]
}
# ==================================================================================
# Initialize Population - Conditioning the simulations (year 1, or 1976) ===========
N.by.area.props <- as.vector(c(0.123525838,0.138168043,0.112731838,0.401168634,0.102462464,0.121943183)) #mean proportion 
# by area for n.areas from survey RPN (1979-2018) (from SurveyRPN.xlsx)
#read in proportions at age for males and females from 1976 estimated N from the single area EM, which are the 
#same for both sexes so read in once here 
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
LLsurvAC_sampsize <- 200
LLfishAC_sampsize <- 200

##### Condition year 1 (aka 1976):
# since we track numbers in this model, I will initialize things in numbers instead of biomass
i <- 1
m <- 1
a <- 1
for(i in 1:n.sims) {
  for(m in 1:n.area) {
    for(a in 1:n.age) {
    N[,1,a,m,] <- Nstart * 0.5 * (Nprop.by.age[a]) #Nstart is in millions (check units), 0.5 is to divide equally between sexes 
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
      # Find Fishing Mortality Rate for Apportioned Catch Level ------------------------ 
      temp.Fmort <- estimate_Fmort4catch(catch=0.5*temp.catchnumbiom[y-1,f,m], 
        temp.selex=va[f,m,,],
        temp.N=N[,y-1,,m,i], 
          wa=wa, mx=mx, 
        bisection=TRUE)$Fmort
      F.mort[f,y-1,m,i] <- temp.Fmort  
      } #next fishery/gear 
    } #close area
    
    for(a in 1:n.age) {
      #Update Numbers and Biomass Matrix
      if(a==1) { #Age 1
        for(m in 1:n.area) {
        #cond.rec are values read in from an excel file in the extract-pars.R function - they are estimated 
          #recruitments for 1977-2018 from the single area EM
        N[,y,a,m,i] <- (1/6)*0.5*(cond.rec$Recruitment[y-1]) #multiplying by 0.5 to split evenly between sexes, y-1 calls the first year of cond.rec list 
      } # close area

        
      }else {
        for (m in 1:n.area){
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
          }#next sex
        } #close area

      } #close else
      
      if(a==n.age) {
        for (m in 1:n.area) {
          h <- 1
          for(h in 1:n.sex) {
          #Fish in Plus Group
          F.a[h,y-1,a,m,i] <- sum(F.mort[,y-1,m,i]*va[,m,h,a])
          Z.a[h,y-1,a,m,i] <- F.a[h,y-1,a,m,i] + mx[h,a]         
          
          #Continuous
          surv[h,y-1,a,m,i] <- exp(-Z.a[h,y-1,a,m,i])
          mort[h,y-1,a,m,i] <- 1-surv[h,y-1,a,m,i]
          
          #Update
          N[h,y,a,m,i] <- N[h,y,a,m,i] + N[h,y-1,a,m,i]*surv[h,y-1,a,m,i] #New Entrants (calculated above), plus existing plus group occupants.
          }#next sex
      } #close area

      }# If plus age group
    }#next age  
    
    for (h in 1:n.sex){ 
      for (a in 1:n.age){
        for (m in 1:n.area){
        N_hold[h,y,m,a,i]<-N[h,y,a,m,i]  
        }  
        N_hold[h,y,,a,i]<-t(prob.move[,,a])%*%N_hold[h,y,,a,i]
        for (m in 1:n.area){
        N[h,y,a,m,i]<-N_hold[h,y,m,a,i]
        } #close area
      } #close age
    } #close sex
    

    #calculate all the derived quantities at once, after movement so that all N, B, ssb, C, and harvest reflect the moved population values
    for (a in 1:n.age){
      for (m in 1:n.area){
    B[,y,a,m,i] <- N[,y,a,m,i] * wa[,a]
    ssb[,a,y,m,i] <- ma[,a]*wa[,a]*N[,y,a,m,i]
      for (h in 1:n.sex){ 
      C.n[h,y-1,a-1,m,i] <- N[h,y-1,a-1,m,i] * (F.a[h,y-1,a-1,m,i]/Z.a[h,y-1,a-1,m,i]) * (1-exp(-1*Z.a[h,y-1,a-1,m,i])) #Catch in number 
      C.b[h,y-1,a-1,m,i] <- C.n[h,y-1,a-1,m,i] * wa[h,a-1]
        
      for(f in 1:n.fish) {
        temp.F <- F.mort[f,y-1,m,i]*va[f,m,h,a-1]
        temp.Z <- sum(F.mort[,y-1,m,i]*va[,m,h,a-1]) + mx[h,a-1]
        harvest.n[h,y-1,a-1,f,m,i] <- N[h,y-1,a-1,m,i] * (temp.F/temp.Z) * (1-exp(-1*temp.Z)) #this is fleet specific catch, catch is fleets combined
        harvest.b[h,y-1,a-1,f,m,i] <- harvest.n[h,y-1,a-1,f,m,i] * wa[h,a-1]
        }#next gear    
       } #close sex
      }#close area
    }#close age
    # plus group age calcs for catch
    for (m in 1:n.area){
      for (h in 1:n.sex){ 
        C.n[h,y-1,30,m,i] <- N[h,y-1,30,m,i] * (F.a[h,y-1,30,m,i]/Z.a[h,y-1,30,m,i]) * (1-exp(-1*Z.a[h,y-1,30,m,i])) #Catch in number 
        C.b[h,y-1,30,m,i] <- C.n[h,y-1,30,m,i] * wa[h,30]
        for(f in 1:n.fish) {
        temp.F <- F.mort[f,y-1,m,i]*va[f,m,h,a]
        temp.Z <- sum(F.mort[,y-1,m,i]*va[,m,h,a]) + mx[h,a]
        harvest.n[h,y-1,a,f,m,i] <- N[h,y-1,a,m,i] * (temp.F/temp.Z) * (1-exp(-1*temp.Z))
        harvest.b[h,y-1,a,f,m,i] <- harvest.n[h,y-1,a,f,m,i] * wa[h,a]
        }#next gear
      } #close sex
    }#close area
  } #close year
}#next i sim




    ##### Sample the conditioning period population/data: ######
    #since this section is deterministic, all the sampling can be done at the end, not within the main loop.

i <- 1
y <- 2 
for(i in 1:n.sims) {
  
  ###first sample abundance and fishery indices for all years
  for(y in 15:43) {
    m <- 1
    for(m in 1:n.area) {
      #write a function that makes it easy to specify (by area) the yield ratio
      # sample longline survey RPN, 'current' year  -- check units
      # is the *selectivity needed here? Or is it already incorporated into the N calcs via F?
      #Surv.RPN[,y,,m,i] <- sample_biom_abund(N[,y,,m,i]*selex$surv$USLongline[h,]*q_surv[1,m],sigma=LLsurvRPNsigma, type='lognorm', seed=c(y+i)) 
      Surv.RPN[,y,,m,i] <- sample_biom_abund(N[,y,,m,i]*q_surv[1,m]*selex$surv$USLongline,sigma=LLsurvRPNsigma, type='lognorm', seed=c(y+i+222)) #*selex$surv$USLongline[h,]*q_surv[1,m]
          } #next area m
  } #close year 
  
  for(y in 15:42) {
    m <- 1
    for(m in 1:n.area) {
      # longline/fixed gear fishery CPUE/RPW , can use LLfishAC_sampsize (specified above) if you want to match the comp draws here with the dat file maker comp sizes, or 
      #specify another comp sample size here
      Fish.RPW[,y,,m,i] <- sample_biom_abund(B[,y,,m,i]*q_fish[2,m]*selex$fish$USfixed_postIFQ[m,,], sigma=LLfishRPWsigma, type='lognorm', seed=c(y+i+14)) #14 is just a randomly chosen # to make seed diff from above
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
        sample_age_comps(harvest.n[h,y,,2,m,i]*selex$fish$USfixed_postIFQ[m,h,], Nsamp=LLsurvAC_sampsize, cpar=1, seed=c(y+i+134)) 
        Fish.AC[h,y,,m,i] <- obs.comp
      } #next sex
    OM_Fish.RPW.age[y,,i] <- aggr_agecomp(Fish.AC[,y,,,i], cond_catch_at_age[y,2,,,],1) #aggregate comps by sex and area, weight by catch in area
    } #next area m
  } #close year 
  
  # longline survey age comps in numbers
  y <- 21  
  for(y in 21:42) { #for 1996-2017
    m <- 1
    for(m in 1:n.area) {
      h<-1
      for(h in 1:n.sex){
        # longline survey age comps in numbers, can use LLfishAC_sampsize (specified above) if you want to match the comp draws here with the dat file maker comp sizes, or 
        #specify another comp sample size here
        sample_age_comps(N[h,y,,m,i]*selex$surv$USLongline[h,], Nsamp=LLfishAC_sampsize, cpar=1, seed=y+i+76) #true.props, Nsamp, cpar
        Surv.AC[h,y,,m,i] <- obs.comp
      } #next sex
      OM_Surv.RPN.age[y,,i] <- aggr_agecomp(Surv.AC[,y,,,i], Surv.RPN[,y,,,i],2)
    } #next area m
  } #close year 
  
    ######### aggregate OM data across age, sex and/or areas for EM and track over time
    #sum catch at age to a single catch (actually, probably harvest) for year y, summed over areas and sexes, for each gear
    #fish 1 - US fixed gear pre-IFQ, 2 - US fixed post-IFQ, 3 - US trawl, 4 - foreign fixed gear
  y <- 2  
  for(y in 2:20) {
    m <- 1
    for(m in 1:n.area) {
      OM_fixed_catch[y,i] <- sum(harvest.b[,y,,1,,i]) #2 - US fixed pre-IFQ 
    } #next area m
  } #close year 
  y <- 21  
  for(y in 21:42) {
    m <- 1
    for(m in 1:n.area) {
    OM_fixed_catch[y,i] <- sum(harvest.b[,y,,2,,i]) #2 - US fixed post-IFQ
    } #next area m
  } #close year 
  y <- 2  
  for(y in 2:42) {
    m <- 1
    for(m in 1:n.area) {
      OM_trawl_catch[y,i] <- sum(harvest.b[,y,,3,,i]) #3 - US trawl
    } #next area m
  } #close year   
  y <- 15  
  for(y in 15:42) {
    m <- 1
    for(m in 1:n.area) {
      #sum RPW to one value for the year and sim (sum over age, sex area) - do these need to be weighted in some way?
      OM_Fish.RPW[y,i] <- sum(Fish.RPW[,y,,,i]) 
    } #next area m
  } #close year 
  
    y <- 15  
    for(y in 15:43) {
      m <- 1
      for(m in 1:n.area) {
      #sum RPN to one value for the year and sim (sum over age, sex area) - do these need to be weighted in some way?
      OM_Surv.RPN[y,i] <- sum(Surv.RPN[,y,,,i]) 
      } #next area m
    } #close year    

}#next i sim 

    ## Build the data: read in a .dat file, advance #years, year counts, add data generated in current year to matrices/arrays  
    ## then generate the updated .dat file to be pushed to the EM
    #only building a single conditioning dat file instead of 1 for each sim since they should all be the same for this project (for now)
    build_conditioning_datfile()  
    #sim_plot_initpop()
    
    
    
#=====================================================================================
# Forward Simulation =============================================================

      
#Loop order: sim, year, area, age, sex.
# Order - read in output from last assessment, calculate F associated with catch permitted from previous year's assessment and apportionment, 
#recruit new cohort, F and M occur, movement occurs, sample the 
#(moved) OM population, add OM data to .dat file, run EM, apply apportionment [end of single 'year cycle']
# ...then start over at beginning with a new year
    
    #a function to call the EM (call this inside the loops below)
    run.model <- function() {
      setwd(dir.temp) # Temporary simulation-specific working directory (creates a folder for every sim)
      system.time( # keeping track of time for run
         invisible(system(paste0(EM_name," -nox -nohess"),wait=T))) #is this correct?
      
      # Need to update for MAC (Curry is needy)
      #require(R2admb)
      #system.time(R2admb::run_admb(EM_name, verbose=TRUE))
      
      #remove existing files
      cor.name<-paste0(EM_name,".cor")
      std.name<-paste0(EM_name,".std")
      
      if (file.exists(cor.name)) {
        file.remove(cor.name)
      }
      if (file.exists(std.name)) {
        file.remove(std.name)
      }
      setwd(wd) #return to original working directory
    } #close run.model function
    
#Prime the pump (of apportionment): 2019/year 44 fixed gear catch levels from the apportioned 2018 projection, trawl catches are placeholders, order is BS-AI-WG-CG-WY-EY/SEO
  for(y in 43:44) {  
    for(i in 1:n.sims){
    apportioned_C[y,1,,i] <- c(0,0,0,0,0,0) 
    apportioned_C[y,2,,i] <- c(1.501,2.03,1.659,5.246,1.765,3.179)
    apportioned_C[y,3,,i] <- c(0.25,0.25,0.25,0.25,0.25,0.25) ## these are just placeholders!! for 2018
    apportioned_C[y,4,,i] <- c(0,0,0,0,0,0)
    }}

#temp place for the apportionment functions (maybe get them into a separate function for simpler code)
    #apport.opt = 1: equal to all areas
    equal_apportionment <- function(ABC.total,n.areas) {
      ABC.EM <- vector(length=n.areas)
      for (a in 1:n.areas) {
        ABC.EM[a]<-ABC.total/n.areas
      }
      return(ABC.EM)
    }
    
    #apport.opt = 2: fixed at current fixed values
    current.props <- c(current.props1,current.props2,current.props3,
                       current.props4,current.props5,current.props6) #rough spatial apportionment proportions after whales, BS-AI-WG-CG-WY-EY
    #probably should use the before whale correction values in the real simulation
    fixed_apportionment <- function(ABC.total,n.areas,fixed_prop) {
      ABC.EM <- vector(length=n.areas)
      for (a in 1:n.areas) {
        ABC.EM[a]<-ABC.total*fixed_prop[a]
      }
      return(ABC.EM)
    }
    
    #apport.opt = 3: equilibrium (stationary movement matrix)
    equilib.props <- c(equilib.props1,equilib.props2,equilib.props3,
                       equilib.props4,equilib.props5,equilib.props6) #stationary distribution of movement matrix, BS-AI-WG-CG-WY-EY
    #will need to come up with a better way to split Ey and WY from EG - because right now it's a kludge. (or just lump them)
    equilib_apportionment <- function(ABC.total,n.areas,equilib_prop) {
      ABC.EM <- vector(length=n.areas)
      for (a in 1:n.areas) {
        ABC.EM[a]<-ABC.total*equilib_prop[a]
      }
      return(ABC.EM)
    }
    
    #apport.opt = 4: status quo apportionment (5 year exponential weighting of survey and fishery)
    NPFMC_apportionment <- function(ABC.total,n.areas,fish.data,biom.data) { 
      ABC.EM <- vector(length=n.areas) #creating the output vector to hold apportioned ABCs
      fish.data.prop <- matrix(data=NA, ncol=n.areas,nrow=5)
      fish.data.prop.wt <- matrix(data=NA, ncol=n.areas,nrow=5)
      fish.prop.sum <- vector(length=n.areas)
      biom.data.prop <- matrix(data=NA, ncol=n.areas,nrow=5)
      biom.data.prop.wt <- matrix(data=NA, ncol=n.areas,nrow=5)
      biom.prop.sum <- vector(length=n.areas)
      wts <- c(0.0625, 0.0625, 0.125, 0.25, 0.5) #the weighting values
      EAprop<-24632/(21820+24632) #proportion of western aleutians to eastern aleutians, applied to fishery RPW because
      #the fishery fishes farther along the aleutians than the survey or than the AI management area
      for (y in 1:n.year){
        for (m in 1:n.areas){
          fish.data[y,m] <- sum(Fish.RPW[,y,,m])
          biom.data[y,m] <- sum(Surv.RPN[,y,,m])
        }}
      fish.data[,2] <- fish.data[,2]*(c(rep(EAprop,times=length(fish.data[,2])))) # multiply the fishery data by the E-W aleutians proportion
      for (i in (length(fish.data[,1])-4):length(fish.data[,1])) {
        for (a in 1:n.areas) {
          m <- i-(length(fish.data[,1])-5)
          fish.data.prop[m,a] <- fish.data[i,a]/sum(fish.data[i,]) #calc proportion by year across areas for fishery data            
          biom.data.prop[m,a] <- biom.data[i,a]/sum(biom.data[i,]) #calc proportion by year across areas for survey data
          }}
      for (i in 1:length(fish.data.prop[,1])) {
        for (a in 1:n.areas) {    
          fish.data.prop.wt[i,a] <- fish.data.prop[i,a]*wts[i]
          biom.data.prop.wt[i,a] <- biom.data.prop[i,a]*wts[i]              
        }}  
        fish.prop.sum <- colSums(fish.data.prop.wt)
        biom.prop.sum <- colSums(biom.data.prop.wt)  
      for (a in 1:n.areas) {
        ABC.EM[a] <- ABC.total * ((fish.prop.sum[a]+(2*biom.prop.sum[a]))/3)  #weighting 2x suvery:1x fishery happens here
      }
     return(ABC.EM)
    }
    
    #apport.opt = 5: Survey 5 year exponential weighting
    expsurvwt_apportionment <- function(ABC.total,n.areas,biom.data) { #note this uses numbers not biomass for survey
      ABC.EM <- vector(length=n.areas) #creating the output vector to hold apportioned ABCs
      for (y in 1:n.year){
        for (m in 1:n.areas){
        biom.data[y,m] <- sum(Surv.RPN[,y,,m])
      }}
      biom.data.prop <- matrix(data=NA, ncol=n.areas,nrow=5)
      biom.data.prop.wt <- matrix(data=NA, ncol=n.areas,nrow=5)
      biom.prop.sum <- vector(length=n.areas)
      wts <- c(0.0625, 0.0625, 0.125, 0.25, 0.5) #the weighting values
      for (i in (length(biom.data[,1])-4):length(biom.data[,1])) {
        for (a in 1:n.areas) {
          m <- i-(length(biom.data[,1])-5)
          biom.data.prop[m,a] <- biom.data[i,a]/sum(biom.data[i,]) #calc proportion by year across areas for survey data
      }}
      for (i in 1:length(biom.data.prop[,1])) {
        for (a in 1:n.areas) {    
          biom.data.prop.wt[i,a] <- biom.data.prop[i,a]*wts[i]    
      }}  
      biom.prop.sum <- colSums(biom.data.prop.wt)  
      for (a in 1:n.areas) {
        ABC.EM[a] <- ABC.total * biom.prop.sum[a]  
      }
      return(ABC.EM)
    }
    
    #apport.opt = 6: fishery 5 year exponential weighting
    expfishwt_apportionment <- function(ABC.total,n.areas,fish.data) { 
      ABC.EM <- vector(length=n.areas) #creating the output vector to hold apportioned ABCs
      for (y in 1:n.year){
        for (m in 1:n.areas){
          fish.data[y,m] <- sum(Fish.RPW[,y,,m])
      }}
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
      }}
      for (i in 1:length(fish.data.prop[,1])) {
        for (a in 1:n.areas) {    
          fish.data.prop.wt[i,a] <- fish.data.prop[i,a]*wts[i]
      }}  
      fish.prop.sum <- colSums(fish.data.prop.wt)
      for (a in 1:n.areas) {
        ABC.EM[a] <- ABC.total * fish.prop.sum[a]  
      }
      return(ABC.EM)
    }
    
    #apport.opt = 7: non-exponentially weighted 5 year moving average of survey and fishery, survey weight is 2x fishery still
    #(so, each year of the past has equal weight instead of exponential)
    #survey data is given 2x the weight of fishery data
    nonexp_apportionment <- function(ABC.total,n.areas,fish.data,biom.data) { 
      ABC.EM <- vector(length=n.areas) #creating the output vector to hold apportioned ABCs
      for (y in 1:n.year){
        for (m in 1:n.areas){
          fish.data[y,m] <- sum(Fish.RPW[,y,,m])
          biom.data[y,m] <- sum(Surv.RPN[,y,,m])
      }}
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
      }}
      for (i in 1:length(fish.data.prop[,1])) {
        for (a in 1:n.areas) {    
          fish.data.prop.wt[i,a] <- fish.data.prop[i,a]*wts[i]
          biom.data.prop.wt[i,a] <- biom.data.prop[i,a]*wts[i]    
      }}  
      fish.prop.sum <- colSums(fish.data.prop.wt)
      biom.prop.sum <- colSums(biom.data.prop.wt)  
      for (a in 1:n.areas) {
        ABC.EM[a] <- ABC.total * ((fish.prop.sum[a]+(2*biom.prop.sum[a]))/3)  #weighting 2x suvery:1x fishery happens here
      }
      return(ABC.EM)
    }

    #apport.opt = 8: partially fixed apportionment (BS and AI fixed at some value, rest use status quo; proportions 
    #for WG through EY/SEO are based on those areas only, excluding BS and AI)
    #(BS and AI fixed at some value, rest use status quo; proportions 
    #survey data is given 2x the weight of fishery data
    #area order should be BS - AI - WG - CG - EG (or if 6 areas, WY - EYSEO instead of EG)
    partfixed_apportionment <- function(ABC.total,n.areas,fish.data,biom.data, Fixed1, Fixed2) {
      #input two values for apportionment proportion for Bering Sea (value 1) and Aleutian Islands (value 2)
      ABC.EM <- vector(length=n.areas) #creating the output vector to hold apportioned ABCs
      fixed_values <- c(Fixed1,Fixed2)
      for (y in 1:n.year){
        for (m in 1:n.areas){
          fish.data[y,m] <- sum(Fish.RPW[,y,,m])
          biom.data[y,m] <- sum(Surv.RPN[,y,,m])
      }}
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
      }}
      for (i in (length(fish.data[,1])-4):length(fish.data[,1])) {  #fill in the fixed values for BS and AI
        for (a in 1:2) {
          m <- i-(length(fish.data[,1])-5)
          fish.data.prop[m,a] <- fixed_values[a]
          biom.data.prop[m,a] <- fixed_values[a]
      }}
      for (i in 1:length(fish.data.prop[,1])) {
        for (a in 1:n.areas) {    
          fish.data.prop.wt[i,a] <- fish.data.prop[i,a]*wts[i]
          biom.data.prop.wt[i,a] <- biom.data.prop[i,a]*wts[i]    
      }}  
      fish.prop.sum <- colSums(fish.data.prop.wt)
      biom.prop.sum <- colSums(biom.data.prop.wt)  
      for (a in 1:n.areas) {
        ABC.EM[a] <- ABC.total * ((fish.prop.sum[a]+(2*biom.prop.sum[a]))/3)  #weighting 2x suvery:1x fishery happens here
      }
      return(ABC.EM)
    }
    
    #apport.opt = 9: length/age based apportionment (they are relatively interchangeable here)
    #calculate the mean (or median?) length/age at 50% female maturity - then calculate the proportion of females of that 
    #length/age in each of the n.areas based on the LL survey data most recent year data
    #L50_mat #input row number for length/age at 50% maturation or greater (data as aggregate across all Alaska areas; 
    #made up value for now 65 (L)/6(A) and over)
    agebased_apportionment <- function(ABC.total,n.areas,LLlencomp,L50_mat) { 
      ABC.EM <- vector(length=n.areas) #creating the output vector to hold apportioned ABCs
      natage.prop1 <- vector(length=n.areas) #creating output vector for prop at L50 (or larger)
      natage.prop2 <- vector(length=n.areas) #creating output vector apportionment based on natage.prop1
      for (a in 1:n.age){
        for (m in 1:n.areas){
          LLlencomp[a,m] <- sum(N[,a,m])
        }}
      for (a in 1:n.areas){
        natage.prop1[a] <- sum(LLlencomp[L50_mat:length(LLlencomp[,a]),a])/sum(LLlencomp[,a])
      }
      for (a in 1:n.areas) {
        natage.prop2[a] <- natage.prop1[a]/sum(natage.prop1)
      }
      return(natage.prop2)
      for (a in 1:n.areas) {
        ABC.EM[a] <- ABC.total * natage.prop2 
      }
    }
    
    #apport.opt = 10: Random effects model apportionment

    #apport.opt = 11: all to one area
    all2one_apportionment <- function(ABC.total,n.areas,luckyarea) {
      ABC.EM <- vector(length=n.areas)
      fixed_prop <- vector(length=n.areas)
      fixed_prop <- rep(0,times=n.areas)
      fixed_prop[luckyarea] <- 1
      for (a in 1:n.areas) {
        ABC.EM[a]<-ABC.total*fixed_prop[a]
      }
      return(ABC.EM)
    }
    #apport.opt = 12: terminal year survey biomass proportions
    #skip adding this one for now, since it's not likely to be a good candidate
    
    #apport.opt = 13: penalized apportionment (individual area can't change by more than x% per year)
    #skipping this for now    
    
    #apport.opt = 14: non exponential (5-year average) length-based
    #skip for now    
    
#try(    
area <- 1
i <- 2
for(i in 1:n.sims) {
  print(paste('Sim:',i,'of',n.sims))
  
  # Create Sim-specific Directory
  dir.temp <- file.path(dir.admb, i)
  dir.create(dir.temp)
  
  # Copy Necessary Components into sim-specific directory
  copy_admb_sim(dir.from=dir.admb, dir.to=dir.temp)
  
  y <- 44  
  for(y in 44:n.year) { #n.year
    print(paste('Year:',y,'of',n.year))
    # somewhere up here, set a bunch of seeds...
    #for the first year (2019, year 44) of forward projections, specify the catch (TAC) apportioned to each area above and use it for year 44
    #for subsequent years, use the EM output for ABC, which is generated at the end of this code chunk
      for(m in 1:n.area) {
        for(f in 1:n.fish) {
        temp.Fmort <- estimate_Fmort4catch(catch=0.5*apportioned_C[y-1,f,m,i],
                      temp.selex=va[f,m,,],
                      temp.N=N[,y-1,,m,i], 
                      wa=wa, mx=mx, 
                      bisection=FALSE)$Fmort
        F.mort[f,y-1,m,i] <- temp.Fmort 
        } #close fish
      } #close area

    m <- 1
    for (m in 1:n.area) {
    a <- 1
    for(a in 1:n.age) {
      #Update Numbers and Biomass Matrix
      if(a==1) { #Age-1
        N[,y,a,m,i] <- 0.5*recruits.area[y,m,i] #multiplying by 0.5 to split evenly between sexes

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
        }#next sex
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
        }#next sex
      }# If plus age group
      
    }#next age  
    } #next area
    
    
    #move all the ages between areas, and then calculate the derived quantities after movement:
    for (h in 1:n.sex){ 
      for (a in 1:n.age){
        for (m in 1:n.area){
          N_hold[h,y,m,a,i]<-N[h,y,a,m,i]  
        }  
        N_hold[h,y,,a,i]<-t(prob.move[,,a])%*%N_hold[h,y,,a,i]
        for (m in 1:n.area){
          N[h,y,a,m,i]<-N_hold[h,y,m,a,i]
        } #close area
      } #close age
    } #close sex
    
    #calculate all the derived quantities at once, after movement so that all N, B, ssb, C, and harvest reflect the moved population values
    for (a in 1:n.age){
      for (m in 1:n.area){
        B[,y,a,m,i] <- N[,y,a,m,i] * wa[,a]
        ssb[,a,y,m,i] <- ma[,a]*wa[,a]*N[,y,a,m,i]
        for (h in 1:n.sex){ 
          C.n[h,y-1,a-1,m,i] <- N[h,y-1,a-1,m,i] * (F.a[h,y-1,a-1,m,i]/Z.a[h,y-1,a-1,m,i]) * (1-exp(-1*Z.a[h,y-1,a-1,m,i])) #Catch in number 
          C.b[h,y-1,a-1,m,i] <- C.n[h,y-1,a-1,m,i] * wa[h,a-1]
          for(f in 1:n.fish) {
            temp.F <- F.mort[f,y-1,m,i]*va[f,m,h,a-1]
            temp.Z <- sum(F.mort[,y-1,m,i]*va[,m,h,a-1]) + mx[h,a-1]
            harvest.n[h,y-1,a-1,f,m,i] <- N[h,y-1,a-1,m,i] * (temp.F/temp.Z) * (1-exp(-1*temp.Z)) #this is fleet specific catch, catch is fleets combined
            harvest.b[h,y-1,a-1,f,m,i] <- harvest.n[h,y-1,a-1,f,m,i] * wa[h,a-1]
          }#next gear
        } #close sex
      }#close area
    }#close age 
    # plus group age calcs for catch
    for (m in 1:n.area){
      for (h in 1:n.sex){ 
        C.n[h,y-1,30,m,i] <- N[h,y-1,30,m,i] * (F.a[h,y-1,30,m,i]/Z.a[h,y-1,30,m,i]) * (1-exp(-1*Z.a[h,y-1,30,m,i])) #Catch in number 
        C.b[h,y-1,30,m,i] <- C.n[h,y-1,30,m,i] * wa[h,30]
        for(f in 1:n.fish) {
          temp.F <- F.mort[f,y-1,m,i]*va[f,m,h,a]
          temp.Z <- sum(F.mort[,y-1,m,i]*va[,m,h,a]) + mx[h,a]
          harvest.n[h,y-1,a,f,m,i] <- N[h,y-1,a,m,i] * (temp.F/temp.Z) * (1-exp(-1*temp.Z))
          harvest.b[h,y-1,a,f,m,i] <- harvest.n[h,y-1,a,f,m,i] * wa[h,a]
        }#next gear
      } #close sex
    }#close area
    
    
    ######Sample population for age comps, survey index, etc. 
    ##### Generate EM Data: ######
    m <- 1
    for (m in 1:n.area) {
        #write a function that makes it easy to specify (by area) the yield ratio
        # longline survey RPN, 'current' year  -- check units
      #Surv.RPN[,y,,m,i] <- sample_biom_abund(N[,y,,m,i]*selex$surv$USLongline[h,],sigma=LLsurvRPNsigma, type='lognorm', seed=c(y+i))         #(we'd talked about concatonating 'sim # + year' for seed)
      Surv.RPN[,y,,m,i] <- sample_biom_abund(N[,y,,m,i]*q_surv[1,m]*selex$surv$USLongline,sigma=LLsurvRPNsigma, type='lognorm', seed=c(y+i+546))         #(we'd talked about concatonating 'sim # + year' for seed)
                    # longline/fixed gear fishery CPUE/RPW  -- check units
      Fish.RPW[,y-1,,m,i] <- sample_biom_abund(B[,y-1,,m,i]*q_fish[1,m]*selex$fish$USfixed_postIFQ[m,,], sigma=LLfishRPWsigma, type='lognorm', seed=c(y+i+14)) #14 is just a randomly chosen # to make seed diff from above

      for(h in 1:n.sex){
      # longline/fixed gear post-IFQ fishery age comps in numbers , can use LLfishAC_sampsize (specified above) if you want to match the comp draws here with the dat file maker comp sizes, or 
        #specify another comp sample size here
      sample_age_comps(harvest.n[h,y-1,,2,m,i]*selex$fish$USfixed_postIFQ[m,h,], Nsamp=LLfishAC_sampsize, cpar=NULL, seed=c(y+i+134)) #does this need to include selectivity?
      Fish.AC[h,y-1,,m,i] <- obs.comp
      
      # longline survey age comps in numbers (not proportions yet) single sex, can use LLfishAC_sampsize (specified above) if you want to match the comp draws here with the dat file maker comp sizes, or 
      #specify another comp sample size here
      sample_age_comps(N[h,y-1,,m,i]*selex$surv$USLongline[h,], Nsamp=LLsurvAC_sampsize, cpar=NULL, seed=c(y+i+135)) #true.props, Nsamp, cpar
      Surv.AC[h,y-1,,m,i] <- obs.comp
      } #close sex
    } #next area m
    
    ######### aggregate OM data across age, sex and/or areas for EM and track over time (these could be moved into the data file builder function)
    #sum catch at age to a single catch (actually, probably harvest) for year y, summed over areas and sexes, for each gear
        #fish 1 - US fixed gear pre-IFQ, 2 - US fixed post-IFQ, 3 - US trawl, 4 - foreign fixed gear
        OM_fixed_catch[y-1,i] <- sum(harvest.b[,y-1,,2,,i]) #2 - US fixed post-IFQ
        OM_trawl_catch[y-1,i] <- sum(harvest.b[,y-1,,3,,i]) #3 - US trawl
        #sum RPN to one value for the year and sim (sum over age, sex area) - do these need to be weighted in some way?
        OM_Surv.RPN[y,i] <- sum(Surv.RPN[,y,,,i])#this could be moved into the data file builder function
        OM_Fish.RPW[y-1,i] <- sum(Fish.RPW[,y-1,,,i])
        #age comps have a year lag
        OM_Surv.RPN.age[y-1,,i] <- aggr_agecomp(Surv.AC[,y-1,,,i], Surv.RPN[,y-1,,,i],2) #aggregate comps by sex and area, weight by survey catch in area
        OM_Fish.RPW.age[y-1,,i] <- aggr_agecomp(Fish.AC[,y-1,,,i], C.n[,y-1,,,i],2) #aggregate comps by sex and area, weight by catch in area
      
       
    ## Build the data: read in a .dat file, advance #years, year counts, add data generated in current year to matrices/arrays  
    ## then generate the updated .dat file to be pushed to the EM
      build_datfile(LLsurvAC_sampsize,LLfishAC_sampsize)  
        
    #Dana think about the code in the EM here between gear types for distibuting F across selectivities
    #calculate the moving average ratio between gear types (F ratio)   
        
    #=============================================================
    #### Conduct Assessment #### 

    time.elapsed<-run.model()

    #=============================================================
    # extract ABC (read in a report file)
    # this will come out of my EM  CHECK IF THERE ARE CODE CHANGES IN THE EM ABOUT F RATIO
    get_ABC <- readList(file.path(dir.temp,"tem.rep"))

    #=============================================================
    #### Set Harvest Limits & apply apportionment method we are testing ####
    # call the apportionment method here
    if (y<n.year){
    if (apport.opt==1) {
      ABC_TS[y+1,,i] <- equal_apportionment(get_ABC$ABC_proj[1],n.area) 
    }
      
    if (apport.opt==2) {
      ABC_TS[y+1,,i] <- fixed_apportionment(get_ABC$ABC_proj[1],n.area,current.props)
    }
      
    if (apport.opt==3) {
      ABC_TS[y+1,,i] <- equilib_apportionment(get_ABC$ABC_proj[1],n.area,equilib.props)
    } 

    if (apport.opt==4) {
      ABC_TS[y+1,,i] <- NPFMC_apportionment(get_ABC$ABC_proj[1],n.area,Fish.RPW[,,,,i],Surv.RPN[,,,,i])
    } 

    if (apport.opt==5) {
      ABC_TS[y+1,,i] <- expsurvwt_apportionment(get_ABC$ABC_proj[1],n.area,Surv.RPN[,,,,i]) #verify this is the right biom
    }
   
    if (apport.opt==6) {
      ABC_TS[y+1,,i] <- expfishwt_apportionment(get_ABC$ABC_proj[1],n.area,Fish.RPW[,,,,i]) #verify this is the right biom
    }      
  
    if (apport.opt==7) {
      ABC_TS[y+1,,i] <- nonexp_apportionment(get_ABC$ABC_proj[1],n.area,Fish.RPW[,,,,i],Surv.RPN[,,,,i]) #verify this is the right biom
    }      

    if (apport.opt==8) {
      ABC_TS[y+1,,i] <- partfixed_apportionment(get_ABC$ABC_proj[1],n.area,Fish.RPW[,,,,i],Surv.RPN[,,,,i],FixedA1,FixedA2) #verify this is the right biom
    } 

    if (apport.opt==9) {
      ABC_TS[y+1,,i] <- agebased_apportionment(get_ABC$ABC_proj[1],n.area,N[,y,,,i],A_L.mat) #L50_mat is the row corresponding to the age/length at 50% maturity
    } 

    #if (apport.opt==10) { #this needs editing
     #ABC_TS[y,,i] <- random.effect(get_ABC$ABC_proj[1],n.area,lucky.area) #L50_mat is the row corresponding to the age/length at 50% maturity
    #} 
      
    if (apport.opt==11) {
      ABC_TS[y+1,,i] <- all2one_apportionment(get_ABC$ABC_proj[1],n.area,lucky.area) #L50_mat is the row corresponding to the age/length at 50% maturity
    } 
    
    #then fill in apportionment and TAC/catch for the next year so we can start the loop all over again  
    for (m in 1:n.area){ #first make it all zero
      for (f in 1:n.fish){
      apportioned_C[y+1,f,m,i] <- 0
      }
    }
    apportioned_C[y+1,2,1,i] <- ABC_TS[y+1,1,i] #fill in ABC to area 1, post-ifq gear
    apportioned_C[y+1,2,2,i] <- ABC_TS[y+1,2,i] #fill in ABC to area 2, post-ifq gear
    apportioned_C[y+1,2,3,i] <- ABC_TS[y+1,3,i] #fill in ABC to area 3, post-ifq gear
    apportioned_C[y+1,2,4,i] <- ABC_TS[y+1,4,i] #fill in ABC to area 4, post-ifq gear
    apportioned_C[y+1,2,5,i] <- ABC_TS[y+1,5,i]*0.95 #post IFQ fishery, giving 95% of ABC to fixed gear
    apportioned_C[y+1,3,5,i] <- ABC_TS[y+1,5,i]*0.05 #trawl fishery, giving 5% of ABC to trawl
    apportioned_C[y+1,2,6,i] <- ABC_TS[y+1,6,i]*0.95 #post IFQ fishery, giving 95% of ABC to fixed gear
    apportioned_C[y+1,3,6,i] <- ABC_TS[y+1,6,i]*0.05 #trawl fishery, giving 5% of ABC to trawl
   
    apportioned_C[is.na(apportioned_C)] <- 0 
    }
    # Accumulate things we want to track from the .rep file(s) and for use in the performance_metrics.R function
    #max_grads <- get_ABC$#(how to get this?)
    #obj_fun_vals <- get_ABC$obj_fun
    #spr_penalty <- get_ABC$SPRpen
    #data_likelihood <- get_ABC$datalikelihood

 
    
    
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


#call the function to look at performance metrics here.

