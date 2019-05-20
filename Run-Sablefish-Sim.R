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
source(file.path(dir.R,'calc-selectivity.R'))
source(file.path(dir.R,'calc-init-age-prop.R'))

source(file.path(dir.R,'create-sim-objects.R'))
source(file.path(dir.R,'create-sim-recruitments.R')) #Simulate Recruitment across years and sims

source(file.path(dir.R,'spatial-rec.R')) #Apportion Recruitment Among Regions

source(file.path(dir.R,'convert-Fmort2catch.R')) #Calculate total catch from F
source(file.path(dir.R,'estimate-Fmort4catch.R')) #Estimate F that provides for a given catch

source(file.path(dir.R,'sample-biom-abund.R')) #sample biomass/numbers of OM population
source(file.path(dir.R,'sample-age-comps.R')) #sample age comps of OM population


        
# Extract Parameters =============================================
extract_pars(input.file="Sablefish_Input.xlsx")

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

# Simulate Annual Recruitments ====================================
#setup_years <- 39 #number of years to run the loop setting up the initial population and building initial dat file
create_sim_recruitments(mu_rec=mu_rec, sigma_rec=sigma_rec, rho_rec=NULL, 
                        n.year=n.year, n.sims=n.sims, seed=101) #Creates rec object DOES THE rec object need to be set up in the 'create-sim-objects.R' function?

# divide annual recruitments into areas 
for(i in 1:n.sims){
for(y in 1:n.year){
  recruits.area[y,,i] <- spatial_rec(rec[i,y],area.props=c(0.14,0.07,0.14,0.43,0.14,0.09), ss=100, seed=1)      # sexes combined, recruitment in Numbers (I think)
}}


# Initialize Population (year 1, or change to years 1-X) =============================================================
#   Should probably update to start from FISHED equilibrium (and spatially mixed equilibrium??) or set to stable 
#   distribution of movement proportions...
#   this sets up the population proportions for year 1 for each of the N number of simulations.
#   I think we need to add code here, starting with init.prop, to run forward ~50-60 years to make a fished equilib...
#   and build our initial .dat file (summed across n.areas), us F generated from actual observed catches
#  might need to call the a dat maker function here to read in our .dat template and then fill it with the data we generate here to initialize the population

#init.prop <- calc_init_age_prop(bo=mu_rec)

i <- 1
m <- 1
for(i in 1:n.sims) {
  for(m in 1:n.area) {
  B[,1,,m,i] <- Bstart*1e6 * (init.prop[,,m]) #dim=c(n.sex, n.year, n.age, n.area, n.sims) (for both B and N)
  N[,1,,m,i] <- B[,1,,m,i] / wa
  } #close area
}#next i

#set up initial population and dat file
calc_init_pop(init.prop,)


#Loop order: sim, year, area, age, sex.
# Order - calculate F associated with catch permitted from previous year's assessment and apportionment, recruit new cohort, F and M occur, movement occurs, sample the 
#(moved) OM population, add OM data to .dat file, run EM, apply apportionment [end of single 'year cycle']
# ...then start over at beginning with a new year
area <- 1
i <- 1
for(i in 1:n.sims) {
  print(paste('Sim:',i,'of',n.sims))
  
  y <- 2  
  for(y in 2:n.year) {
    # somewhere up here, set a bunch of seeds...
    
    m <- 1
    for (m in 1:n.area) {
     
    # Forward Simulation =============================================================
    # Temporarily no movement, need to add that in in the future
      
    #take most recent (2018) assessment and outputted ABC apportionment and calculate F 
    f <- 1
    for(f in 1:n.fish) {
      # UPDATE: Instead of setting a fixed F, we can now set a fixed catch
      #  biomass and find the F for the specific fishery
      #  that will match that catch. This is so we can input the catch from the EM projection and apportionment output and then calculate the associated F.
      # need to think about how we deal with trawl vs longline F here - do we specify these catches currently?
      # Here lets specify an arbitrary fixed catch (kg) -- will need to eventually read it in for the first year here (based on the most recent assessment 
      #and harvest control rule output)...future year's catch will come from the EM/projectiom model and apportionment application, so this section still needs work
      catch <- 1e7 # 10 million kg or 10,000 metric tons
      
      # Find Fishing Mortality Rate for Apportioned Catch Level ------------------------  #right now F is a single value - probably need to have spatial F?
      temp.Fmort <- estimate_Fmort4catch(catch=catch, 
                                           temp.selex=va[f,area,,],
                                           temp.N=N[,y-1,,m,i], 
                                           wa=wa, mx=mx, 
                                           bisection=TRUE)$Fmort
      Fmort[f,y,m,i] <- temp.Fmort  #0.1
    }#next g 
        
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
          F.a[h,y-1,a-1,m,i] <- sum(Fmort[,y,m,i]*va[,m,h,a-1]) #va dim = n.fish,n.area,n.sex,n.age
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
            temp.F <- Fmort[f,y,m,i]*va[f,m,h,a-1]
            # temp.Z <- temp.F + mx[h,a-1]
            temp.Z <- sum(Fmort[,y,m,i]*va[,m,h,a-1]) + mx[h,a-1]
            
            harvest.n[h,y-1,a-1,f,m,i] <- N[h,y-1,a-1,m,i] * (temp.F/temp.Z) * (1-exp(-1*temp.Z)) 
            
            harvest.b[h,y-1,a-1,f,m,i] <- harvest.n[h,y-1,a-1,f,m,i] * wa[h,a-1]
          }#next gear
        }#next sex
        ## add movement for ages 
      }
      
      if(a==A) {
        h <- 1
        for(h in 1:n.sex) {
          #Fish in Plus Group
          F.a[h,y-1,a,m,i] <- sum(Fmort[,y,m,i]*va[,m,h,a])
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
            temp.F <- Fmort[g,y,m,i]*va[f,m,h,a]
            # temp.Z <- temp.F + mx[h,a]
            temp.Z <- sum(Fmort[,y,m,i]*va[,m,h,a]) + mx[h,a]
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
        
        # longline/fixed gear fishery age comps in numbers (not proportions yet) 
        #Fish.AC[y,,m,i] <- sample_age_comps() #true.props, Nsamp, cpar
        
        # longline survey age comps in numbers (not proportions yet) single sex
        #Surv.AC[y,,m,i] <- sample_age_comps() #true.props, Nsamp, cpar

    } #next area m
    
    ######### aggregate OM data across age and/or areas for EM and track over time
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
        
        
    #=============================================================
    #### Conduct Assessment #### 
    #2) Call ADMB Model
    #add code here
    #=============================================================
    #### Determine SPR ####
    # extract SPR (read in a report file)
    # this will come out of my EM
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





