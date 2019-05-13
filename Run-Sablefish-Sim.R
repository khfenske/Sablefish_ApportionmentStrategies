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
create_sim_recruitments(mu_rec=mu_rec, sigma_rec=sigma_rec, rho_rec=NULL, 
                        n.year=n.year, n.sims=n.sims, seed=101) #Creates rec object

# divide annual recruitments into areas 
for(i in 1:n.sims){
for(y in 1:n.year){
  recruits.area[y,,i] <- spatial_rec(rec[i,y],area.props=c(0.14,0.07,0.14,0.43,0.14,0.09), ss=100, seed=1)      # this needs thought about sex distribution of recruitment.
}}


# Initialize Population (year 1, or change to years 1-X) =============================================================
#   Should probably update to start from FISHED equilibrium (and spatially mixed equilibrium??) or set to stable 
#   distribution of movement proportions...
#   this sets up the population proportions for year 1 for each of the N number of simulations.
#   I think we need to add code here, starting with init.prop, to run forward ~50-60 years to make a fished equilib...
#   and build our initial .dat file (summed across n.areas), us F generated from actual observed catches
init.prop <- calc_init_age_prop(bo=mu_rec)

i <- 1
m <- 1
for(i in 1:n.sims) {
  for(m in 1:n.area) {
  B[,1,,m,i] <- Bstart*1e6 * (init.prop[,,m]) #dim=c(n.sex, n.year, n.age, n.area, n.sims) (for both B and N)
  N[,1,,m,i] <- B[,1,,m,i] / wa
  } #close area
}#next i


#Loop order: sim, year, area, age, sex.
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
    # Temporarily Assuming no movement, need to add that in in the future
      
    #take most recent assessment and outputted ABC apportionment and calculate F 
    #apply F from the most recent "REAL" stock assessment (spatial model) to OM population (assume perfect execution)- update B, N, SSB
    # need to get an F to use for below
    f <- 1
    for(f in 1:n.fish) {
      # UPDATE: Instead of setting a fixed F, we can now set a fixed catch
      #  biomass and find the F for the specific fishery
      #   that will match that catch
      
      # Here lets specify an arbitrary fixed catch (kg) -- will need to eventually read it in for the first year here (based on the most recent assessment 
      #and harvest control rule output)...future year's catch will come from the EM/projectiom model and apportionment application, so this section still needs work
      catch <- 1e6 # 1 million kg.
      
      # Find Fishing Mortality Rate for Apportioned Catch Level ------------------------  #right now F is a single value - probably need to have spatial F?
      temp.Fmort <- estimate_Fmort4catch(catch=catch, 
                                           temp.selex=va[f,area,,],
                                           temp.N=N[,y-1,,m,i], 
                                           wa=wa, mx=mx, 
                                           bisection=TRUE)$Fmort
      Fmort[f,y,m,i] <- temp.Fmort  #0.1#HCR_linear(curr.SSB=temp.ssb, SSB0=SSB0, floor.F=floors[g], ceiling.F=ceilings[g], 
                                 # ascent.range=ascent.range, plot=FALSE)
    }#next g 
        
    a <- 1
    for(a in 1:n.age) {
      #Update Numbers and Biomass Matrix
      if(a==1) { #Age-1
        N[,y,a,m,i] <- recruits.area[y-1,m,i]
        B[,y,a,m,i] <- recruits.area[y-1,m,i]*wa[,a]
        # N[,y,a] <- rec[,y-1]/wa[,a]
        # B[,y,a] <- rec[,y-1]
        
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
          N[h,y,a,m,i] <- N[h,y-1,a-1,m,i]*surv[h,y-1,a-1,m,i] #add movement here?
          # B[h,y,a,i] <- B[h,y-1,a-1,i]*surv[h,y-1,a-1,i]
          B[h,y,a,m,i] <- N[h,y,a,m,i]*wa[h,a]
          #Total Catch
          C.n[h,y-1,a-1,m,i] <- N[h,y-1,a-1,m,i] * (F.a[h,y-1,a-1,m,i]/Z.a[h,y-1,a-1,m,i]) * (1-exp(-1*Z.a[h,y-1,a-1,m,i])) #Catch in number of halibut
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
    
    #calculate SSB for OM pop  #maybe the year itentifier is wrong?
    ssb[,,y-1,m,i] <- ma*wa*N[,y-1,,m,i] #ssb dims = n.sex, n.age, n.year, n.area, n.sims; N dims = n.sex, n.year, n.age, n.area, n.sims
    
    } #next area
    
    ######Sample population for age comps, survey index, etc. 
    #m <- 1 #maybe don't need all these loops
    #for (m in 1:n.area) {
      #a <- 1
      #for(a in 1:n.age) {
        #h <- 1
        #for(h in 1:n.sex) {
        ##### Generate Assessment Data: ######
        # observed catch (based on what for F?), 'current' year, for 6 areas then combine to 3 and to 1 
          #write a function that makes it easy to specify (by area) the yield ratio
      
        # longline survey RPN, 'current' year, for 6 areas then combine to 1
        Surv.RPN[,y,,m,i] <- sample_biom_abund(atest,sigma=0.2, type='lognorm', seed=12345) #need to create a more sophisticated seed higher in the code
        #(we'd talked about concatonating 'sim # + year' for seed)
      
        # longline/fixed gear fishery CPUE/RPW, lagged 1 year, for 6 areas then combine to 1
        Fish.RPW[,y,,m,i] <- sample_biom_abund(B[,y,,m,i], sigma=0.4, type='lognorm', seed=333)
        
        # longline/fixed gear fishery age comps, lagged 1 year, for 6 areas then combine to 1, single sex
        Fish.AC[,y,,m,i] <- sample_age_comps() #true.props, Nsamp, cpar
        
        # longline survey age comps, lagged 1 year, for 6 areas then combine to 3, single sex
        Surv.AC[,y,,m,i] <- sample_age_comps() #true.props, Nsamp, cpar
        
        #### NOTE - not doing length comps for now ####
        ## longline/fixed gear fishery length comps, lagged 1 year, for 6 areas then combine to 3, two sexes
        ## trawl gear fishery length comps, lagged 1 year, for 6 areas then combine to 3, two sexes
        # #longline survey length comps, 'current' year, for 6 areas then combine to 3, two sexes
      
        #} #next sex
      #} #next age
    #} #next area
    
    #Add sampled data to .dat file (generate/update .dat file)
    
    #Pass to EM, run EM
    #Get estimated quantities and HCR output 
    #apportion output (via chosen method) 
    #apply apportionment output to OM (start loop over)
    

    #HARVEST CONTROL RULE [this section may not need to be retained here]
    #temp.ssb <- sum(ssb[,,y-1,m,i]) #presumably this needs an actual HCR coded in??  Also, why is this here and not somewhere else?
    # No Recruitment relationship  Can we change this so it reads in a rec value from a separate file which draws N simulations * N years worth of rec values all 
    # at once so the same recruitment can be applied to single and spatial models? Could make it so that if SSB is 0, 0 rec is used instead so we 
    # don't spontaneously generate fish if the pop crashes.
    #rec[,y-1,m,i] <- 0.5 * exp(mu_rec*(1+mx[,1]) + rnorm(1, 0, sigma_rec)) #Note this has the addition to account for assessment model predicting recruitment to age 2. 
        #Ricker
    # rec[,y-1] <- 0.5 * ricker_recruit(ssb[y-1], steep, bo)
    #Beverton-Holt
    # rec[,y-1,i] <-  0.5 * beverton_holt_recruit(sum(ssb[,,y-1,i]), steep, bo=ro) * exp(rnorm(1,0,sigma_rec) - ((sigma_rec^2)/2))
    

    

    
    #=============================================================
    #### Conduct Assessment #### 
    #2) Call ADMB Model
    #add code here
    #=============================================================
    #### Determine SPR ####
    # doesn't this come out of my EM?
    #=============================================================
    #### Set Harvest Limits ####
    

    

    #=============================================================
    #### Conduct Assessment #### 
    #2) Call ADMB Model
    #add code here
    #=============================================================
    #### Determine SPR ####
    # doesn't this come out of my EM?
    #=============================================================
    #### Set Harvest Limits ####    
    
    
    
  }#next y
  
}#next i





