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
  recruits.area[y,,i] <- spatial_rec(rec[i,y],area.props=c(1,1,1,1,1,1), ss=100, seed=1)      # this needs thought about sex distribution of recruitment.
}}


# Initialize Population (year 1, or change to years 1-X) =============================================================
#   Should probably update to start from FISHED equilibrium (and spatially mixed equilibrium??) or set to stable distribution of movement proportions...
#   HOW DO WE DO THAT? WOULD WE NEED A SEPARATE FUNCTION TO COME UP WITH EQUILIB THEN READ IT IN HERE BY AREA?
#   this sets up the population proportions for year 1 for each of the N number of simulations.
#   I think we need to add code here, starting with init.prop, to run forward ~50-60 years to make a fished equilib...
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
    
    
    
    m <- 1
    for (m in 1:n.area) {
    #Initial Recruitment (<- is this a bad section name? I don't understand why it's called this)
    ssb[,,y-1,m,i] <- ma*wa*N[,y-1,,m,i] #ssb dims = n.sex, n.age, n.year, n.area, n.sims; N dims = n.sex, n.year, n.age, n.area, n.sims

    #HARVEST CONTROL RULE
    temp.ssb <- sum(ssb[,,y-1,m,i]) #presumably this needs an actual HCR coded in??  Also, why is this here and not somewhere else?
    
    # need to get an F to use for below
    
    f <- 1
    for(f in 1:n.fish) {
      # UPDATE: Instead of setting a fixed F, we can now set a fixed catch
      #  biomass and find the F for the specific fishery
      #   that will match that catch
      
      # Here lets specify an arbitrary fixed catch (kg)
      catch <- 1e6 # 1 million kg.
      
      # Find Fishing Mortality Rate for Apportioned Catch Level ------------------------
      temp.Fmort <- estimate_Fmort4catch(catch=catch, 
                                           temp.selex=va[f,area,,],
                                           temp.N=N[,y-1,,m,i], 
                                           wa=wa, mx=mx, 
                                           bisection=TRUE)$Fmort
      
      
      
      Fmort[f,y,m,i] <- 0.1#HCR_linear(curr.SSB=temp.ssb, SSB0=SSB0, floor.F=floors[g], ceiling.F=ceilings[g], 
                                 # ascent.range=ascent.range, plot=FALSE)
    }#next g (g for gear?)
    
    #No Recruitment relationship  Can we change this so it reads in a rec value from a separate file which draws N simulations * N years worth of rec values all 
    # at once so the same recruitment can be applied to single and spatial models? Could make it so that if SSB is 0, 0 rec is used instead so we 
    # don't spontaneously generate fish if the pop crashes.
    #rec[,y-1,m,i] <- 0.5 * exp(mu_rec*(1+mx[,1]) + rnorm(1, 0, sigma_rec)) #Note this has the addition to account for assessment model predicting recruitment to age 2. 
    
    #Ricker
    # rec[,y-1] <- 0.5 * ricker_recruit(ssb[y-1], steep, bo)
    #Beverton-Holt
    # rec[,y-1,i] <-  0.5 * beverton_holt_recruit(sum(ssb[,,y-1,i]), steep, bo=ro) * exp(rnorm(1,0,sigma_rec) - ((sigma_rec^2)/2))
    
    #1) Generate Assessment Data:
    # insert that code here for: where do these go?  how to sample for these and add error?
    # observed catch (based on what for F?), 'current' year, for 6 areas then combine to 3 and to 1 (or how do we want to do this so it reads into a single area and a spatial model?)
    # longline survey RPN, 'current' year, for 6 areas then combine to 3
    # longline/fixed gear fishery CPUE/RPW, lagged 1 year, for 6 areas then combine to 3
    # longline/fixed gear fishery age comps, lagged 1 year, for 6 areas then combine to 3, single sex
    # longline survey age comps, lagged 1 year, for 6 areas then combine to 3, single sex
    # longline/fixed gear fishery length comps, lagged 1 year, for 6 areas then combine to 3, two sexes
    # trawl gear fishery length comps, lagged 1 year, for 6 areas then combine to 3, two sexes
    # longline survey length comps, 'current' year, for 6 areas then combine to 3, two sexes
    
    # Forward Simulation =============================================================
    #Temporarily Assuming no movement, need to add that in WHERE AND WHEN WILL FISH MOVE????
    
    #=============================================================
    #### Conduct Assessment #### <- this seems like the wrong place for this??
    #2) Call ADMB Model
    #add code here
    #=============================================================
    #### Determine SPR ####
    # doesn't this come out of my EM?
    #=============================================================
    #### Set Harvest Limits ####
    
    a <- 1
    for(a in 1:n.age) {
      #Update Numbers and Biomass Matrix
      if(a==1) { #Age-1
        N[,y,a,m,i] <- recruits.area[y-1,m,i]
        B[,y,a,m,i] <- recruits.area[y-1,m,i]*wa[,a]
        # N[,y,a] <- rec[,y-1]/wa[,a]
        # B[,y,a] <- rec[,y-1]
        
        ## add movement for age 1 here? ##
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
      }# If plus age group
    }#next age  
    
    } #next area
    
    #call a function that generates a .dat file here?
    
    #=============================================================
    #### Conduct Assessment #### <- this seems like the wrong place for this??
    #2) Call ADMB Model
    #add code here
    #=============================================================
    #### Determine SPR ####
    # doesn't this come out of my EM?
    #=============================================================
    #### Set Harvest Limits ####    
    
    
    
  }#next y
  
}#next i


# need to print out or save a copy of the simulated data somehow



