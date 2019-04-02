#==================================================================================================
#Project Name: SABLEFISH APPORTIONMENT - Run Simple Simulation
#Creator: Curry James Cunningham, NOAA/NMFS, ABL
#Modified by Kari Fenske
#Date: 3.10.18
#
#Purpose: Read in Parameters and Conduct Simple Simulation
#
#==================================================================================================
#NOTES:
#  a) I have put in commented placeholders for funtions that need to be 
#     functions/code that still needs to be developed or added
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
source(file.path(dir.R,'create-sim-objects.R'))
source(file.path(dir.R,'calc-init-age-prop.R'))

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
selex$fish$Foreign <- calc_selectivity(type='fish', fleet='Foreign')  #might not need these in here since it's only in the past...

#Calculate Vulnerability @ Age ====================================
va <- array(dim=c(n.fish,n.area,n.sex,n.age), dimnames=list(fish,1:n.area,sexes,ages))
i <- 1
for(i in 1:n.fish) {
  va[i,,,] <- calc_selectivity(type='fish', fleet=fish[i])
}

#Rec Dev creation ================================================
#Calculate a matrix of number of Sims by number of years rec devs
  #per discussion, Curry will work on this code, we will use the 
  #mean recruitment from the 2018 single area model
  #call function here

# Create Simulation Objects =======================================
#NOTE: Currently calculates data for n areas, where n is defined in the input spreadsheet (n.areas)  
create_sim_objects() #sets up all the spatial arrays to hold simulated data

#Specify movement type ===========================================
move.type <- 1 #type 1 is single movement matrix
if(move.type!=1) {stop("#### You screwed up move.type must equal 1")}


#==================================================================
# Initialize Population ===========================================
#build a 'SIMlandia' population using fishery and survey selectivity
#from the 1977 proportions at age for all areas, break into spatial
#areas using 1977 survey biomass by area. Then fish and M and recruit
#and move them. End product will be a 1979-2018 set of N at age data.
#then need to 'sample' this population for survey and fishery indices, 
#age comps and use that to construct the early .dat file years. 
#take this .dat file and run it through the single area EM to see
#now it compares, and also compare the 'sim' and samples to single
#area EM output to make sure things match (this is only to make sure
#we are setting things up right, it won't be done in our simulation loops).
#2018 will be 'fished' (and sampled?) using Dana's adjusted F max
#Once the initial pop and .dat files look good, we run the 
#Kari working on this function...
# currently, it is NOT WORKING

init_population <- calc_init_pop(init_year_N, init_pop_rec, init_rec_proportion, selex$fish$USfixed_postIFQ, selex$fish$USTrawl, wa)

#sample the init population and create the .dat file history for 1979-2018 using the functions for sampling...


# this code/function (below) will change, but leaving it in for now because it runs
#and creates fish so we can still test other things.
init.prop <- calc_init_age_prop(bo=mu_rec) #1977 proportion at age for each area

i <- 1
m <- 1
for(i in 1:n.sims) {
  for(m in 1:n.area) {
  N[,1,,m,i] <- Bstart*1e6 * (init.prop[,,m]) #numbers at age 1 (in millions), dim=c(n.sex, n.year, n.age, n.area, n.sims) (for both B and N)
  } #close area
#move year 1 fish...
    for (a in 1:n.age) {
      for (h in 1:n.sex) {
      N[h,1,a,,i] <- N[h,1,a,,i] %*% phi # %*% is the matrix multiplication command 
      } #next sex
    } #next age
  #Calculate biomass from numbers for age 1:
  for (a in 1:n.age) {
    for (h in 1:n.sex) {
      B[h,1,a,,i] <- N[h,1,a,,i] * wa[h,a] #was /wa when the code was starting with biomass and converting to N
  } #next age
  } #next sex
}#next i


#run EM
#read in output from EM
#use R to project EM output forward 1 year, get FOFL, FABC, overall ABC (or 3 ABCs), these values will be used in the next year's fishery
#call apportionment function to apportion ABC to areas, this is the next year's apportionment/catch basis




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
      Fmort[f,y,m,i] <- 0.1#HCR_linear(curr.SSB=temp.ssb, SSB0=SSB0, floor.F=floors[g], ceiling.F=ceilings[g], 
                                 # ascent.range=ascent.range, plot=FALSE)
    }#next g (g for gear?)
    
    #No Recruitment relationship  Can we change this so it reads in a rec value from a separate file which draws N simulations * N years worth of rec values all 
    # at once so the same recruitment can be applied to single and spatial models? Could make it so that if SSB is 0, 0 rec is used instead so we 
    # don't spontaneously generate fish if the pop crashes.
    # Rec needs to be in NUMBERS, not biomass.
    rec[,y-1,m,i] <- 0.5 * exp(mu_rec*(1+mx[,1]) + rnorm(1, 0, sigma_rec)) #Note this has the addition to account for assessment model predicting recruitment to age 2. 
    
    #Ricker
    # rec[,y-1] <- 0.5 * ricker_recruit(ssb[y-1], steep, bo)
    #Beverton-Holt
    # rec[,y-1,i] <-  0.5 * beverton_holt_recruit(sum(ssb[,,y-1,i]), steep, bo=ro) * exp(rnorm(1,0,sigma_rec) - ((sigma_rec^2)/2))
    

    
    
    for(a in 1:n.age) {
      #Update Numbers and Biomass Matrix
      if(a==1) { #Age-1 
        N[,y,a,m,i] <- rec[,y-1,m,i]
        # move age 1
        #N[,y,a,m,i] <- N[,y,a,m,i] %*% phi
        #then calc biomass from moved N (this is changing how it was done before, where B was calculated from rec - is that ok?)
        #B[,y,a,m,i] <- rec[,y-1,m,i]*wa[,a]     
        B[,y,a,m,i] <- N[,y,a,m,i] * wa[,a]
        
      }else {
        #a <- 2
        h <- 1
      #for (a in 1:n.age) {
        for (h in 1:n.sex) {
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
      } #close else
      
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
      
      
    #now that all ages, sexes, and areas filled out, move them before proceeding to next year...
    #for (a in 1:n.age) {
      #for (h in 1:n.sex) {
        #N_temp[h,y,a,,i] <- N[h,y,a,,i] %*% phi # %*% is the matrix multiplication command
        #B_temp[h,y,a,,i] <- B[h,y,a,,i] %*% phi # %*% is the matrix multiplication command
      #} #next sex
    #} #next age
     #N[,y,,,i] <- N_temp[,y,,,i]#change name back after movement      
      
      #1) Generate Assessment Data:
      ##### 'sample' the N and B for survey and fishery abundance indices, comps
      #do we want to read in a vector of seeds that will be applied to all years and sims for consistency?
      #do these functions need to be in the same folder as the rest of the functions?
      #Srv.RPN <- sample_num(N, sigma=0.2, type='lognorm', seed=134) # longline survey RPN
      #Fish.RPW <- sample_biom(B, sigma=0.2, type='lognorm', seed=134)  #fixed gear fishery RPW
      #Fish.AC <- sample_LLfish_AC()  #longline/fixed gear fishery age comps
      #Surv.AC <-  sample_LLsurv_AC() #longline survey age comps
      #nope, not gonna use for now #Fxfish.LC <-  #longline/fixed gear fishery length comps
      #nope, not gonna use for now #Trfish.LC <-  #trawl gear fishery length comps
      #nope, not gonna use for now #Fxsurv.LC <-  #longline survey length comps
      # insert that code here for: where do these go?  how to sample for these and add error?
      # observed catch (based on what for F?), 'current' year, for 6 areas then combine to 3 and to 1 (or how do we want to do this so it reads into a single area and a spatial model?)
      # longline survey RPN, 'current' year, for 6 areas then combine to 3
      # longline/fixed gear fishery CPUE/RPW, lagged 1 year, for 6 areas then combine to 3
      # longline/fixed gear fishery age comps, lagged 1 year, for 6 areas then combine to 3, single sex
      # longline survey age comps, lagged 1 year, for 6 areas then combine to 3, single sex
      # longline/fixed gear fishery length comps, lagged 1 year, for 6 areas then combine to 3, two sexes
      # trawl gear fishery length comps, lagged 1 year, for 6 areas then combine to 3, two sexes
      # longline survey length comps, 'current' year, for 6 areas then combine to 3, two sexes
      
      
      #call the function that makes a .dat file for EM
      
      #=============================================================
      #### Conduct Assessment #### <- this seems like the wrong place for this??
      #2) Call ADMB Model
      #add code here
      #read in EM output/estimates
      #=============================================================
      #### Determine SPR ####
      # doesn't this come out of my EM?
      #=============================================================
      #### Set Harvest Limits ####
    }#next age  
    
    } #next area
    

     

    
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



