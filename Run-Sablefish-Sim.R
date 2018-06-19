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
selex$fish$Foreign <- calc_selectivity(type='fish', fleet='Foreign')

#Calculate Vulnerability @ Age
va <- array(dim=c(n.fish,n.area,n.sex,n.age), dimnames=list(fish,1:n.area,sexes,ages))
i <- 1
for(i in 1:n.fish) {
  va[i,,,] <- calc_selectivity(type='fish', fleet=fish[i])
}

# Create Simulation Objects =======================================
#NOTE: Currently only creates objects for a single area.
create_sim_objects()

# Initialize Population (year 1) =============================================================
#   Should probably update to start from FISHED equilibrium
init.prop <- calc_init_age_prop(bo=mu_rec)

i <- 1
for(i in 1:n.sims) {
  B[,1,,i] <- Bstart*1e6 * (init.prop)
  N[,1,,i] <- B[,1,,i] / wa
}#next i


# Forward Simulation =============================================================
#Temporarily Assuming Single Area
area <- 1

i <- 1
for(i in 1:n.sims) {
  print(paste('Sim:',i,'of',n.sims))
  
  y <- 2
  for(y in 2:n.year) {
    
    #Initial Recruitment
    ssb[,,y-1,i] <- ma*wa*N[,y-1,,i]
    
    
    #HARVEST CONTROL RULE
    temp.ssb <- sum(ssb[,,y-1,i])
    
    #=============================================================
    #### Conduct Assessment ####
    
    #1) Generate Index of Abundance from Survey
    
    #2) Call ADMB Model
    
    #=============================================================
    #### Determine SPR ####
    
    #=============================================================
    #### Set Harvest Limits ####
    
    f <- 1
    for(f in 1:n.fish) {
      Fmort[f,y,i] <- 0.1#HCR_linear(curr.SSB=temp.ssb, SSB0=SSB0, floor.F=floors[g], ceiling.F=ceilings[g], 
                                 # ascent.range=ascent.range, plot=FALSE)
    }#next g
    
    #No Recruitment relationship
    rec[,y-1,i] <- 0.5 * exp(mu_rec*(1+mx[,1]) + rnorm(1, 0, sigma_rec)) #Note this has the addition to account for assessment model predicting recruitment to age 2. 
    
    #Ricker
    # rec[,y-1] <- 0.5 * ricker_recruit(ssb[y-1], steep, bo)
    #Beverton-Holt
    # rec[,y-1,i] <-  0.5 * beverton_holt_recruit(sum(ssb[,,y-1,i]), steep, bo=ro) * exp(rnorm(1,0,sigma_rec) - ((sigma_rec^2)/2))
    
    a <- 1
    for(a in 1:n.age) {
      #Update Numbers and Biomass Matrix
      if(a==1) { #Age-1
        N[,y,a,i] <- rec[,y-1,i]
        B[,y,a,i] <- rec[,y-1,i]*wa[,a]
        # N[,y,a] <- rec[,y-1]/wa[,a]
        # B[,y,a] <- rec[,y-1]
      }else {
        h <- 1
        for(h in 1:n.sex) {
          #Instantaneous Version
          F.a[h,y-1,a-1,i] <- sum(Fmort[,y,i]*va[,area,h,a-1])
          Z.a[h,y-1,a-1,i] <- F.a[h,y-1,a-1,i] + mx[h,a-1]  #Natural mortality is NOT time-varying
          
          #Continuous
          surv[h,y-1,a-1,i] <- exp(-Z.a[h,y-1,a-1,i])
          mort[h,y-1,a-1,i] <- 1-surv[h,y-1,a-1,i]
          
          #Update
          
          N[h,y,a,i] <- N[h,y-1,a-1,i]*surv[h,y-1,a-1,i]
          # B[h,y,a,i] <- B[h,y-1,a-1,i]*surv[h,y-1,a-1,i]
          B[h,y,a,i] <- N[h,y,a,i]*wa[h,a]
          #Total Catch
          C.n[h,y-1,a-1,i] <- N[h,y-1,a-1,i] * (F.a[h,y-1,a-1,i]/Z.a[h,y-1,a-1,i]) * (1-exp(-1*Z.a[h,y-1,a-1,i])) #Catch in number of halibut
          C.b[h,y-1,a-1,i] <- C.n[h,y-1,a-1,i] * wa[h,a-1]
          
          f <- 1
          for(f in 1:n.fish) {
            temp.F <- Fmort[f,y,i]*va[f,area,h,a-1]
            # temp.Z <- temp.F + mx[h,a-1]
            temp.Z <- sum(Fmort[,y,i]*va[,area,h,a-1]) + mx[h,a-1]
            
            harvest.n[h,y-1,a-1,f,i] <- N[h,y-1,a-1,i] * (temp.F/temp.Z) * (1-exp(-1*temp.Z))

            harvest.b[h,y-1,a-1,f,i] <- harvest.n[h,y-1,a-1,f,i] * wa[h,a-1]
          }#next gear
        }#next sex
      }
      
      if(a==A) {
        h <- 1
        for(h in 1:n.sex) {
          #Fish in Plus Group
          F.a[h,y-1,a,i] <- sum(Fmort[,y,i]*va[,area,h,a])
          Z.a[h,y-1,a,i] <- F.a[h,y-1,a,i] + mx[h,a]  #Natural mortality is NOT time-varying        
          
          #Continuous
          surv[h,y-1,a,i] <- exp(-Z.a[h,y-1,a,i])
          mort[h,y-1,a,i] <- 1-surv[h,y-1,a,i]
          
          #Update
          N[h,y,a,i] <- N[h,y,a,i] + N[h,y-1,a,i]*surv[h,y-1,a,i] #New Entrants (calculated above), plus existing plus group occupants.
          # B[h,y,a,i] <- B[h,y,a,i] + B[h,y-1,a,i]*surv[h,y-1,a,i] 
          B[h,y,a,i] <- N[h,y,a,i] * wa[h,a]
          #Total Catch
          C.n[h,y-1,a,i] <- N[h,y-1,a,i] * (F.a[h,y-1,a,i]/Z.a[h,y-1,a,i]) * (1-exp(-1*Z.a[h,y-1,a,i])) #Catch in number of halibut
          C.b[h,y-1,a,i] <- C.n[h,y-1,a,i] * wa[h,a]
          
          f <- 1
          for(f in 1:n.fish) {
            temp.F <- Fmort[g,y,i]*va[f,area,h,a]
            # temp.Z <- temp.F + mx[h,a]
            temp.Z <- sum(Fmort[,y,i]*va[,area,h,a]) + mx[h,a]
            # 
            harvest.n[h,y-1,a,f,i] <- N[h,y-1,a,i] * (temp.F/temp.Z) * (1-exp(-1*temp.Z))
            harvest.b[h,y-1,a,f,i] <- harvest.n[h,y-1,a,f,i] * wa[h,a]
          }#next gear
        }#next sex
      }# If plus age group
    }#next age  
    
  }#next y
  
}#next i























