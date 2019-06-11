#' add new year of data to vectors, matrices, and arrays that are ready to compile into a .dat file that can be used for the single area EM
#' 
#' @param LLsurvAC_N is the longline survey age comp sample sizes, which are specified in the Run-Sablefish-Sim.R code as LLsurvAC_sampsize
#' @param LLfishAC_N is the longline fishery age comp samples sizes
#' @return 
#' @export 

#require(PBSmodelling) 
#require(admb2R)

build_datfile <- function(LLsurvAC_N,LLfishAC_N) {
  dir.admb.single <- file.path(wd,"admb","Single_area")
  #use the PBSmodeling package to read the ADMB .dat file into the model and update it with new OM generated data
  #note that catch is in 1000 mt units when read in
  testdat <- readList("C:/Repositories/Sablefish_ApportionmentStrategies/admb/Single_area/tem_single2018.dat") 
  #testdat <- readList("C:/Repositories/Sablefish_ApportionmentStrategies/admb/Single_area/permanant_tem_single2018.dat") #should I read this in as global?
  
  #head(testdat, 20)
  #names(testdat)

  #add newly generated data from OM to the .dat file
  testdat$endyr <- testdat$endyr + 1 #advance one year on end year
  #catch
  testdat$n_yrs_catch <- testdat$n_yrs_catch + 1 #advance the number of years of catch data
  testdat$fixed_catch[testdat$n_yrs_catch] <- sum(apportioned_C[y,2,,i]) #add newest fixed gear catch (make sure units are in 1000 mt)
  testdat$trawl_catch[testdat$n_yrs_catch] <- sum(apportioned_C[y,3,,i]) #add newest trawl gear catch (make sure units are in 1000 mt)

  #domestic LL survey RPN (check units)
  testdat$nyrs_domLLsurv <- testdat$nyrs_domLLsurv + 1 #advance one year on number of years
  testdat$yrs_domLLsurv[testdat$nyrs_domLLsurv] <- testdat$yrs_domLLsurv[testdat$nyrs_domLLsurv-1] + 1 #add a year to the sequence
  testdat$obs_domLLsurv_biom[testdat$nyrs_domLLsurv] <- OM_Surv.RPN[y,i] #add newest RPN data from the OM
  testdat$obs_domLLsurv_se[testdat$nyrs_domLLsurv] <- 0.1*testdat$obs_domLLsurv_biom[testdat$nyrs_domLLsurv]#temp fill in bogus values, need to decide if these are needed 
  testdat$obs_domLLsurv_lci[testdat$nyrs_domLLsurv] <- testdat$obs_domLLsurv_biom[testdat$nyrs_domLLsurv]-(2*testdat$obs_domLLsurv_se[testdat$nyrs_domLLsurv]) #add a lower CI value
  testdat$obs_domLLsurv_uci[testdat$nyrs_domLLsurv] <- testdat$obs_domLLsurv_biom[testdat$nyrs_domLLsurv]+(2*testdat$obs_domLLsurv_se[testdat$nyrs_domLLsurv]) #add an upper CI value

  #domestic LL fishery RPW (check units)
  testdat$nyrs_LLfish <- testdat$nyrs_LLfish + 1 #advance the number of years
  testdat$yrs_LLfish[testdat$nyrs_LLfish] <- testdat$yrs_LLfish[testdat$nyrs_LLfish-1] + 1 #add a year to the sequence
  testdat$obs_LLfish_biom[testdat$nyrs_LLfish] <- OM_Fish.RPW[y-1,i] #add newest RPW data from OM
  testdat$obs_LLfish_se[testdat$nyrs_LLfish] <- 0.1* testdat$obs_LLfish_biom[testdat$nyrs_LLfish] #add a SE value
  testdat$obs_LLfish_lci[testdat$nyrs_LLfish] <- testdat$obs_LLfish_biom[testdat$nyrs_LLfish]-(2*testdat$obs_LLfish_se[testdat$nyrs_LLfish])#add a lower CI value
  testdat$obs_LLfish_uci[testdat$nyrs_LLfish] <- testdat$obs_LLfish_biom[testdat$nyrs_LLfish]+(2*testdat$obs_LLfish_se[testdat$nyrs_LLfish])#add an upper CI value
  
  #domestic LL fishery age comps
  testdat$nyrs_LLfish_age <- testdat$nyrs_LLfish_age + 1 #advance the number of years
  testdat$yrs_LLfish_age[testdat$nyrs_LLfish_age] <- testdat$yrs_LLfish_age[testdat$nyrs_LLfish_age-1] + 1 #add a year to the sequence
  testdat$nsamples_LLfish_age_bsaiwgcgeg[testdat$nyrs_LLfish_age] <- LLfishAC_N  #add to number of samples based on the value we read into the function 
  testdat$oac_LLfish_bsaiwgcgeg <- rbind(testdat$oac_LLfish_bsaiwgcgeg,OM_Fish.RPW.age[y-1,,i])

  #domestic LL survey age comps
  testdat$nyrs_domLLsurv_age <- testdat$nyrs_domLLsurv_age + 1 #advance the number of years
  testdat$yrs_domLLsurv_age[testdat$nyrs_domLLsurv_age] <- testdat$yrs_domLLsurv_age[testdat$nyrs_domLLsurv_age-1] + 1 #add a year to the sequence
  testdat$nsamples_domLLsurv_age_bsaiwgcgeg[testdat$nyrs_domLLsurv_age] <- LLsurvAC_N  #add to number of samples
  testdat$oac_domLLsurv_bsaiwgcgeg <- rbind(testdat$oac_domLLsurv_bsaiwgcgeg, OM_Surv.RPN.age[y-1,,i]) #add a row of age comps for year y (or y-1 if we want to maintain the lag)

  
  #Write the new .dat file
  #====================================================================================================
  #================ Get Working Directories, define format vectors, define data call parameters
  #====================================================================================================
  
  ######### Define vector for formatting
    #Sep<-"#=========================================================================================================================="
  
  #====================================================================================================
  #================ Header
  #====================================================================================================
  #L_1<-Sep
  #L_2<-"# Sablefish Model_1_single_area .dat file"
  #L_3<-paste("# Single area model(bsaiwgcgeg) data prepared by Kari Fenske on,",substr(Sys.time(),1,10),sep=" ")
  #L_4<-Sep
  #L_5<-""
  #L_6<-""
  #Header<-c(L_1,L_2,L_3,L_4,L_5,L_6)
  
  #====================================================================================================
  #===== Model Input Parameters/Values
  #====================================================================================================
    ######### Concatenate (first, define lines, then put together)
    L_1<-"# Sablefish Model_1_single_area .dat file"
    L_2<-"# Model input parameters/vectors"
    L_3<-"#styr"
    L_4<-testdat$styr
    L_5<-"#endyr"
    L_6<-testdat$endyr
    L_7<-"#recage"
    L_8<-testdat$recage
    L_9<-"#nages"
    L_10<-testdat$nages
    L_11<-"#nlenbins"
    L_12<-testdat$nlenbins
    L_13<-"#len_bin_labels"
    L_14<-paste(as.vector(testdat$len_bin_labels),collapse=" ")
    L_15<-"#spawn_fract"
    L_16<-testdat$spawn_fract
    L_17<-"#natmort"
    L_18<-"#testdat$natmort"
    L_19<-"#p_mature"
    L_20<-paste(as.vector(testdat$p_mature),collapse=" ")
    L_21<-"#wt_m"
    L_22<-paste(as.vector(testdat$wt_m),collapse=" ")
    L_23<-"#wt_f"
    L_24<-paste(as.vector(testdat$wt_f),collapse=" ")
    
    MIPV<-c(L_1,L_2,L_3,L_4,L_5,L_6,L_7,L_8,L_9,L_10,L_11,L_12,L_13,L_14,L_15,L_16,L_17,L_18,L_19,L_20,L_21,L_22,L_23,L_24)
    
    L_1<-"# Observed Catches"
    L_2<-"#n_yrs_catch"
    L_3<-testdat$n_yrs_catch
    L_4<-"#fixed_catch"
    L_5<-paste(as.vector(testdat$fixed_catch),collapse=" ")
    L_6<-"#trawl_catch"
    L_7<-paste(as.vector(testdat$trawl_catch),collapse=" ")
    
    FC<-c(L_1,L_2,L_3,L_4,L_5,L_6,L_7)
    
    L_1<-"# Domestic longline survey RPN, aka srv3"
    L_2<-"#nyrs_domLLsurv"
    L_3<-testdat$nyrs_domLLsurv
    L_4<-"#yrs_domLLsurv"
    L_5<-paste(as.vector(testdat$yrs_domLLsurv),collapse=" ")
    L_6<-"#obs_domLLsurv_biom"
    L_7<-paste(as.vector(testdat$obs_domLLsurv_biom),collapse=" ")
    L_8<-"#obs_domLLsurv_se"
    L_9<-paste(as.vector(testdat$obs_domLLsurv_se),collapse=" ")
    L_10<-"#obs_domLLsurv_lci"
    L_11<-paste(as.vector(testdat$obs_domLLsurv_lci),collapse=" ")
    L_12<-"#obs_domLLsurv_uci"
    L_13<-paste(as.vector(testdat$obs_domLLsurv_uci),collapse=" ")
  
    LLA<-c(L_1,L_2,L_3,L_4,L_5,L_6,L_7,L_8,L_9,L_10,L_11,L_12,L_13)
    
      
    L_1<-"# Japanese/coop longline survey RPNs by area, aka srv4"
    L_2<-"#nyrs_coopLLsurv"
    L_3<-testdat$nyrs_coopLLsurv
    L_4<-"#yrs_coopLLsurv"
    L_5<-paste(as.vector(testdat$yrs_coopLLsurv),collapse=" ")
    L_6<-"#obs_coopLLsurv_biom"
    L_7<-paste(as.vector(testdat$obs_coopLLsurv_biom),collapse=" ")
    L_8<-"#obs_coopLLsurv_se"
    L_9<-paste(as.vector(testdat$obs_coopLLsurv_se),collapse=" ")
    L_10<-"#obs_coopLLsurv_lci"
    L_11<-paste(as.vector(testdat$obs_coopLLsurv_lci),collapse=" ")
    L_12<-"#obs_coopLLsurv_uci"
    L_13<-paste(as.vector(testdat$obs_coopLLsurv_uci),collapse=" ")
    
    LLA5<-c(L_1,L_2,L_3,L_4,L_5,L_6,L_7,L_8,L_9,L_10,L_11,L_12,L_13)
      
    L_1<-"# Domestic Longline Fishery CPUE (RPW), aka srv5"
    L_2<-"#nyrs_LLfish"
    L_3<-testdat$nyrs_LLfish
    L_4<-"#yrs_LLfish"
    L_5<-paste(as.vector(testdat$yrs_LLfish),collapse=" ")
    L_6<-"#obs_LLfish_biom"
    L_7<-paste(as.vector(testdat$obs_LLfish_biom),collapse=" ")
    L_8<-"#obs_LLfish_se"
    L_9<- paste(as.vector(testdat$obs_LLfish_se),collapse=" ")
    L_10<-"#obs_LLfish_lci"
    L_11<-paste(as.vector(testdat$obs_LLfish_lci),collapse=" ")
    L_12<-"#obs_LLfish_uci"
    L_13<-paste(as.vector(testdat$obs_LLfish_uci),collapse=" ")

    FB1<-c(L_1,L_2,L_3,L_4,L_5,L_6,L_7,L_8,L_9,L_10,L_11,L_12,L_13)      
      
    L_1<-"# Japanese LL fishery RPW, aka srv6"
    L_2<-"#nyrs_JPfish"
    L_3<-testdat$nyrs_JPfish
    L_4<-"#yrs_JPfish"
    L_5<-paste(as.vector(testdat$yrs_JPfish),collapse=" ")
    L_6<-"#obs_JPfish_biom"
    L_7<-paste(as.vector(testdat$obs_JPfish_biom),collapse=" ")
    L_8<-"#obs_JPfish_se"
    L_9<-paste(as.vector(testdat$obs_JPfish_se),collapse=" ")
    L_10<-"#obs_JPfish_lci"
    L_11<-paste(as.vector(testdat$obs_JPfish_lci),collapse=" ")
    L_12<-"#obs_JPfish_uci"
    L_13<-paste(as.vector(testdat$obs_JPfish_uci),collapse=" ")

    FB2<-c(L_1,L_2,L_3,L_4,L_5,L_6,L_7,L_8,L_9,L_10,L_11,L_12,L_13)    
      
    L_1<-"# Domestic LL/Fixed gear fishery age compositions, oac_fish1"
    L_2<-"#nyrs_LLfish_age"
    L_3<-testdat$nyrs_LLfish_age
    L_4<-"#yrs_LLfish_age"
    L_5<-paste(as.vector(testdat$yrs_LLfish_age),collapse=" ")
    L_6<-"#nsamples_LLfish_age_bsaiwgcgeg"
    L_7<-paste(as.vector(testdat$nsamples_LLfish_age_bsaiwgcgeg),collapse=" ")
    L_8<-"#oac_LLfish_bsaiwgcgeg"
    L_9<-paste(as.vector(testdat$oac_LLfish_bsaiwgcgeg[1,]),collapse=" ") #this might need work...
    for(y in 2:length(testdat$oac_LLfish_bsaiwgcgeg[,1])){
      L_add<-paste(as.vector(testdat$oac_LLfish_bsaiwgcgeg[y,]),collapse=" ")
      L_9<-c(L_9,L_add)}

    FAC<-c(L_1,L_2,L_3,L_4,L_5,L_6,L_7,L_8,L_9)
      

    L_1<-"# Domestic LL survey Age Compositions"
    L_2<-"#nyrs_domLLsurv_age"
    L_3<-testdat$nyrs_domLLsurv_age
    L_4<-"#yrs_domLLsurv_age"
    L_5<-paste(as.vector(testdat$yrs_domLLsurv_age),collapse=" ")
    L_6<-"#nsamples_domLLsurv_age_bsaiwgcgeg"
    L_7<-paste(as.vector(testdat$nsamples_domLLsurv_age_bsaiwgcgeg),collapse=" ")
    L_8<-"#oac_domLLsurv_bsaiwgcgeg"
    L_9<-paste(as.vector(testdat$oac_domLLsurv_bsaiwgcgeg[1,]),collapse=" ") #this might need work...
    for(y in 2:length(testdat$oac_domLLsurv_bsaiwgcgeg[,1])){
      L_add<-paste(as.vector(testdat$oac_domLLsurv_bsaiwgcgeg[y,]),collapse=" ")
      L_9<-c(L_9,L_add)}
    
    USAC1<-c(L_1,L_2,L_3,L_4,L_5,L_6,L_7,L_8,L_9)
    
    
    L_1<-"# Domestic LL/Fixed gear fishery length compositions"
    L_2<-"#nyrs_LLfish_size"
    L_3<-testdat$nyrs_LLfish_size
    L_4<-"#yrs_LLfish_size"
    L_5<-paste(as.vector(testdat$yrs_LLfish_size),collapse=" ")
    L_6<-"#nsamples_LLfish_size"
    L_7<-paste(as.vector(testdat$nsamples_LLfish_size),collapse=" ")
    L_8<-"#osc_LLfish_m"
    L_9<-paste(as.vector(testdat$osc_LLfish_m[1,]),collapse=" ")
    for(y in 2:length(testdat$osc_LLfish_m[,1])){
      L_add<-paste(as.vector(testdat$osc_LLfish_m[y,]),collapse=" ")
      L_9<-c(L_9,L_add)}
    L_10<-"#osc_LLfish_f"
    L_11<-paste(as.vector(testdat$osc_LLfish_f[1,]),collapse=" ")
    for(y in 2:length(testdat$osc_LLfish_f[,1])){
      L_add<-paste(as.vector(testdat$osc_LLfish_f[y,]),collapse=" ")
    L_11<-c(L_11,L_add)}
    
    FFS<-c(L_1,L_2,L_3,L_4,L_5,L_6,L_7,L_8,L_9,L_10,L_11)
    
    L_1<-"# U.S. Trawl gear fishery length compositions"
    L_2<-"#nyrs_fish3_size"
    L_3<-testdat$nyrs_fish3_size
    L_4<-"#yrs_fish3_size"
    L_5<-paste(as.vector(testdat$yrs_fish3_size),collapse=" ")
    L_6<-"#nsamples_fish3_size"
    L_7<-paste(as.vector(testdat$nsamples_fish3_size),collapse=" ")
    L_8<-"#osc_fish3_m"
    L_9<-paste(as.vector(testdat$osc_fish3_m[1,]),collapse=" ")
    for(y in 2:length(testdat$osc_fish3_m[,1])){
      L_add<-paste(as.vector(testdat$osc_fish3_m[y,]),collapse=" ")
      L_9<-c(L_9,L_add)}
    L_10<-"#osc_fish3_f"
    L_11<-paste(as.vector(testdat$osc_fish3_f[1,]),collapse=" ")
    for(y in 2:length(testdat$osc_fish3_f[,1])){
      L_add<-paste(as.vector(testdat$osc_fish3_f[y,]),collapse=" ")
      L_11<-c(L_11,L_add)}
    
    TFS<-c(L_1,L_2,L_3,L_4,L_5,L_6,L_7,L_8,L_9,L_10,L_11)
    
    
    L_1<-"# U.S. Domestic LL Survey Length Proportions"
    L_2<-"#nyrs_srv1_size"
    L_3<-testdat$nyrs_srv1_size
    L_4<-"#yrs_srv1_size"
    L_5<-paste(as.vector(testdat$yrs_srv1_size),collapse=" ")
    L_6<-"#nsamples_srv1_size"
    L_7<-paste(as.vector(testdat$nsamples_srv1_size),collapse=" ")
    L_8<-"#osc_srv1_m"
    L_9<-paste(as.vector(testdat$osc_srv1_m[1,]),collapse=" ")
    for(y in 2:length(testdat$osc_srv1_m[,1])){
      L_add<-paste(as.vector(testdat$osc_srv1_m[y,]),collapse=" ")
      L_9<-c(L_9,L_add)}
    L_10<-"#osc_srv1_f"
    L_11<-paste(as.vector(testdat$osc_srv1_f[1,]),collapse=" ")
    for(y in 2:length(testdat$osc_srv1_f[,1])){
      L_add<-paste(as.vector(testdat$osc_srv1_f[y,]),collapse=" ")
      L_11<-c(L_11,L_add)}
    
    USS<-c(L_1,L_2,L_3,L_4,L_5,L_6,L_7,L_8,L_9,L_10,L_11)
    
    
    L_1<-"# US/Japanese coop LL survey length Composition"
    L_2<-"#nyrs_srv2_size"
    L_3<-testdat$nyrs_srv2_size
    L_4<-"#yrs_srv2_size"
    L_5<-paste(as.vector(testdat$yrs_srv2_size),collapse=" ")
    L_6<-"#nsamples_srv2_size"
    L_7<-paste(as.vector(testdat$nsamples_srv2_size),collapse=" ")
    L_8<-"#osc_srv2_m"
    L_9<-paste(as.vector(testdat$osc_srv2_m[1,]),collapse=" ")
    for(y in 2:length(testdat$osc_srv2_m[,1])){
      L_add<-paste(as.vector(testdat$osc_srv2_m[y,]),collapse=" ")
      L_9<-c(L_9,L_add)}
    L_10<-"#osc_srv2_f"
    L_11<-paste(as.vector(testdat$osc_srv2_f[1,]),collapse=" ")
    for(y in 2:length(testdat$osc_srv2_f[,1])){
      L_add<-paste(as.vector(testdat$osc_srv2_f[y,]),collapse=" ")
      L_11<-c(L_11,L_add)}
    
    JPLLSC<-c(L_1,L_2,L_3,L_4,L_5,L_6,L_7,L_8,L_9,L_10,L_11)
    
    
    
    L_1<-"# Size-age transition matrices: proportion at size given age"
    L_2<-"#Size.age.one.males"
    L_3<-paste(as.vector(testdat$Size.age.one.males),collapse=" ")
    L_4<-"#Size.age.one.Females"
    L_5<-paste(as.vector(testdat$Size.age.one.Females),collapse=" ")
    L_6<-"#Size.age.one.unsexed"
    L_7<-paste(as.vector(testdat$Size.age.one.unsexed),collapse=" ")
    L_8<-"#Size.age.2.males"
    L_9<-paste(as.vector(testdat$Size.age.2.males),collapse=" ")
    L_10<-"#size.age.2.females"
    L_11<-paste(as.vector(testdat$size.age.2.females),collapse=" ")
    L_12<-"#Age.age.transition.matrix"    
    L_13<-paste(as.vector(testdat$Age.age.transition.matrix),collapse=" ")
    L_14<-"#eof"
    L_15<-paste(as.vector(testdat$eof),collapse=" ")

    SA<-c(L_1,L_2,L_3,L_4,L_5,L_6,L_7,L_8,L_9,L_10,L_11,L_12,L_13,L_14,L_15)
    
      
    DAT<-c(MIPV,FC,LLA,LLA5,FB1,FB2,FAC,USAC1,FFS,TFS,USS,JPLLSC,SA)
    DAT_NAME<-paste(dir.admb.single,"/tem_single2018",".dat",sep="")
    write.table(DAT,file=DAT_NAME,quote=F,row.names=F,col.names=F)
  #somehow write the file and save to the right folder so we can call the new EM
  #write.table(testdat, file="C:/Repositories/Sablefish_ApportionmentStrategies/admb/Single_area/updated_dat.csv") #eventually it'll need to overwrite the original
  
} #close function
