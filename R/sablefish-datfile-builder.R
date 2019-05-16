require(PBSmodelling)
require(admb2R)

build_datfile <- function() {
dir.admb.single <- file.path(wd,"admb","Single_area")
#use the PBSmodeling package to read a list of matrices into the model created for the original model
#note that catch is in 1000 mt units when read in
testdat <- readList("C:/Repositories/Sablefish_ApportionmentStrategies/admb/Single_area/tem_single2015.dat")
#head(testdat, 20)
names(testdat)

#str(testdat$p_mature)
#str(testdat$n_yrs_catch)
#str(testdat$fixed_catch)
#str(testdat$oac_LLfish_bsaiwgcgeg)

#add newly generated data from OM to the .dat file
#catch
testdat$endyr <- testdat$endyr + 1 #advance one year on end year
testdat$n_yrs_catch <- testdat$n_yrs_catch + 1 #advance the number of years of catch data
testdat$fixed_catch[testdat$n_yrs_catch] <- OM_fixed_catch[y,i] #add newest fixed gear catch (make sure units are in 1000 mt)
testdat$trawl_catch[testdat$n_yrs_catch] <- OM_trawl_catch[y,i] #add newest trawl gear catch (make sure units are in 1000 mt)

#domestic LL survey RPN (check units)
testdat$nyrs_domLLsurv <- testdat$nyrs_domLLsurv + 1 #advance one year on number of years
testdat$yrs_domLLsurv[testdat$nyrs_domLLsurv] <- testdat$yrs_domLLsurv[testdat$nyrs_domLLsurv-1] + 1
testdat$obs_domLLsurv_biom[testdat$nyrs_domLLsurv] <- OM_Surv.RPN[y,i] 
testdat$obs_domLLsurv_se[testdat$nyrs_domLLsurv] <- 0.1*testdat$obs_domLLsurv_biom[testdat$nyrs_domLLsurv]#temp fill in bogus values, need to decide if these are needed 
testdat$obs_domLLsurv_lci[testdat$nyrs_domLLsurv] <- testdat$obs_domLLsurv_biom[testdat$nyrs_domLLsurv]-(2*testdat$obs_domLLsurv_se[testdat$nyrs_domLLsurv])
testdat$obs_domLLsurv_uci[testdat$nyrs_domLLsurv] <- testdat$obs_domLLsurv_biom[testdat$nyrs_domLLsurv]+(2*testdat$obs_domLLsurv_se[testdat$nyrs_domLLsurv])

#domestic LL fishery RPW (check units)
testdat$nyrs_LLfish <- testdat$nyrs_LLfish + 1 #advance the number of years
testdat$yrs_LLfish[testdat$nyrs_LLfish] <- testdat$yrs_LLfish[testdat$nyrs_LLfish] + 1
testdat$obs_LLfish_biom[testdat$nyrs_LLfish] <- OM_Fish.RPW[y,i]
testdat$obs_LLfish_se[testdat$nyrs_LLfish] <- 0.1* testdat$obs_LLfish_biom[testdat$nyrs_LLfish]
testdat$obs_LLfish_lci[testdat$nyrs_LLfish] <- testdat$obs_LLfish_biom[testdat$nyrs_LLfish]-(2*testdat$obs_LLfish_se[testdat$nyrs_LLfish])
testdat$obs_LLfish_uci[testdat$nyrs_LLfish] <- testdat$obs_LLfish_biom[testdat$nyrs_LLfish]+(2*testdat$obs_LLfish_se[testdat$nyrs_LLfish])
  
#domestic LL fishery age comps
testdat$nyrs_LLfish_age <- testdat$nyrs_LLfish_age + 1 #advance the number of years
testdat$yrs_LLfish_age[testdat$nyrs_LLfish_age] <- testdat$yrs_LLfish_age[testdat$nyrs_LLfish_age] + 1
testdat$nsamples_LLfish_age_bsaiwgcgeg[testdat$nyrs_LLfish_age] <- INCOMPLETE  #add to number of samples
testdat$oac_LLfish_bsaiwgcgeg[testdat$nyrs_LLfish_age,] <- OM_Fish.RPW.age[,,i] #add a row of age comps for year y (or y-1 if we want to maintain the lag)

#domestic LL survey age comps
  
#compliation code (like what's used for the spatial model to make.dat)

#somehow write the file and save to the right folder so we can call the new EM
write.table(testdat, file="C:/Repositories/Sablefish_ApportionmentStrategies/admb/Single_area/updated_dat.csv") #eventually it'll need to overwrite the original

} #close function