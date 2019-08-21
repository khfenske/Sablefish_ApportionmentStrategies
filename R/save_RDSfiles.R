#==================================================================================================
#Project Name: SABLEFISH APPORTIONMENT
#Creator: 
#Date: 
#
#Purpose: Function to save RDS files for each object we want to track and save to the 
#correct apportionment folder
#
#==================================================================================================
#NOTES:
#  a) 
#
#==================================================================================================

saveFerris <- function() {
#save all the things from the OM space and EM output
setwd(dir.output)

#save the generic things to the output folder  
saveRDS(n.year,"n.year.rds")
saveRDS(years,"years.rds")
saveRDS(n.sims,"n.sims.rds")
saveRDS(sims,"sims.rds")
saveRDS(n.age,"n.age.rds")
saveRDS(ages,"ages.rds")
saveRDS(n.sex,"n.sex.rds")
saveRDS(sexes,"sexes.rds")
saveRDS(n.area,"n.area.rds")
saveRDS(areas,"areas.rds")
saveRDS(age.rec,"age.rec.rds")
saveRDS(A,"A.rds")
saveRDS(apport.opt,"apport.opt.rds")
saveRDS(n.fish,"n.fish.rds")
saveRDS(fish,"fish.rds")

#save the OM conditioning period things to the output folder
saveRDS(ssb,"ssb.rds")
saveRDS(C.b,"C.b.rds" )
saveRDS(C.n,"C.n.rds" )
saveRDS(OM_fixed_catch,"OM_fixed_catch.rds" )
saveRDS(OM_trawl_catch,"OM_trawl_catch.rds" )
saveRDS(recruits.area,"recruits.area.rds" )
saveRDS(cond.rec$Recruitment,"cond.Recruitment.rds" ) 
saveRDS(OM_Surv.RPN,"OM_Surv.RPN.rds" )
saveRDS(OM_Fish.RPW,"OM_Fish.RPW.rds" )
saveRDS(N,"N.rds" )
saveRDS(OM_Surv.RPN.age,"OM_Surv.RPN.age.rds" )
saveRDS(OM_Fish.RPW.age,"OM_Fish.RPW.age.rds" )
saveRDS(Surv.RPN,"Surv.RPN.rds" )
saveRDS(Fish.RPW,"Fish.RPW.rds")
saveRDS(F.mort,"F.mort.rds" )
saveRDS(va,"va.rds" )
saveRDS(va_surv,"va_surv.rds" ) 


#save the apportionment-specific EM things to an apportionment folder
setwd(paste0(dir.output,"/Apport.Option_",apport.opt)) #switch to WD where we want to save the files

saveRDS(max_grads,"maxgrads.rds")
saveRDS(obj_fun_vals,"obj_fun_vals.rds")
saveRDS(spr_penalty,"spr_penalty.rds")
saveRDS(data_likelihood,"data_likelihood.rds")
saveRDS(ABC_projection,"ABC_projection.rds")
saveRDS(age_likelihood,"age_likelihood.rds")
#saveRDS(index_likelihood,"index_likelihood.rds")
saveRDS(EM_B40,"EM_B40.rds")
saveRDS(EM_SBF40,"EM_SBF40.rds")
saveRDS(EM_SBF35,"EM_SBF35.rds")
#saveRDS(EM_SBF0,"EM_SBF0.rds")
saveRDS(ABC_projection,"ABC_projection.rds")
saveRDS(EM_depletion1,"EM_depletion1.rds" )
saveRDS(EM_depletion2,"EM_depletion2.rds" )
saveRDS(EM_spbiom,"EM_spbiom.rds" )
saveRDS(EM_pred.srvRPN,"EM_pred.srvRPN.rds" )
saveRDS(EM_pred.fishRPW,"EM_pred.fishRPW.rds" )
saveRDS(EM_predrec,"EM_predrec.rds" )
saveRDS(EM_predcatch_fixedgear,"EM_predcatch_fixedgear.rds" )
saveRDS(EM_predcatch_trawlgear,"EM_predcatch_trawlgear.rds" )
saveRDS(EM_pred.sel.preifqfish,"EM_pred.sel.preifqfish.rds" ) 
saveRDS(EM_pred.sel.preifqfish,"EM_pred.sel.preifqfish.rds" )
saveRDS(EM_pred.sel.postifqfish,"EM_pred.sel.postifqfish.rds" )
saveRDS(EM_pred.sel.postifqfish,"EM_pred.sel.postifqfish.rds" )
saveRDS(EM_pred.sel.trawlfish,"EM_pred.sel.trawlfish.rds" )
saveRDS(EM_pred.sel.trawlfish,"EM_pred.sel.trawlfish.rds" )
saveRDS(EM_pred.sel.LLsurv,"EM_pred.sel.LLsurv.rds" )
saveRDS(EM_pred.sel.LLsurv,"EM_pred.sel.LLsurv.rds" )
saveRDS(EM_q.LLsurv,"EM_q.LLsurv.rds" ) 
saveRDS(EM_q.USJPsurv,"EM_q.USJPsurv.rds" )
saveRDS(EM_q.preifqfish,"EM_q.preifqfish.rds" )
saveRDS(EM_q.postifqfish,"EM_q.postifqfish.rds" )
saveRDS(EM_q.forfish,"EM_q.forfish.rds" )
#saveRDS(EM_predAC.surv,"EM_predAC.surv.rds" )
#saveRDS(EM_predAC.fish,"EM_predAC.fish.rds" )
#saveRDS(EM_natage,"EM_natage.rds" )
#saveRDS(EM_LLcatchatage"EM_LLcatchatage.rds" )
#saveRDS(EM_TRcatchatage,"EM_TRcatchatage.rds" )
#saveRDS(EM_totbiomass,"EM_totbiomass.rds" ) 
#saveRDS(EM_F.a,"EM_F.a.rds" )


setwd(wd) #return to original working directory

}