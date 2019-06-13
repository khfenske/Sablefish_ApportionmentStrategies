#=====================================================
#generate plots to examine the simulated initial (conditioning period) population
#=====================================================


sim_plot_initpop <- function() {
  require(PBSmodelling) #can evenutually remove this

  
  # Define Workflow Paths ==============================================================
  # *Assumes you are working from the Sablefish_ApportionmentStrategies R project
  wd <- getwd()
  dir.output <- file.path(wd,"output/sim_data_output_figs") #plots will write to this folder
  #dir.input <- file.path(wd,"admb/Single_area") #pull in .dat and .rep files from this folder
  
  #read in data from single area management model/Kari's single model (it's sort of hybrid data)
  mgmt_dat <- readList("C:/Repositories/Sablefish_ApportionmentStrategies/admb/Single_area/permanant_tem_single2018.dat")
  head(mgmt_dat)
  #read in data from the single area mgmt report file
  mgmt_rep <- readList("C:/Repositories/Sablefish_ApportionmentStrategies/admb/Management/tem_simplified.rep")
  head(mgmt_rep)
  mgmt_rep_years <- c(1960:2018)
  
  #read in data from the conditioning OM years
  OM <- readList("C:/Repositories/Sablefish_ApportionmentStrategies/admb/Single_area/tem_single2018.dat")
  head(OM)
  #OM data that's not in the .dat file (aka 'report' type data) includes:
  dim(ssb)
  dim(N)
  dim(C.n)
  dim(C.b)
  dim(OM_Surv.RPN)
  dim(OM_Fish.RPW)
  dim(OM_Fish.RPW.age)
  dim(OM_Surv.RPN.age)
  dim(Surv.RPN)
  dim(Fish.RPW)
  dim(Fish.AC)
  dim(Surv.AC) 
  OMyears <- c(1977:2018)
  #make some plots  

  #spawning biomass
  mgmt_rep$SpBiom
  ssb_fsum <- apply(ssb[1,2:30,2:43,,1],2,sum)
  plot(ssb_fsum~mgmt_rep_years[18:59])
  lines(mgmt_rep$SpBiom[18:59]~mgmt_rep_years[18:59])
  
  #ggsave("ssb_tot_lines.png",width=8,height=5,dpi=325, path=dir.output)  
  
  
  melted_ssb <- melt(ssb,varnames = c("Sex", "Age","Year", "Area","Sim"), na.rm=FALSE, value.name = "ssb")
  b2 <-melted_ssb
  b3<-b2%>%group_by(Year,Sim) %>% summarize(ssb=sum(ssb,na.rm=T))
  b4<-b3
  ggplot(b4)+geom_line(aes(x=Year,y=ssb,colour=Sim),alpha=0.5)+theme_bw(base_size=13)+
    theme(legend.position="none")+ggtitle("ssb trajectories")
  #ggsave("ssb_tot_lines.png",width=8,height=5,dpi=325, path=dir.output)  
  
  
  #catch
  mgmtCatch <- mgmt_rep$Obs_CatchTrawl + mgmt_rep$Obs_CatchFixedGear
  C.b_sum <- apply(C.b[,1:42,,,1],2,sum)
  
  plot(mgmtCatch~mgmt_rep_years)
  lines(C.b_sum~OMyears)
  
  
  #recruitment
  mgmt_rep_recruitment <- mgmt_rep$Numbers_Females[,1] + mgmt_rep$Numbers_Males[,1]
  recruits.area #recruits for the forward projecting period
  cond.rec$Recruitment #recruits for the conditioning period
  
  plot(mgmt_rep_recruitment~mgmt_rep_years)
  lines(cond.rec$Recruitment~OMyears)
  
  
  
  
  
  ###below here needs work...##
  
  
  #abundance N plots
  #change from generated 5D array to a dataframe for plotting
  melted_Ninit <- melt(N, varnames = c("Sex", "Year","Age", "Area","Sim"), na.rm=FALSE, value.name = "Numbers")
  b2 <- melted_Ninit
  
  b3<-b2%>%group_by(Year,Sim) %>% summarize(Numbers=sum(Numbers,na.rm=T))
  b4<-b3
  ggplot(b4)+geom_line(aes(x=Year,y=Numbers,colour=Sim),alpha=0.5)+theme_bw(base_size=13)+
    theme(legend.position="none")+ggtitle("Total Numbers trajectories")
  ggsave("Numbers_tot_lines.png",width=8,height=5,dpi=325, path=dir.output)  
  
  
  #biomass B plots
  melted_Binit <- melt(B, varnames = c("Sex", "Year","Age", "Area","Sim"), na.rm=FALSE, value.name = "Biomass")
  b2 <- melted_Binit
  
  b3<-b2%>%group_by(Year,Sim) %>% summarize(Biomass=sum(Biomass,na.rm=T))
  b4<-b3
  ggplot(b4)+geom_line(aes(x=Year,y=Biomass,colour=Sim),alpha=0.5)+theme_bw(base_size=13)+
    theme(legend.position="none")+ggtitle("Total Biomass trajectories")
  ggsave("Biomass_tot_lines.png",width=8,height=5,dpi=325, path=dir.output)  
  
  #compare EMdata to condition output
  #survey RPN
  plot(testdat$yrs_domLLsurv,testdat$obs_domLLsurv_biom, typ="l", ylim=c(0,1000))
  lines(testdat$yrs_domLLsurv,OM_Surv.RPN[15:40,1])
  
  #fishery RPW
  plot(testdat$yrs_LLfish,testdat$obs_LLfish_biom, typ="l", ylim=c(0,2500))
  lines(testdat$yrs_LLfish,OM_Fish.RPW[15:39,1])    
  
  
    
    }
