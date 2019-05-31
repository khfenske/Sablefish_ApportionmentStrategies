#=====================================================
#generate plots to examine the simulated initial age proportions
#=====================================================


#for testing
#sim_initprop_data <- N[,2:43,,,]




sim_plot_initprop <- function() {
  require(PBSmodelling) 
  require(admb2R)
  
  # Define Workflow Paths ==============================================================
  # *Assumes you are working from the Sablefish_ApportionmentStrategies R project
  wd <- getwd()
  dir.output <- file.path(wd,"output/sim_data_output_figs") #plots will write to this folder
  
  #read in data from single area EM
  #dir.admb.single <- file.path(wd,"admb","Single_area")
  #use the PBSmodeling package to read the ADMB .dat file into the model and update it with new OM generated data
  #note that catch is in 1000 mt units when read in
  testdat <- readList("C:/Repositories/Sablefish_ApportionmentStrategies/admb/Single_area/permanant_tem_single2015.dat")
  
  
  #make some plots  
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
    plot(testdat$yrs_domLLsurv,testdat$obs_domLLsurv_biom, typ="l", ylim=c(0,700))
    lines(testdat$yrs_domLLsurv,OM_Surv.RPN[15:40,1])
    
    #fishery RPW
    plot(testdat$yrs_LLfish,testdat$obs_LLfish_biom, typ="l", ylim=c(0,1500))
    lines(testdat$yrs_LLfish,OM_Fish.RPW[15:39,1])    

    }
