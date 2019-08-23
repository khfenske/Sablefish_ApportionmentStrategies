#=====================================================
#generate plots to examine the simulated initial (conditioning period) population
#=====================================================


sim_plot_initpop <- function() {
  #require(PBSmodelling) #can evenutually remove this

  
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
  OMyears <- c(1977:2018)

####make some plots  
  #wt at age
  plot(wa[1,]~ages,typ="l",lwd=3,col="black")
  lines(wa[2,]~ages,typ="l",lwd=3,col="black",lty=3)
  lines(mgmt_dat$wt_f~ages,typ="p",lwd=3,col="red",lty=1)
  lines(mgmt_dat$wt_m~ages,typ="p",lwd=3,col="red",lty=3)
  
  #maturity at age
  plot(ma[1,]~ages,typ="l",lwd=3,col="black")
  lines(ma[2,]~ages,typ="l",lwd=3,col="black",lty=3)
  lines(mgmt_dat$p_mature~ages,typ="p",lwd=3,col="red",lty=1)
  
  
  #abundance N plots
  #change from generated 5D array to a dataframe for plotting
  #melted_Ninit <- melt(N, varnames = c("Sex", "Year","Age", "Area","Sim"), na.rm=FALSE, value.name = "Numbers")
  #b2 <- melted_Ninit
  
  #b3<-b2%>%group_by(Year,Sim) %>% summarize(Numbers=sum(Numbers,na.rm=T))
  #b4<-b3
  #ggplot(b4)+geom_line(aes(x=Year,y=Numbers,colour=Sim),alpha=0.5)+theme_bw(base_size=13)+
   # theme(legend.position="none")+ggtitle("Total Numbers trajectories")
  #ggsave("Numbers_tot_lines.png",width=8,height=5,dpi=325, path=dir.output)  
  
  dim(N)
  simN_sum <- apply(N[,,,,1],2,sum)
  simB_sum <- apply(B[,,,,1],2,sum)
  mgmtN <- c(227.569424,225.133078,214.348775,202.312974,234.635297,228.675439,
             223.024404,233.474335,226.238316,210.835746,197.26262,183.638328,
             162.608407,138.835745,123.192946,118.53197,108.080044,93.414099,
             83.964022,159.561953,166.4444777,157.2009806,180.5002287,183.5209969,
             205.8722927,184.1836206,181.7986075,174.4102706,150.830862,129.4616883,
             113.7451334,121.4683674,104.0961752,111.7859086,99.2854377,89.0569055,
             82.837678,87.163893,77.2629253,96.086156,100.0417057,96.0967192,
             124.029391,113.7307861,110.2138382,99.9701582,95.7489713,89.5424044,
             84.6328623,80.147036,86.599641,79.9309313,77.8211843,67.23660782,
             64.6833935,68.2611382,208.8285434,225.8192621,212.4078021)
  (mgmtN) #59 years  
  par(mfrow=c(1,1))
  plot(mgmtN~mgmt_rep_years, ylim=c(0,250),typ="l",lwd=3,col="red")
  lines(simN_sum[2:43]~OMyears,typ="l",lwd=3,col="black") # plotting one sim worth of N for illustration - but all sims should be same?
  
  #N_F <- apply(N[1,,,,1],c(1,2),sum)
  #N_M <- apply(N[2,,,,1],c(1,2),sum)
  #par(mfrow=c(6,6))
  #for(a in 1:n.age){  
  #plot(N_F[,a]~years,typ="l",col="red")
  #lines(N_M[,a]~years,typ="l",col="blue")
  #}

  
  #apply(N[,43,,,1],c(2),sum)
  #write.table(ma,"clipboard")
  apply(N[,1,,,1],c(1,2,3),sum)
  ab <- apply(N[,,,,1],c(2,3),sum)
  #proportion N at age and B at age
  ab123 <- apply(N[1,,,,1],c(1,2),sum)
  abc <- prop.table(ab123,1) #proportion F at age for each year
  mab123 <- apply(N[2,,,,1],c(1,2),sum)
  mabc <- prop.table(ab123,1) #proportion F at age for each year
  bab123 <- apply(B[1,,,,1],c(1,2),sum)
  babc <- prop.table(bab123,1) #proportion F at age for each year
  bmab123 <- apply(B[2,,,,1],c(1,2),sum)
  bmabc <- prop.table(bab123,1) #proportion F at age for each year
  
  plot(abc[1,]~ages,typ="l",lwd=3,col="red",ylim=c(0,1)) #1976
  lines(mabc[1,]~ages,typ="l",lwd=3,col="blue")
  lines(abc[5,]~ages,typ="l",lwd=3,col="red")
  lines(mabc[5,]~ages,typ="l",lwd=3,col="blue")
  lines(babc[1,]~ages,typ="l",lwd=3,col="orange")
  lines(bmabc[1,]~ages,typ="l",lwd=3,col="green")  
  
  #proportion F:M
  #ab123[1,]/ab[1]
  
  #total biomass
  OM_totB <- apply(B[,,,,1],2,sum) #b dim = sex,year,age,area,sim
  mgmt_rep$Tot_biom # for years 1960:2018
  plot(mgmt_rep$Tot_biom[18:59]~mgmt_rep_years[18:59],typ="l",lwd=3,col="red",ylim=c(0,600))
  lines(OM_totB[2:43]~c(1977:2018),typ="l",lwd=3,col="black")
  #spawning biomass
    mgmt_rep$SpBiom #1960-2018
  ssb_fsum <- apply(ssb[1,,2:43,,1],2,sum) #1977-2018 female OM SSB
  B2 <- apply(B[,,,,1],2,sum) #female OM Biomass
  par(mfrow=c(1,1))
  plot(ssb_fsum~mgmt_rep_years[18:59],typ="l",lwd=3,col="black",ylim=c(0,250))
  lines(mgmt_rep$SpBiom[18:59]~mgmt_rep_years[18:59],lwd=3,col="red")
  #lines(B2[2:43]~mgmt_rep_years[18:59],col="black",typ="p",lwd=3)
  #lines(mgmt_rep$Tot_biom[18:59]~mgmt_rep_years[18:59],lwd=3,col="red")
  
  plot(mgmtN~mgmt_rep_years, ylim=c(0,450),typ="l",lwd=3,col="red")
  lines(simN_sum[2:43]~OMyears,typ="l",lwd=3,col="black") # plotting one sim worth of N for illustration - but all sims should be same?
  lines(B2[2:43]~OMyears,col="green",typ="l",lwd=3)
  lines(ssb_fsum~OMyears)
  #melted_ssb <- melt(ssb,varnames = c("Sex", "Age","Year", "Area","Sim"), na.rm=FALSE, value.name = "ssb")
  #b2 <-melted_ssb
  #b3<-b2%>%group_by(Year,Sim) %>% summarize(ssb=sum(ssb,na.rm=T))
  #b4<-b3
  #ggplot(b4)+geom_line(aes(x=Year,y=ssb,colour=Sim),alpha=0.5)+theme_bw(base_size=13)+
    #theme(legend.position="none")+ggtitle("ssb trajectories")
  #ggsave("ssb_tot_lines.png",width=8,height=5,dpi=325, path=dir.output)  
  
  
  #catch
  mgmtCatch <- mgmt_rep$Obs_CatchTrawl + mgmt_rep$Obs_CatchFixedGear
  C.b_sum<-vector(length=n.year) 
  OM_sumcatch <- vector(length=n.year)
  for (y in 2:43){
    C.b_sum[y] <- sum(C.b[,y,,,1]) #catch in biomass summed across sexes, ages, areas
    OM_sumcatch[y] <- OM_fixed_catch[y,1]+OM_trawl_catch[y,1] #harvest in biomass summed across sexes, ages, areas and gears
  }

  plot(mgmtCatch~mgmt_rep_years,typ="l",col="red",lwd=3) #mgmt catch
  lines(C.b_sum[2:43]~OMyears,lwd=3) #sim catch 
  lines(OM_sumcatch[2:43]~OMyears, col="blue",lwd=3)#sim harvest
  
  #PLOT CATCH BY GEAR/FLEET 
  C.n
  C.b
  C.b_sum2 <- apply(C.b[,,,,1],2,sum)
  C.b2fem <- apply(C.b[1,,,,1],1,sum) #catch in biomass for years, females summed over age,area, for sim1
  C.b3fem <- apply(C.b[1,,,,1],c(1,3),sum) #catch in biomass for years, areas, females summed over age, for sim1
  C.b2mal <- apply(C.b[2,,,,1],1,sum) #catch in biomass for years, males summed over sex,age,area, for sim1
  C.b3mal <- apply(C.b[2,,,,1],c(1,3),sum) #catch in biomass for years, areas, males summed over age, for sim1
  
  harvest.n2fem <- apply(harvest.b[1,,,,,1],c(1),sum)#harvest in biomass for years, females summed over gear,age,area, for sim1
  harvest.n2mal <- apply(harvest.b[2,,,,,1],c(1),sum)#harvest in biomass for years, males summed over gear,age,area, for sim1
  cond.catch2 <- apply(cond.catch[,2:13],1,sum)
  temp.catchnumbiom2 <- apply(temp.catchnumbiom,1,sum)
  cond_catch_at_age2 <- apply(cond_catch_at_age,1,sum) #how does cond catch at age have sex?
  
  par(mfrow=c(1,1))
  plot(C.b_sum2~years,typ="l",col="black",lwd=3,ylim=c(0,40))
  lines(C.b2fem+C.b2mal~years,col="blue")
  lines(C.b2fem~years,typ="l",col="red",lwd=3) 
  lines(harvest.n2fem~years,typ="l",col="red",lwd=3) #catch and harvet match? yep
  lines(C.b2mal~years,typ="l",col="blue",lwd=3)
  lines(harvest.n2mal~years,typ="l",col="blue",lwd=3) #catch and harvet match? yep
  lines(temp.catchnumbiom2~c(1:43),typ="l",col="green",lwd=3)
  lines(cond_catch_at_age2~c(1:43),typ="l",col="darkgreen",lwd=3)
  lines(cond.catch2~c(1:43),typ="l",col="orange",lwd=3)
  
  
  plot(C.b3fem[,1]~years,typ="l",col="blue",lwd=3,ylim=c(0,15))
  lines(C.b3fem[,2]~years,typ="l",col="green",lwd=3)
  lines(C.b3fem[,3]~years,typ="l",col="orange",lwd=3)
  lines(C.b3fem[,4]~years,typ="l",col="red",lwd=3)
  lines(C.b3fem[,5]~years,typ="l",col="black",lwd=3)
  lines(C.b3fem[,6]~years,typ="l",col="purple",lwd=3)
  lines(C.b3mal[,1]~years,typ="l",col="blue",lwd=3,lty=3)
  lines(C.b3mal[,2]~years,typ="l",col="green",lwd=3,lty=3)
  lines(C.b3mal[,3]~years,typ="l",col="orange",lwd=3,lty=3)
  lines(C.b3mal[,4]~years,typ="l",col="red",lwd=3,lty=3)
  lines(C.b3mal[,5]~years,typ="l",col="black",lwd=3,lty=3)
  lines(C.b3mal[,6]~years,typ="l",col="purple",lwd=3,lty=3)
  
  
  #sex ratio generated by create-cond-catch
  ttt <- apply(cond_catch_at_age,c(1,3,4),sum) #dim=year,gear,area, sex, age
  for(m in 1:n.area){
  rrr <- ttt[,m,1]/ttt[,m,2]
  uuu <- ttt[,m,1]/(ttt[,m,1]+ttt[,m,2])
  }
  
  
  #recruitment
  #mgmt_rep_recruitment <- mgmt_rep$Numbers_Females[,1] + mgmt_rep$Numbers_Males[,1]
  mgmt_rep_recruitment <- c(122.9618,20.0246,15.45504,16.50548,57.0088,18.69596,18.98642,36.665,21.0668,15.71956,
       17.67248,16.18402,9.21424,7.10778,8.0244,16.61306,9.04232,4.41974,5.29922,86.386,25.42,9.9051,
       42.303,23.9728,43.5138,2.33748,19.53042,18.80922,3.6228,4.52162,6.87626,27.2682,1.286332,23.7892,4.59836,
       5.31364,7.73184,16.88416,2.30162,29.923,16.93562,10.15266,41.4692,6.34222,12.67834,6.05714,10.71728,
       7.79438,8.44232,7.82108,17.84292,5.03906,9.29366,1.01781,7.98824,13.20866,150.343,40.1198,12.4912)
  recruits.area #recruits for the forward projecting period
  sim_rec <- apply(recruits.area,c(1,3),sum)
  cond.rec$Recruitment #recruits for the conditioning period
  mean_sim <- mean(mgmt_rep_recruitment[18:58])
  mean_mgmt <- mean(sim_rec)
  
  plot(sim_rec[,1]~c(1976:(1975+length(years))),xlim=c(1960,2020),typ="l",lwd=3,col="blue",ylim=c(0,300))
  for(i in 1:n.sims){
    lines(sim_rec[,i]~c(1976:(1975+length(years))),typ="l",lwd=1,col="blue")
  }
  lines(mgmt_rep_recruitment~mgmt_rep_years,typ="l",lwd=3,col="red")  
  lines(cond.rec$Recruitment~OMyears,lty=3,lwd=3,col="black")

  
  #indices - Survey
  par(mfrow=c(1,1))
  OM_Surv.RPN
  rpn_years <- c(15:43)
  #temporary specify these since I can't read them in
  mgmt_rep_SurveyN <- c(640.728503016315,578.357884316905, 497.686157855691,
                              548.856594587629, 476.199957695552, 486.633273082579,
                              506.761565260931, 477.009736419158, 473.82221883975,
                              526.031097836628, 455.567006420982, 534.67133980174,
                              549.580737938698, 515.776225130287, 539.73401087771,
                              541.428138528616, 568.917927484529, 507.987087585334,
                              460.977929394305, 414.194846870673, 457.871240499099,
                              554.820613993086, 444.38539069095, 420.076843267757,
                              484.066574150728, 385.215796739882, 494.334197637078,
                              561.460234787346, 611.493838991725)
  #the sim survey vs the mgmt model observed survey values
  plot(mgmt_rep_SurveyN~c(15:43),ylim=c(0,1000),typ="l",lwd=3,col="red") #management model
  for(i in 1:n.sims){
    lines(OM_Surv.RPN[15:43,i]~rpn_years, lwd=1,col="black") #simulated values
  }

  
  #indices - fishery index vs the mgmt observed fishery values
  OM_Fish.RPW
  rpw_years <- c(15:42)
  #temporary specify these since I can't read them in
  mgmt_rep_FishBiomass <- c(1200.559703, 1065.846303, 907.7053904, 903.693265,
                            822.1961692, 1243.377135, 1200.869005, 1341.139079,
                            1129.655707, 1326.1280020879, 1138.84538350516,
                            1118.29202628419, 1143.37866120555, 1218.72872733656,
                            1360.37863957325, 1313.26648248675, 1216.08850975844,
                            1280.59748998437, 1380.05115178867, 1131.96389722547,
                            1064.98552084971, 1056.38875761707, 1033.7860426452,
                            908.191324166509,969.481383346887,847.541829934837,
                            656.339331262708, 656.427863349683)
  #the sim 1 survey vs the mgmt model observed survey values
  plot(mgmt_rep_FishBiomass~c(15:42),ylim=c(0,2500),typ="l",lwd=3,col="red")
  for(i in 1:n.sims){
  lines(OM_Fish.RPW[15:42,i]~rpw_years, lwd=1,col="black")
  }

  
  
  
   #age comps
  #Spatial movement obs vs pred age comps - fixed gear fishery (fish1) AREA 1
  length(OMyears)
  OM_Surv.RPN.age
  dim(OM_Surv.RPN.age)
  
  #surv age comps
  par(mfrow=c(3,3))
  for(i in 1:n.sims){
  for(y in 1:length(OM_Surv.RPN.age[,1,i])) {
    plot(OM_Surv.RPN.age[y,,i]~ages,ann=FALSE, typ="l", lty=1, col="red" , lwd=2, ylim=c(0,0.33))
    #lines(B77$obs_fish1_age_1[i,]~B77$ages, typ="p", pch=16)
    legend("topright",legend=c(y))
  }}

  #fish age comps
  par(mfrow=c(3,3))
  for(i in 1:n.sims){
    for(y in 1:length(OM_Fish.RPW.age[,1,i])) {
      plot(OM_Fish.RPW.age[y,,i]~ages,ann=FALSE, typ="l", lty=1, col="black" , lwd=2, ylim=c(0,0.33))
      #lines(B77$obs_fish1_age_1[i,]~B77$ages, typ="p", pch=16)
      legend("topright",legend=c(y))
    }}
  #Surv RPN at age by year
  par(mfrow=c(3,2))
  for(i in 1:n.sims){
    for(m in 1:n.area) {
    for(y in 1:length(Surv.RPN[1,y,,1,1])) {
      plot(Surv.RPN[1,y,,m,1]~ages,ann=FALSE, typ="l", lty=1, col="red" , lwd=2, ylim=c(0,10))
      #lines(B77$obs_fish1_age_1[i,]~B77$ages, typ="p", pch=16)
      legend("topright",legend=c(y,m))
    }}}  
  #fish RPW at age by year
  par(mfrow=c(3,2))
  for(i in 1:n.sims){
    for(m in 1:n.area) {
      for(y in 1:length(Fish.RPW[1,y,,1,1])) {
        plot(Fish.RPW[1,y,,m,1]~ages,ann=FALSE, typ="l", lty=1, col="red" , lwd=2, ylim=c(0,10))
        #lines(B77$obs_fish1_age_1[i,]~B77$ages, typ="p", pch=16)
        legend("topright",legend=c(y,m))
      }}} 
  
  #Fmort
  #F.mort[f,y,m,i]
  par(mfrow=c(2,3))
  #for(i in 1:n.sims){
    for(f in 1:n.fish) {
      for(m in 1:n.area){
      plot(F.mort[f,,m,i]~years,ann=FALSE, typ="l", lty=1, col="red" , lwd=2, ylim=c(0,0.3))
      #lines(B77$obs_fish1_age_1[i,]~B77$ages, typ="p", pch=16)
      legend("topright",legend=c(f,m))
      }}#}

  #selectivity
  dim(va)
  #fishery by area
  par(mfrow=c(2,2))
  for(m in 1:n.area){
    for(f in 1:n.fish){
  plot(va[f,m,1,]~ages,typ="l",col="black",lwd=3,ylim=c(0,1))
  lines(va[f,m,2,]~ages,typ="l",col="blue",lwd=3,lty=3)
  title(paste("Area",m,"Fishery",fish[f]))
    }
  }
 #fishery by fleet
  par(mfrow=c(3,2))
    for(f in 1:n.fish){  
      for(m in 1:n.area){
      plot(va[f,m,1,]~ages,typ="l",col="black",lwd=3,ylim=c(0,1))
      lines(va[f,m,2,]~ages,typ="l",col="blue",lwd=3,lty=3)
      title(paste("Area",m,"Fishery",fish[f]))
    }
  }  
  #fishery all on one 
  par(mfrow=c(4,2))
  plot(va[1,1,2,]~ages,typ="l",col="black",lwd=3,ylim=c(0,1))
  lines(va[1,2,2,]~ages,typ="l",col="blue",lwd=3,lty=3)
  lines(va[1,3,2,]~ages,typ="l",col="blue",lwd=3,lty=3)
  lines(va[1,4,2,]~ages,typ="l",col="blue",lwd=3,lty=3)
  lines(va[1,5,2,]~ages,typ="l",col="blue",lwd=3,lty=3)
  lines(va[1,6,2,]~ages,typ="l",col="blue",lwd=3,lty=3)
  title(paste("Fishery",fish[1],"Males"))
  
  plot(va[1,1,1,]~ages,typ="l",col="black",lwd=3,ylim=c(0,1))
  lines(va[1,2,1,]~ages,typ="l",col="blue",lwd=3,lty=3)
  lines(va[1,3,1,]~ages,typ="l",col="blue",lwd=3,lty=3)
  lines(va[1,4,1,]~ages,typ="l",col="blue",lwd=3,lty=3)
  lines(va[1,5,1,]~ages,typ="l",col="blue",lwd=3,lty=3)
  lines(va[1,6,1,]~ages,typ="l",col="blue",lwd=3,lty=3)
  title(paste("Fishery",fish[1],"Females"))
  
  plot(va[2,1,2,]~ages,typ="l",col="black",lwd=3,ylim=c(0,1))
  lines(va[2,2,2,]~ages,typ="l",col="blue",lwd=3,lty=3)
  lines(va[2,3,2,]~ages,typ="l",col="blue",lwd=3,lty=3)
  lines(va[2,4,2,]~ages,typ="l",col="blue",lwd=3,lty=3)
  lines(va[2,5,2,]~ages,typ="l",col="blue",lwd=3,lty=3)
  lines(va[2,6,2,]~ages,typ="l",col="blue",lwd=3,lty=3)
  title(paste("Fishery",fish[2],"Males"))
  
  plot(va[2,1,1,]~ages,typ="l",col="black",lwd=3,ylim=c(0,1))
  lines(va[2,2,1,]~ages,typ="l",col="blue",lwd=3,lty=3)
  lines(va[2,3,1,]~ages,typ="l",col="blue",lwd=3,lty=3)
  lines(va[2,4,1,]~ages,typ="l",col="blue",lwd=3,lty=3)
  lines(va[2,5,1,]~ages,typ="l",col="blue",lwd=3,lty=3)
  lines(va[2,6,1,]~ages,typ="l",col="blue",lwd=3,lty=3)
  title(paste("Fishery",fish[2],"Females"))
  
  plot(va[3,1,2,]~ages,typ="l",col="black",lwd=3,ylim=c(0,1))
  lines(va[3,2,2,]~ages,typ="l",col="blue",lwd=3,lty=3)
  lines(va[3,3,2,]~ages,typ="l",col="blue",lwd=3,lty=3)
  lines(va[3,4,2,]~ages,typ="l",col="blue",lwd=3,lty=3)
  lines(va[3,5,2,]~ages,typ="l",col="blue",lwd=3,lty=3)
  lines(va[3,6,2,]~ages,typ="l",col="blue",lwd=3,lty=3)
  title(paste("Fishery",fish[3],"Males"))
  
  plot(va[3,1,1,]~ages,typ="l",col="black",lwd=3,ylim=c(0,1))
  lines(va[3,2,1,]~ages,typ="l",col="blue",lwd=3,lty=3)
  lines(va[3,3,1,]~ages,typ="l",col="blue",lwd=3,lty=3)
  lines(va[3,4,1,]~ages,typ="l",col="blue",lwd=3,lty=3)
  lines(va[3,5,1,]~ages,typ="l",col="blue",lwd=3,lty=3)
  lines(va[3,6,1,]~ages,typ="l",col="blue",lwd=3,lty=3)
  title(paste("Fishery",fish[3],"Females"))  

  plot(va[4,1,2,]~ages,typ="l",col="black",lwd=3,ylim=c(0,1))
  lines(va[4,2,2,]~ages,typ="l",col="blue",lwd=3,lty=3)
  lines(va[4,3,2,]~ages,typ="l",col="blue",lwd=3,lty=3)
  lines(va[4,4,2,]~ages,typ="l",col="blue",lwd=3,lty=3)
  lines(va[4,5,2,]~ages,typ="l",col="blue",lwd=3,lty=3)
  lines(va[4,6,2,]~ages,typ="l",col="blue",lwd=3,lty=3)
  title(paste("Fishery",fish[4],"Males"))
  
  plot(va[4,1,1,]~ages,typ="l",col="black",lwd=3,ylim=c(0,1))
  lines(va[4,2,1,]~ages,typ="l",col="blue",lwd=3,lty=3)
  lines(va[4,3,1,]~ages,typ="l",col="blue",lwd=3,lty=3)
  lines(va[4,4,1,]~ages,typ="l",col="blue",lwd=3,lty=3)
  lines(va[4,5,1,]~ages,typ="l",col="blue",lwd=3,lty=3)
  lines(va[4,6,1,]~ages,typ="l",col="blue",lwd=3,lty=3)
  title(paste("Fishery",fish[4],"Females")) 
  
  #survey by area
  par(mfrow=c(3,4))
  #for(m in 1:n.area){
    for(f in 1:n.surv){
      plot(va_surv[f,1,]~ages,typ="l",col="black",lwd=3,ylim=c(0,1))
      lines(va_surv[f,2,]~ages,typ="l",col="blue",lwd=3,lty=3)
      title(paste("Area","Survey",surv[f]))
    #}
  }
  #survey by fleet
  par(mfrow=c(3,2))
  for(f in 1:n.surv){  
    #for(m in 1:n.area){
      plot(va_surv[f,1,]~ages,typ="l",col="black",lwd=3,ylim=c(0,1))
      lines(va_surv[f,2,]~ages,typ="l",col="blue",lwd=3,lty=3)
      title(paste("Area","Survey",surv[f]))
    #}
  }  
  
  
  #selectivty OM vs Mgmt EM
  #by fleet for fishery
  #US fixed gear pre IFQ
  par(mfrow=c(3,2))
  for(m in 1:n.area){
    plot(va[1,m,1,]~ages,typ="l",col="black",lwd=3,ylim=c(0,1))
    lines(va[1,m,2,]~ages,typ="l",col="black",lwd=3,lty=3)
    lines(mgmt_rep$Fishery_sel1Females~ages,typ="l",col="red",lwd=3,lty=1)
    lines(mgmt_rep$Fishery_sel1Males~ages,typ="l",col="orange",lwd=3,lty=3)
    title(paste("Area",m,"Fishery",fish[1]))
  }
  
  #US fixed gear post IFQ
  par(mfrow=c(3,2))
  for(m in 1:n.area){
    plot(va[2,m,1,]~ages,typ="l",col="black",lwd=3,ylim=c(0,1))
    lines(va[2,m,2,]~ages,typ="l",col="black",lwd=3,lty=3)
    lines(mgmt_rep$Fishery_sel4_f~ages,typ="l",col="red",lwd=3,lty=1)
    lines(mgmt_rep$Fishery_sel4_m~ages,typ="l",col="orange",lwd=3,lty=3)
    title(paste("Area",m,"Fishery",fish[2]))
  }  

  #US trawl fish
  par(mfrow=c(3,2))
  for(m in 1:n.area){
    plot(va[3,m,1,]~ages,typ="l",col="black",lwd=3,ylim=c(0,1))
    lines(va[3,m,2,]~ages,typ="l",col="black",lwd=3,lty=3)
    lines(mgmt_rep$Fishery_sel3~ages,typ="l",col="red",lwd=3,lty=1)
    title(paste("Area",m,"Fishery",fish[3]))
  }    
  
  #US fixed gear foreign
  par(mfrow=c(3,2))
  for(m in 1:n.area){
    plot(va[4,m,1,]~ages,typ="l",col="black",lwd=3,ylim=c(0,1))
    lines(va[4,m,2,]~ages,typ="l",col="black",lwd=3,lty=3)
    lines(mgmt_rep$Fishery_sel2~ages,typ="l",col="red",lwd=3,lty=1)
    title(paste("Area",m,"Fishery",fish[4]))
  } 
  
  #US ll survey
  par(mfrow=c(1,1))
      plot(va_surv[1,1,]~ages,typ="l",col="black",lwd=3,ylim=c(0,1))
      lines(va_surv[1,2,]~ages,typ="l",col="black",lwd=3,lty=3)
      lines(mgmt_rep$Survey_sel1Female~ages,typ="l",col="red",lwd=3,lty=1)
      lines(mgmt_rep$Survey_sel1male~ages,typ="l",col="orange",lwd=3,lty=3)
      title(paste("Area","Survey","US LL"))
  
  #USJP ll coop survey
  par(mfrow=c(1,1))
    plot(va_surv[2,1,]~ages,typ="l",col="black",lwd=3,ylim=c(0,1))
    lines(va_surv[2,2,]~ages,typ="l",col="black",lwd=3,lty=3)
    lines(mgmt_rep$Survey_sel2Female~ages,typ="l",col="red",lwd=3,lty=1)
    lines(mgmt_rep$Survey_sel2male~ages,typ="l",col="orange",lwd=3,lty=3)
    title(paste("Area","Survey", "USJP coop"))
  
      }
