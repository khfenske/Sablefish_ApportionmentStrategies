#=====================================================
#generate plots to examine the simulated biomass data 
#=====================================================

#str(B)
#head(B)
#saveRDS(B,"B.RDS") or #save(B, file=".RData")
#array order:
#sex (2)
#years (50)
#ages (30)
#reas (6)
#sims (100)

sim_plot_fish_RPW <- function(sim_Fish_RPW_data) {
  # Define Workflow Paths ==============================================================
  # *Assumes you are working from the Sablefish_ApportionmentStrategies R project
  wd <- getwd()
  dir.output <- file.path(wd,"output/sim_data_output_figs") #plots will write to this folder
  #change from generated 5D array to a dataframe for plotting
  meltedB <- melt(sim_Fish_RPW_data, varnames = c("Sex", "Year", "Age", "Area", "Sim"), na.rm=FALSE, value.name = "FisheryRPW")
  
  b2 <- meltedB
  b3<-b2%>%group_by(Year,Area,Sim) %>% summarize(FisheryRPW=sum(FisheryRPW,na.rm=T))
  b4<-b3[b3$Year<21,] ## keeping dimensions reasonable for now
  b4$Year<-as.factor(b4$Year)
  b4$Area<-as.factor(b4$Area)
  ggplot(b4)+geom_boxplot(aes(x=factor(Year),y=FisheryRPW),colour="red")+theme_bw(base_size=13)
  ggplot(b4)+geom_boxplot(aes(x=Year,y=FisheryRPW,fill=Area))+theme_bw(base_size=13)
  ggsave("FishRPW_area_box.png",width=8,height=5,dpi=325, path=dir.output)
  
  b3<-b2%>%group_by(Age,Area,Sim) %>% summarize(FisheryRPW=sum(FisheryRPW,na.rm=T))
  b4<-b3[b3$Age<15,] ## keeping dimensions reasonable for now
  b4$Age<-as.factor(b4$Age)
  b4$Area<-as.factor(b4$Area)
  ggplot(b4)+geom_boxplot(aes(x=Age,y=FisheryRPW),colour="red")+theme_bw(base_size=13)
  ggplot(b4)+geom_boxplot(aes(x=Age,y=FisheryRPW,fill=Area))+theme_bw(base_size=13)
  ggsave("FishRPW_age_box.png",width=8,height=5,dpi=325, path=dir.output)
  
  b3<-b2%>%group_by(Year,Sim) %>% summarize(FisheryRPW=sum(FisheryRPW,na.rm=T))
  b4<-b3
  ggplot(b4)+geom_line(aes(x=Year,y=FisheryRPW,colour=Sim),alpha=0.5)+theme_bw(base_size=13)+
    theme(legend.position="none")+ggtitle("Total FisheryRPW trajectories")
  ggsave("FishRPW_tot_lines.png",width=8,height=5,dpi=325, path=dir.output)
  
  ### do some grouping over stuff
  b3<-b2%>%group_by(Area,Sim) %>% summarize(FisheryRPW=sum(FisheryRPW,na.rm=T))
  b4<-b3
  b4$Area<-as.factor(b4$Area)
  ggplot(b4)+geom_boxplot(aes(x=Area,y=FisheryRPW,fill=Area))+
    theme_bw(base_size=13)+ggtitle("Total FisheryRPW by area all year")                            
  ggsave("FishRPW_area_tot.png",width=8,height=5,dpi=325, path=dir.output)
  
  ### do some grouping over stuff
  b3<-b2%>%group_by(Year,Area,Sim) %>% summarize(FisheryRPW=sum(FisheryRPW,na.rm=T))
  b4<-b3[b3$Year<11,] ## keeping dimensions reasonable for now
  b4$Year<-as.factor(b4$Year)
  b4$Area<-as.factor(b4$Area)
  ggplot(b4)+geom_bar(aes(x=Year,y=FisheryRPW,fill=Area),stat="identity")+
    theme_bw(base_size=13)+ggtitle("Total FisheryRPW by area 10 year")                            
  ggsave("FishRPW_area_tot_10year.png",width=8,height=5,dpi=325, path=dir.output)
  
  ggplot(b4)+geom_bar(aes(x=Year,y=FisheryRPW,fill=Area),position="fill",stat="identity")+
    theme_bw(base_size=13)+ggtitle("Proportion FisheryRPW by area 10 year")+
    scale_y_continuous(labels = scales::percent)
  ggsave("FishRPW_area_prop_10year.png",width=8,height=5,dpi=325, path=dir.output)
  
  ### do some grouping over stuff
  b3<-b2%>%group_by(Year,Area,Sim) %>% summarize(FisheryRPW=sum(FisheryRPW,na.rm=T))
  b4<-b3
  b4$Year<-as.factor(b4$Year)
  b4$Area<-as.factor(b4$Area)
  ggplot(b4)+geom_bar(aes(x=Year,y=FisheryRPW,fill=Area),stat="identity")+
    theme_bw(base_size=13)+ggtitle("Total FisheryRPW by area all year")                            
  ggsave("FishRPW_area_tot_allyear.png",width=8,height=5,dpi=325, path=dir.output)
  
  ggplot(b4)+geom_bar(aes(x=Year,y=FisheryRPW,fill=Area),position="fill",stat="identity")+
    theme_bw(base_size=13)+ggtitle("Proportion FisheryRPW by area all year")+
    scale_y_continuous(labels = scales::percent)
  ggsave("FishRPW_area_prop_allyear.png",width=8,height=5,dpi=325, path=dir.output)
  
}






