#=====================================================
#generate plots to examine the simulated N at age data 
#=====================================================
#testing sim_N_data<-N

sim_plot_NatAge <- function(sim_N_data) {
  # Define Workflow Paths ==============================================================
  # *Assumes you are working from the Sablefish_ApportionmentStrategies R project
  wd <- getwd()
  dir.output <- file.path(wd,"output/sim_data_output_figs") #plots will write to this folder
  #change from generated 5D array to a dataframe for plotting
  meltedN <- melt(sim_N_data, varnames = c("Sex", "Year", "Age", "Area", "Sim"), na.rm=FALSE, value.name = "Numbers")
  #meltedN <- melt(N, varnames = c("Sex", "Year", "Age", "Area", "Sim"), na.rm=FALSE, value.name = "Numbers")
  
  b2 <- meltedN
  
  b3<-b2%>%group_by(Year,Area,Sim) %>% summarize(Numbers=sum(Numbers,na.rm=T))
  b4<-b3 #[b3$Year<21,] ## keeping dimensions reasonable for now
  b4$Year<-as.factor(b4$Year)
  b4$Area<-as.factor(b4$Area)
  ggplot(b4)+geom_boxplot(aes(x=factor(Year),y=Numbers),colour="red")+theme_bw(base_size=13)
  ggplot(b4)+geom_boxplot(aes(x=Year,y=Numbers,fill=Area))+theme_bw(base_size=13)
  ggsave("Numbers_area_box.png",width=8,height=5,dpi=325, path=dir.output)

  b3<-b2%>%group_by(Age,Area,Sim) %>% summarize(Numbers=sum(Numbers,na.rm=T))
  b4<-b3#[b3$Age<15,] ## keeping dimensions reasonable for now
  b4$Age<-as.factor(b4$Age)
  b4$Area<-as.factor(b4$Area)
  ggplot(b4)+geom_boxplot(aes(x=Age,y=Numbers),colour="red")+theme_bw(base_size=13)
  ggplot(b4)+geom_boxplot(aes(x=Age,y=Numbers,fill=Area))+theme_bw(base_size=13)
  ggsave("Numbers_age_box.png",width=8,height=5,dpi=325, path=dir.output)
  
  b3<-b2%>%group_by(Year,Sim) %>% summarize(Numbers=sum(Numbers,na.rm=T))
  b4<-b3
  ggplot(b4)+geom_line(aes(x=Year,y=Numbers,colour=Sim),alpha=0.5)+theme_bw(base_size=13)+
    theme(legend.position="none")+ggtitle("Total Numbers trajectories")
  ggsave("Numbers_tot_lines.png",width=8,height=5,dpi=325, path=dir.output)
  
  ### do some grouping over stuff
  b3<-b2%>%group_by(Area,Sim) %>% summarize(Numbers=sum(Numbers,na.rm=T))
  b4<-b3
  b4$Area<-as.factor(b4$Area)
  ggplot(b4)+geom_boxplot(aes(x=Area,y=Numbers,fill=Area))+
    theme_bw(base_size=13)+ggtitle("Total Numbers by area all year")                            
  ggsave("Numbers_area_tot.png",width=8,height=5,dpi=325, path=dir.output)
  
  ### do some grouping over stuff
  b3<-b2%>%group_by(Year,Area,Sim) %>% summarize(Numbers=sum(Numbers,na.rm=T))
  b4<-b3[b3$Year<11,] ## keeping dimensions reasonable for now
  b4$Year<-as.factor(b4$Year)
  b4$Area<-as.factor(b4$Area)
  ggplot(b4)+geom_bar(aes(x=Year,y=Numbers,fill=Area),stat="identity")+
    theme_bw(base_size=13)+ggtitle("Total Numbers by area 10 year")                            
  ggsave("Numbers_area_tot_10year.png",width=8,height=5,dpi=325, path=dir.output)
  
  ### do some grouping over stuff
  b3<-b2%>%group_by(Year,Area,Sim) %>% summarize(Numbers=sum(Numbers,na.rm=T))
  b4<-b3
  b4$Year<-as.factor(b4$Year)
  b4$Area<-as.factor(b4$Area)
  ggplot(b4)+geom_bar(aes(x=Year,y=Numbers,fill=Area),stat="identity")+
    theme_bw(base_size=13)+ggtitle("Total Numbers by area all year")                            
  ggsave("Numbers_area_tot_allyear.png",width=8,height=5,dpi=325, path=dir.output)  
  
  
  ggplot(b4)+geom_bar(aes(x=Year,y=Numbers,fill=Area),position="fill",stat="identity")+
    theme_bw(base_size=13)+ggtitle("Proportion Numbers by area 10 year")+
    scale_y_continuous(labels = scales::percent)
  ggsave("Numbers_area_prop_10year.png",width=8,height=5,dpi=325, path=dir.output)
  
  ggplot(b4)+geom_bar(aes(x=Year,y=Numbers,fill=Area),position="fill",stat="identity")+
    theme_bw(base_size=13)+ggtitle("Proportion Numbers by area all year")+
    scale_y_continuous(labels = scales::percent)
  ggsave("Numbers_area_prop_allyear.png",width=8,height=5,dpi=325, path=dir.output)
  #the graph below isn't currently functioning, needs work, commented out for now
  #b5<-b2 %>% group_by(Year,Area,Sim) 
  #b5<-b5 %>%  filter(Age==1)
  #b5$Year<-as.factor(b5$Year)
  #b5$Area<-as.factor(b5$Area)
  #ggplot(b5) + 
   # geom_boxplot(aes(x=Year,y=Numbers,fill=Area)) +
  #  theme_bw(base_size=13) + ggtitle("Numbers at age 1") +
  #  scale_y_log10(waiver())
  #ggsave("Number_at_age1_area", width=8,height=5,dpi=325,path=dir.output) #not sure why this one won't print
}  