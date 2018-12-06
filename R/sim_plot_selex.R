#=====================================================
#generate plots to examine the simulated selectivity
#=====================================================

#single selex - not spatial, 1 area
#selex$surv$USLongline
#selex$surv$USJPLL
#spatial selex - n.areas
#str(selex$fish$USfixed_preIFQ)
#str(selex$fish$USfixed_postIFQ)
#str(selex$fish$USTrawl)
#str(selex$fish$Foreign)

sim_plot_selex <- function(sim_selex_data) {
  # Define Workflow Paths ==============================================================
  # *Assumes you are working from the Sablefish_ApportionmentStrategies R project
  wd <- getwd()
  dir.output <- file.path(wd,"output/sim_data_output_figs") #plots will write to this folder
  #change from generated list to a dataframe for plotting, one df for surveys, one for fisheries
  meltedSurvSel <- melt(sim_selex_data$surv, na.rm=FALSE, value.name = "Selectivity")
  meltedFishSel <- melt(sim_selex_data$fish, na.rm=FALSE, value.name = "Selectivity")
  colnames(meltedSurvSel) <- c("Sex","Age","Selectivity","Survey")
  colnames(meltedFishSel) <- c("Area","Sex","Age","Selectivity","Fishery")  
  
  meltedSurvSel %>% 
    ggplot(aes(Age,Selectivity)) + 
    geom_line(aes(color=Sex),alpha=1) + 
    theme_bw(base_size=13) +
    theme(legend.position="right") +
    ggtitle("Selectivity") +
    facet_wrap(~Survey)
    ggsave("Selex_surv.png",width=8,height=5,dpi=325, path=dir.output)
  
  meltedFishSel %>% 
    filter(Fishery=="Foreign") %>% 
    ggplot(aes(Age,Selectivity)) + 
    geom_line(aes(),alpha=1) + 
    theme_bw(base_size=13) +
    ggtitle("Selectivity") 
  ggsave("Selex_ForeignFish.png",width=8,height=5,dpi=325, path=dir.output)

    meltedFishSel %>% 
    filter(Fishery=="USfixed_preIFQ") %>% 
    ggplot(aes(Age,Selectivity)) + 
    geom_line(aes(color=Sex),alpha=1) + 
    theme_bw(base_size=13) +
    theme(legend.position="right") +
    ggtitle("Selectivity") 
    ggsave("Selex_preIFQfish.png",width=8,height=5,dpi=325, path=dir.output)
    
  meltedFishSel %>% 
    filter(Fishery=="USfixed_postIFQ") %>% 
    ggplot(aes(Age,Selectivity)) + 
    geom_line(aes(color=Sex),alpha=1) + 
    theme_bw(base_size=13) +
    theme(legend.position="right") +
    facet_wrap(~Area)
    ggtitle("Selectivity") #why doesn't the title show up here?
    ggsave("Selex_postIFQ_area.png",width=8,height=5,dpi=325, path=dir.output)
    
  meltedFishSel %>% 
    filter(Fishery=="USfixed_postIFQ") %>% 
    ggplot(aes(Age,Selectivity)) + 
    geom_line(aes(color=as.factor(Area)),alpha=1) + 
    theme_bw(base_size=13) +
    theme(legend.position="right") +
    facet_wrap(~Sex)
    ggtitle("Selectivity") #why doesn't the title show up here?
    ggsave("Selex_postIFQfish_sex.png",width=8,height=5,dpi=325, path=dir.output)
    
  meltedFishSel %>% 
    filter(Fishery=="USTrawl") %>% 
    ggplot(aes(Age,Selectivity)) + 
    geom_line(aes(color=Sex),alpha=1) + 
    theme_bw(base_size=13) +
    theme(legend.position="right") +
    ggtitle("Selectivity") 
    ggsave("Selex_trawlfish.png",width=8,height=5,dpi=325, path=dir.output)
    
}
  