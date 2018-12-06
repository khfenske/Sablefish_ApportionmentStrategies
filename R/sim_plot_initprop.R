#=====================================================
#generate plots to examine the simulated initial age proportions
#=====================================================


#for testing
#sim_initprop_data <- init.prop




sim_plot_initprop <- function(sim_initprop_data) {
  # Define Workflow Paths ==============================================================
  # *Assumes you are working from the Sablefish_ApportionmentStrategies R project
  wd <- getwd()
  dir.output <- file.path(wd,"output/sim_data_output_figs") #plots will write to this folder
  #change from generated 5D array to a dataframe for plotting
  melted_init <- melt(sim_initprop_data, varnames = c("Sex", "Age", "Area"), na.rm=FALSE, value.name = "InitProp")
  
  melted_init %>% 
    ggplot(aes(Age,InitProp)) + 
    geom_point(aes(color=Sex),alpha=1) + 
    theme_bw(base_size=13) +
    theme(legend.position="right") +
    ggtitle("Initial Proportions") +
    facet_wrap(~Area)
  ggsave("Selex_surv.png",width=8,height=5,dpi=325, path=dir.output)
}
