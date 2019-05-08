
#wrapper to call simulated data ploting functions
######################################
#khf, 11-26-18
# run 'Run-Sablefish-Sim.R' first to simulate the data
# run this file to generate the plots

require(dplyr)
require(reshape2)
require(tidyverse)
require(ggplot2)

# Source Necessary Files =========================================
wd <- getwd() #Project Directory
dir.R <- file.path(wd,"R")

source(file.path(dir.R,'sim_plot_numbers.R'))
source(file.path(dir.R,'sim_plot_biom.R'))
source(file.path(dir.R,'sim_plot_fish_RPW.R')) 
source(file.path(dir.R,'sim_plot_initprop.R'))
source(file.path(dir.R,'sim_plot_selex.R'))

#plot simulated biomass data by reading in the generated data (B) to the function.
#Figure files are saved to the folder 'Sim_data_output_figs' in 'output' folder
sim_plot_biom(B)
sim_plot_NatAge(N) 
sim_plot_selex(selex)
sim_plot_initprop(init.prop)
