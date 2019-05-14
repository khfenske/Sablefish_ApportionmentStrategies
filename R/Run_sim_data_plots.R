
#wrapper to call simulated data ploting functions
######################################
#khf, 11-26-18
# run 'Run-Sablefish-Sim.R' first to simulate the data
# run this file to generate the plots


# Source Necessary Files =========================================
wd <- getwd() #Project Directory
dir.R <- file.path(wd,"R")

source(file.path(dir.R,'sim_plot_numbers.R'))
source(file.path(dir.R,'sim_plot_biom.R'))
source(file.path(dir.R,'sim_plot_fish_RPW.R')) 
source(file.path(dir.R,'sim_plot_initprop.R'))
source(file.path(dir.R,'sim_plot_selex.R'))


# Plot settings ==================================================
require(dplyr) #these are also called in Run-Sablefish-Sim.R
require(reshape2)
require(tidyverse)
require(ggplot2)

##############################################
#plot set up  (this section needs work - copied from KB's SPASAM code)
##############################################
#set up the colors you want to use for TRUE and ESTIMATED
t.col="black" #true
e.col="blue"  #estimated

#set up the color that are wanted for MOVEMENT plot - used a color ramp
mycols=colorRampPalette(c("blue", "cyan","black"))

# Select Line width
line.wd=0.8

#select threshold correlation level for corrlation plot
cor.level = 0.4# can change this in the make.plots function below

# Call plots =====================================================
#These should run right now, still need work though...
#plot simulated biomass data by reading in the generated data (B) to the function.
#Figure files are saved to the folder 'Sim_data_output_figs' in 'output' folder
sim_plot_biom(B)
sim_plot_NatAge(N) 
sim_plot_selex(selex)
sim_plot_initprop(init.prop)
