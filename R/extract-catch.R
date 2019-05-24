#==================================================================================================
#Project Name: SABLEFISH APPORTIONMENT - Data Reading Function
#Creator: Curry James Cunningham, NOAA/NMFS, ABL
#Date: 2.21.18
#
#Purpose: Function to read demographic parameters for operating model
#
#==================================================================================================
#NOTES:
#  a) 
#
#==================================================================================================



extract_catch <- function(input.file="catch_input_conditioning.xlsx") {
  #require(tidyverse)
  #require(readxl)
  #require(xlsx)
  

  # Define Workflow Paths ==============================================================
  # *Assumes you are working from the Sablefish_ApportionmentStrategies R project
  #wd <- getwd()
  dir.x <- file.path("C:/Repositories/hidden files with conf data") #change this path to whatever place you have the confidential catch data files stored. DO NOT LOAD TO GITHUB.
  
  #dir.output <- file.path(wd,"output")
  #dir.data <- file.path(wd,"data")
  

  # Extract: Recruitment values from EM for model conditioning==========
  # these values are summed across sexes and from the 2018 management EM (because grabbing them
  # from my single area EM would only get us values through 2015 at present...so these are a bit different)
  #cond.rec <<- list()  
  cond.catch <<- read.xlsx(file=file.path(dir.x, input.file), sheetName='CatchConditioning')
  
  }

# extract_pars(input.file="Sablefish_Input.xlsx")
  
