#==================================================================================================
#Project Name: SABLEFISH APPORTIONMENT - Data Reading Function
#Creator: Curry James Cunningham, NOAA/NMFS, ABL
#Date: 2.21.18
#
#Purpose: Function to read in catch data (which is confidential) so it's in a separate function from other parameters
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
  dir.x <- file.path("C:/Repositories/hidden files with conf data") #change this path to whatever place you have the confidential catch data files stored. DO NOT LOAD TO GITHUB.
 
  cond.catch <<- read.xlsx(file=file.path(dir.x, input.file), sheetName='CatchConditioning')
  
  }

