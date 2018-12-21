#' Sets up initial, spatial sex-specific proportions of numbers at age, based
#' based on 1979 survey RPN spatial proportions and 
#'
#' @return init.prop - matrix of proportions by sex and age
#' @export
#'

#TESTING
#init_yr_N <- init_year_N 
#init_rec <- init_pop_rec
#init_rec_prop <- init_rec_proportion
#fixed_fish_selex <- selex$fish$USfixed_postIFQ
#trawl_fish_selex <- selex$fish$USTrawl
#wt_at_age <- wa
##


calc_init_pop <- function(init_yr_N, init_rec, init_rec_prop, fixed_fish_selex, trawl_fish_selex, wt_at_age) {
  require(dplyr)
  #y<-1 #are these necessary? the y, a, m, h starting values?
  #a<-1
  #m<-1
  #h<-1
  init_years<- c(seq(from=1979,to=2018,by=1)) #set the years for the initial population
  len.yrs<- length(init_years) 
  init_pop <<- array(dim=c(n.sex,len.yrs,n.age,n.area), dimnames=list(sexes,init_years,ages,areas)) #global so it's avail in other functions?
  init_biom <- array(dim=c(n.sex,len.yrs,n.age,n.area), dimnames=list(sexes,init_years,ages,areas)) #will hold the data we want to generate and output
  fratio <<- as.vector(c(0.931769,0.931769,0.931769, 0.745474, 0.925208,0.925208)) # from spatial model .rep fil
  F_est <<- array(dim=c(len.yrs,n.area), dimnames=list(init_years,areas))
  
  #apportion recruitment (initial age) to areas for years 1979-2018 (40 years, 1:40), doing sexes separately
  #don't need a *0.5 here because init_rec is for 1 sex (so half of total rec is already calculated in the input SS), assuming 50:50 split so F=M
  for(y in 1:40) {
    for(m in 1:6) {
      init_pop[1,y,1,m] <<- init_rec[y,1]*  init_rec_prop[1,m] #female n for 1979, first age in model
      init_biom[1,y,1,m] <- init_pop[1,y,1,m]*wt_at_age[1,1] #biomass for 1979, first age in model 
  }}
  for(y in 1:40) {
    for(m in 1:6) {
      init_pop[2,y,1,m] <<- init_rec[y,1]*  init_rec_prop[1,m]#male n for 1979, first age in model
      init_biom[2,y,1,m] <- init_pop[2,y,1,m]*wt_at_age[2,1] #biomass for 1979, first age in model 
    
  }}
  #fill in initial year of initial population data, this is N at age for 1979 from 2018 sablefish stock assessment, and divided into areas as
  #on the spreadsheet indicated in the Sablefish_Input.xlsx file
  for (m in 1:6){ #females
    aaa<-filter(init_yr_N, init_year_N$Sex=='Female'&init_yr_N$Area==m) 
    init_pop[1,1,,m] <<- aaa$Number 
    init_biom[1,1,,m] <- init_pop[1,1,,m]*wt_at_age[1,]
  }
  for (m in 1:6){ #males
    aaa<-filter(init_yr_N, init_year_N$Sex=='Male'&init_yr_N$Area==m) 
    init_pop[2,1,,m] <<- aaa$Number 
    init_biom[1,1,,m] <- init_pop[1,1,,m]*wt_at_age[1,]
  }  
  
  #Get F for year 1
  #it doesn't work for one year, much less the loop below!!
  for(y in 1:1){ 
    for(m in 1:n.area){
      F_est[y,m] <- optimize(get_F,interval=c(0,1),maximum=FALSE)  
      }}
      
  
  #iteratively solve for all the Fs and calculate N at age for years and ages
  for(y in 2:len.yrs){ 
    for(m in 1:n.area){
      #calc year 1 F to apply to year 2
      #F_est[y,m] <- optimize(get_F,lower=0,upper=10,maximum=FALSE)
        
      for(a in 2:n.age){  
        for(h in 1:n.sex){
          #add F from function to an array to track it over years?
          init_pop[h,y,a,m] <<- init_pop[h,y-1,a-1,m]*(exp(-1.0*(F_est[y-1,m] + mx[a-1])))
          init_biom[h,y,a,m] <- init_pop[h,y,a,m]*wt_at_age[h,a]    
        } #next sex
      } #next age
    } #next area
  } #next year
  
  return(init_pop)
  return(init_biom)
  return(F_est)
} #closes function






