#' Sets up initial, spatial sex-specific proportions of numbers at age, based
#' based on 1979 survey RPN spatial proportions and 
#'
#' @return init.prop - matrix of proportions by sex and age
#' @export
#'

calc_init_pop <- function(init_yr_N, init_rec, init_catch, fixed_fish_selex, trawl_fish_selex) {
  y<-1
  a<-1
  m<-1
  h<-1
  mort<- 0.1
  init_years<- c(seq(from=1979,to=2018,by=1)) #set the years for the initial population
  len.yrs<- length(init_years) 
  init_pop <- array(dim=c(n.sex,len.yrs,n.age,n.area), dimnames=list(sexes,years,ages,areas))
  
  #first year is equal to the input values from the sablefish_input.xlsx spreadsheet
  for (h in 1:n.sex) {
    for (a in 1:n.age) {
      for (m in i:n.area) {
  init_pop[h,1,a,m] <- init_year_N[h,a,m]
how do i get init_year_N to be in a form that I can do this?
      } #next area
    } #next age
  } #next sex
  
  
  #fill in subsequent years
  init_pop <- ()
  for(y in 2:len.yrs){ #needs more loops
    read in recruitment for year y age 1 (need to divide into areas)
    calc F rate (at age?) from current (or prev?) year catch and abundance, and the various selectivities
    #calc N at age
    init_pop[,y,,] <- init_pop[,y-1,,]*exp(-Z)
    
  } #next year
  
  return(init.pop)
} #closes function




melt(init_year_N)
