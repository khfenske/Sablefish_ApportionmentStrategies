#' Sets up initial, spatial sex-specific proportions of numbers at age, based
#' based on 1979 survey RPN spatial proportions and 
#'
#' @return init.prop - matrix of proportions by sex and age
#' @export
#'

#testing
#obs_catch <- init_pop_catch[y,]
#init_pop2 <- init_pop[,y,,]
#wt_at_age <- wa
#ixed_fish_selex <- selex$fish$USfixed_postIFQ
#rawl_fish_selex <- selex$fish$USTrawl
#y=1
#a=1
#m=1
#  F_est <- optimize(get_F,interval=c(0,10),maximum=FALSE)
optimize(get_F,interval=c(0,10),maximum=FALSE)
#  F_est
##Fa=0.1


get_F<-function(Fa){ #<-fix inputs
  FixTr_Catch <- vector(length=n.age)#array(dim=c(n.age), dimnames=list(ages)) #will hold catch for all gear by year and area
  obs_catch <- init_pop_catch[y,m]
  init_pop2 <- init_pop[,y,,m]
  fratio2 <- fratio[m]
  Fa <<- numeric(length=1)
  #wt_at_age <- wa
  fixed_fish_selex2 <- selex$fish$USfixed_postIFQ[m,,]
  trawl_fish_selex2 <- selex$fish$USTrawl[m,,]
  
  for(a in 1:n.age){
    #these are in order of
    #(female gear 1) + (male gear 1) + (female gear 2) + (male gear 2)
    FixTr_Catch[a] <- (init_pop2[1,a]*wa[1,a]*fixed_fish_selex2[1,a]*Fa*(fratio2)/ 
         (Fa*(fratio2)*fixed_fish_selex2[1,a]+mx[1,a])*(1-exp(-Fa*
         (fratio2)*fixed_fish_selex2[1,a]-mx[1,a]))) + 
      (init_pop2[2,a]*wa[2,a]*fixed_fish_selex2[2,a]*Fa*(fratio2)/ 
         (Fa*(fratio2)*fixed_fish_selex2[2,a]+mx[2,a])*(1-exp(-Fa*
         (fratio2)*fixed_fish_selex2[2,a]-mx[2,a]))) + 
      (init_pop2[1,a]*wa[1,a]*trawl_fish_selex2[1,a]*Fa*(fratio2)/ 
         (Fa*(fratio2)*trawl_fish_selex2[1,a]+mx[1,a])*(1-exp(-Fa*
         (fratio2)*trawl_fish_selex2[1,a]-mx[1,a]))) + 
      (init_pop2[2,a]*wa[2,a]*trawl_fish_selex2[2,a]*Fa*(fratio2)/ 
         (Fa*(fratio2)*trawl_fish_selex2[2,a]+mx[2,a])*(1-exp(-Fa*
         (fratio2)*trawl_fish_selex2[2,a]-mx[2,a])))
  }
  
  for(a in 1:n.age) {
    FixTr_CatchSum <- sum(FixTr_Catch[a])  
  }
  
  ssq <- (FixTr_CatchSum-obs_catch)^2   #minimize difference between these
  return(ssq)
  #}
  
} #close function





