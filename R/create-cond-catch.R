#' Take the catch values by area and gear (from a single area EM)
#' get into the correct format, and apply selectivity so we have
#' catch at age 
#'
#' @param EMcatch - this is catch for conditioning the OM, by area, gear, and year
#' @param vuln.age - vulnerability at age
#' @param Ctype - output catch in numbers (millions) or biomass (kt)
#'        Ctype = 1 outputs catch in biomass (Kt), Ctype=2 outputs catch in numbers (millions)
#'
#' @return EMcond_catch
#' @export


cond_catch_AA  <- function(EMcatch, vuln.age, Ctype) {
#build catch by year, area and gear arrays
cond.catch2 <- array(dim=c(43, n.area, n.fish), dimnames=list(1:43,1:n.area,fish))
cond.catch2[,1,1] <- c(EMcatch$FG1[1:20], rep(0, times=23))
cond.catch2[,2,1] <- c(EMcatch$FG2[1:20], rep(0, times=23))
cond.catch2[,3,1] <- c(EMcatch$FG3[1:20], rep(0, times=23))
cond.catch2[,4,1] <- c(EMcatch$FG4[1:20], rep(0, times=23))
cond.catch2[,5,1] <- c(EMcatch$FG5[1:20], rep(0, times=23))
cond.catch2[,6,1] <- c(EMcatch$FG6[1:20], rep(0, times=23))

cond.catch2[,1,2] <- c(rep(0, times=20), EMcatch$FG1[21:43])
cond.catch2[,2,2] <- c(rep(0, times=20), EMcatch$FG2[21:43])
cond.catch2[,3,2] <- c(rep(0, times=20), EMcatch$FG3[21:43])
cond.catch2[,4,2] <- c(rep(0, times=20), EMcatch$FG4[21:43])
cond.catch2[,5,2] <- c(rep(0, times=20), EMcatch$FG5[21:43])
cond.catch2[,6,2] <- c(rep(0, times=20), EMcatch$FG6[21:43])

cond.catch2[,1,3] <- EMcatch$TG1
cond.catch2[,2,3] <- EMcatch$TG2
cond.catch2[,3,3] <- EMcatch$TG3
cond.catch2[,4,3] <- EMcatch$TG4
cond.catch2[,5,3] <- EMcatch$TG5
cond.catch2[,6,3] <- EMcatch$TG6

cond.catch2[is.na(cond.catch2)]<-0

#=====calculate catch by sex, area, gear, year, age
EMcond_catch <- array(data=NA, dim=c(43,n.fish,n.area,n.sex,n.age),dimnames=list(1:43,fish,1:n.area,sexes,ages))
va_prop <- array(dim=c(n.fish,n.area,n.sex,n.age), dimnames=list(fish,1:n.area,sexes,ages))

#make vulnerablility into a proportional multiplier 
for(m in 1:n.area) { 
  for(f in 1:n.fish) {
    va_prop[f,m,,] <- prop.table(vuln.age[f,m,,])
  }
}
#get catch at age by area
for(y in 1:43) {
  for(m in 1:n.area) { 
    for(f in 1:n.fish) {
      for(h in 1:n.sex) {
        for(a in 1:n.age) {
          EMcond_catch[y,f,m,h,a] <- cond.catch2[y,m,f] * va_prop[f,m,h,a]
        }
      }
    }
  }
}

#switch between catch in N or kt
if(Ctype==1) {
  #sum over fishery/gear for catch in kt by year, age, sex, area
  EMcond_catch2 <- array(data=NA, dim=c(43,n.area,n.sex,n.age),dimnames=list(1:43,1:n.area,sexes,ages))
  EMcond_catch2[,,,] <- EMcond_catch[,1,,,] +EMcond_catch[,2,,,]+EMcond_catch[,3,,,]+EMcond_catch[,4,,,]
  return(EMcond_catch2)
  
}else {
  
#convert from catch in kt (millions of kg) of biomass to numbers in millions
  EMcond_catch <- EMcond_catch*1000000 #catch in kg
  for (a in 1:n.age){
    for (h in 1:n.sex) {
    EMcond_catch[,,,h,a] <- EMcond_catch[,,,h,a]/(wa[h]) #divide by weight at age (in kg) to get catch in numbers
    }}
  EMcond_catch <- EMcond_catch/1000000 #catch in millions of fish (numbers)

  #sum over fishery/gear for catch in number by year, age, sex, area
  EMcond_catch2 <- array(data=NA, dim=c(43,n.area,n.sex,n.age),dimnames=list(1:43,1:n.area,sexes,ages))
  EMcond_catch2[,,,] <- EMcond_catch[,1,,,] +EMcond_catch[,2,,,]+EMcond_catch[,3,,,]+EMcond_catch[,4,,,]
  return(EMcond_catch2) #numbers of catch at age,sex,area,
  }
}


####Testing
#EMcatch=cond.catch
#vuln.age=va
#y=1
#m=1
#f=1
#h=1
#a=1

#EMcond_catch[y,,,,]
#sum(EMcond_catch[,,,,])
#va_prop[1,,1,]
#va_prop[1,,2,]
#wa
