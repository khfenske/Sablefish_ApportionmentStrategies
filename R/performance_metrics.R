#' Calculate and or plot the performance metrics
#'
#' @param 
#'
#' @return 
#' @export
#'
#'

#some general things needed for multiple things below
#forward projecting start year, for indexing
forproj.styr <- 44
##### Model validation things
#############################

#Convergence - proportion or number of years in a sim that converged##


#Max Gradient##


#objective function values##
obj_fun_vals[forproj.styr:y,]
#for(f in 1:length(obj_fun_vals[1,])){
 # for(h in 1:length(forproj.styr:y))
  #par(mfrow=c(1,1))
  #plot(obj_fun_vals[,h]~(forproj.styr:y))
#}

#SPR penalty values##
spr_penalty[forproj.styr:y,]

#data likelihood values##
data_likelihood[forproj.styr:y,]

#maturity##

#OM vs EM selectivity##
#longline fishery PreIFQ
par(mfrow=c(2,1))
plot(va[1,1,1,]~ages,typ="l",lwd=4,ylim=c(0,1),main=c("Female LL fishery Pre-IFQ selectivity"),ylab="Selectivity",xlab="Age")
lines(va[1,2,1,]~ages,typ="l",lwd=4)
lines(va[1,3,1,]~ages,typ="l",lwd=4)
lines(va[1,4,1,]~ages,typ="l",lwd=4)
lines(va[1,5,1,]~ages,typ="l",lwd=4)
lines(va[1,6,1,]~ages,typ="l",lwd=4)
for(f in forproj.styr:n.year){
for(h in 1:n.sims){
lines(EM_pred.sel.preifqfish[f,,1,h]~ages,typ="l",lwd=2, col="red")
}}
legend("bottomright",legend=c("EM","OM"),lwd=c(2,4),col=c("red","black"))

plot(va[1,1,2,]~ages,typ="l",lwd=4,ylim=c(0,1),main=c("Male LL fishery Pre-IFQ selectivity"),ylab="Selectivity",xlab="Age")
lines(va[1,2,2,]~ages,typ="l",lwd=4)
lines(va[1,3,2,]~ages,typ="l",lwd=4)
lines(va[1,4,2,]~ages,typ="l",lwd=4)
lines(va[1,5,2,]~ages,typ="l",lwd=4)
lines(va[1,6,2,]~ages,typ="l",lwd=4)
for(f in forproj.styr:n.year){
  for(h in 1:n.sims){
    lines(EM_pred.sel.preifqfish[f,,2,h]~ages,typ="l",lwd=2, col="red")
  }}
legend("bottomright",legend=c("EM","OM"),lwd=c(2,4),col=c("red","black"))

#longline fishery PostIFQ
par(mfrow=c(2,1))
plot(va[2,1,1,]~ages,typ="l",lwd=4,ylim=c(0,1),main=c("Female LL fishery Post-IFQ selectivity"),ylab="Selectivity",xlab="Age")
lines(va[2,2,1,]~ages,typ="l",lwd=4)
lines(va[2,3,1,]~ages,typ="l",lwd=4)
lines(va[2,4,1,]~ages,typ="l",lwd=4)
lines(va[2,5,1,]~ages,typ="l",lwd=4)
lines(va[2,6,1,]~ages,typ="l",lwd=4)
for(f in forproj.styr:n.year){
  for(h in 1:n.sims){
    lines(EM_pred.sel.postifqfish[f,,1,h]~ages,typ="l",lwd=2, col="red")
  }}
legend("bottomright",legend=c("EM","OM"),lwd=c(2,4),col=c("red","black"))

plot(va[2,1,2,]~ages,typ="l",lwd=4,ylim=c(0,1),main=c("Male LL fishery Post-IFQ selectivity"),ylab="Selectivity",xlab="Age")
lines(va[2,2,2,]~ages,typ="l",lwd=4)
lines(va[2,3,2,]~ages,typ="l",lwd=4)
lines(va[2,4,2,]~ages,typ="l",lwd=4)
lines(va[2,5,2,]~ages,typ="l",lwd=4)
lines(va[2,6,2,]~ages,typ="l",lwd=4)
for(f in forproj.styr:n.year){
  for(h in 1:n.sims){
    lines(EM_pred.sel.postifqfish[f,,2,h]~ages,typ="l",lwd=2, col="red")
  }}
legend("bottomright",legend=c("EM","OM"),lwd=c(2,4),col=c("red","black"))

#trawl fishery 
par(mfrow=c(2,1))
plot(va[3,1,1,]~ages,typ="l",lwd=4,ylim=c(0,1),main=c("Female Trawl selectivity"),ylab="Selectivity",xlab="Age")
lines(va[3,2,1,]~ages,typ="l",lwd=4)
lines(va[3,3,1,]~ages,typ="l",lwd=4)
lines(va[3,4,1,]~ages,typ="l",lwd=4)
lines(va[3,5,1,]~ages,typ="l",lwd=4)
lines(va[3,6,1,]~ages,typ="l",lwd=4)
for(f in forproj.styr:n.year){
  for(h in 1:n.sims){
    lines(EM_pred.sel.trawlfish[f,,1,h]~ages,typ="l",lwd=2, col="red")
  }}
legend("topright",legend=c("EM","OM"),lwd=c(2,4),col=c("red","black"))

plot(va[3,1,2,]~ages,typ="l",lwd=4,ylim=c(0,1),main=c("Male Trawl selectivity"),ylab="Selectivity",xlab="Age")
lines(va[3,2,2,]~ages,typ="l",lwd=4)
lines(va[3,3,2,]~ages,typ="l",lwd=4)
lines(va[3,4,2,]~ages,typ="l",lwd=4)
lines(va[3,5,2,]~ages,typ="l",lwd=4)
lines(va[3,6,2,]~ages,typ="l",lwd=4)
for(f in forproj.styr:n.year){
  for(h in 1:n.sims){
    lines(EM_pred.sel.trawlfish[f,,2,h]~ages,typ="l",lwd=2, col="red")
  }}
legend("topright",legend=c("EM","OM"),lwd=c(2,4),col=c("red","black"))

#foreign fishery and USJP survey (maybe add later; would be conditioning years only, so prob not relevant here)

#US longline survey
#trawl fishery 
par(mfrow=c(2,1))
plot(va_surv[1,1,]~ages,typ="l",lwd=4,ylim=c(0,1),main=c("Female LL survey selectivity"),ylab="Selectivity",xlab="Age")
for(f in forproj.styr:n.year){
  for(h in 1:n.sims){
    lines(EM_pred.sel.LLsurv[f,,1,h]~ages,typ="l",lwd=2, col="red")
  }}
legend("topright",legend=c("EM","OM"),lwd=c(2,4),col=c("red","black"))

plot(va_surv[1,2,]~ages,typ="l",lwd=4,ylim=c(0,1),main=c("Male LL survey selectivity"),ylab="Selectivity",xlab="Age")
for(f in forproj.styr:n.year){
  for(h in 1:n.sims){
    lines(EM_pred.sel.LLsurv[f,,2,h]~ages,typ="l",lwd=2, col="red")
  }}
legend("topright",legend=c("EM","OM"),lwd=c(2,4),col=c("red","black"))


#catchability##
EM_q.LLsurv[forproj.styr:y,]
#plot things here
temp.mean <-vector()
temp.mean <- apply(EM_q.LLsurv[forproj.styr:y,],2,mean) #mean of each sim across years

temp.resids

EM_q.USJPsurv[forproj.styr:y,]
EM_q.preifqfish[forproj.styr:y,]
EM_q.postifqfish[forproj.styr:y,]
EM_q.forfish[forproj.styr:y,]

#fit to age comps##
age_likelihood[,forproj.styr:y,] #likelihoods table


##### Performance metrics things
################################

#SSB##

#catch##

#yield##

#Bx%##

#Fx%##




