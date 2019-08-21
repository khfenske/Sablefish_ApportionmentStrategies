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

#read in and build data across the apportionment types




#Max Gradient - convergence##
#proportion of years in a sim that converged
#average proportion of years in all sims that converged for each apportionment option
conv_num <- matrix(NA,ncol=n.sims,nrow=n.year)
#max_grads <- (format(max_grads,scientific=FALSE))
for(i in 1:n.sims){
  for(y in forproj.styr:n.year){
    if(abs(max_grads[y,i]) < 0.1) {
    conv_num[y,i] <- 1      
    } else {
      conv_num[y,i] <- 0        
    }
  }
}
#proportion converged 
#(proportion of sims that converged in year y)
temp_a <- apply(conv_num[forproj.styr:n.year,],1,sum)
(temp_a1 <- temp_a/n.sims) 

#(proportion of years that converged in each sim i)
temp_b <- apply(conv_num[forproj.styr:n.year,],2,sum)
(temp_b1 <- temp_b/length(forproj.styr:n.year))

#proportion of sims and years that converged
sum(temp_b)/(length(forproj.styr:n.year)*n.sims) 


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
colnames(EM_q.LLsurv) <- c(1:n.sims)
rownames(EM_q.LLsurv) <- c(1:n.year)
as.data.frame(EM_q.LLsurv)
EM_q.LLsurv[forproj.styr:y,]
EMQ.LLSmean <-vector()
(EMQ.LLSmean <- apply(EM_q.LLsurv[forproj.styr:y,],2,mean)) #mean of each sim across years
EMQ.LLSmedian <-vector()
(EMQ.LLSmedian <- apply(EM_q.LLsurv[forproj.styr:y,],2,median)) #median of each sim across years
qmean <- mean(EM_q.LLsurv[forproj.styr:y,])
qmed <- median(EM_q.LLsurv[forproj.styr:y,])
melted_EMqLLsrv <- melt(EM_q.LLsurv[forproj.styr:y,],na.rm=FALSE,value.name="Catchability") #=c("year","sim")) how do I name var1 var2?

ggplot() + 
  geom_histogram(data=melted_EMqLLsrv,aes(Catchability),binwidth=1/10) + 
  geom_vline(aes(xintercept=qmed,color="EM_median",linetype="EM_median")) + 
  geom_vline(aes(xintercept=qmean, color="EM_mean", linetype="EM_mean")) +
  geom_vline(aes(xintercept=q_surv[1,1],linetype="OM_BS_q",color="OM_BS_q")) + #OM q values for LL survey
  geom_vline(aes(xintercept=q_surv[1,2],linetype="OM_AI_q",color="OM_AI_q")) + #OM q values for LL survey
  geom_vline(aes(xintercept=q_surv[1,3],linetype="OM_WG_q",color="OM_WG_q")) + #OM q values for LL survey
  geom_vline(aes(xintercept=q_surv[1,4],linetype="OM_CG_q",color="OM_CG_q")) + #OM q values for LL survey
  geom_vline(aes(xintercept=q_surv[1,5],linetype="OM_WY_q",color="OM_WY_q")) + #OM q values for LL survey
  geom_vline(aes(xintercept=q_surv[1,6],linetype="OM_EY_q",color="OM_EY_q")) + #OM q values for LL survey
  ggtitle("Catchability - US LL survey") + 
  scale_linetype_manual(name = "", values = c(EM_median = "solid", EM_mean = "dashed",
                                              OM_BS_q="solid",OM_AI_q="dashed",
                                              OM_WG_q="dotted",OM_CG_q="dotdash",
                                              OM_WY_q="longdash",OM_EY_q="twodash")) +
  scale_color_manual(name = "", values = c(EM_median="red",EM_mean="red",
                                           OM_BS_q="blue",OM_AI_q="blue",
                                           OM_WG_q="blue",OM_CG_q="blue",
                                           OM_WY_q="blue",OM_EY_q="blue")) 

EM_q.USJPsurv[forproj.styr:y,] #not used in the forward projecting OM sim anywhere, but still estimated in the EM
colnames(EM_q.USJPsurv) <- c(1:n.sims)
rownames(EM_q.USJPsurv) <- c(1:n.year)
as.data.frame(EM_q.USJPsurv)
EM_q.USJPsurv[forproj.styr:y,]
EMQ.USJPSmean <-vector()
(EMQ.USJPSmean <- apply(EM_q.USJPsurv[forproj.styr:y,],2,mean)) #mean of each sim across years
EMQ.USJPSmedian <-vector()
(EMQ.USJPSmedian <- apply(EM_q.USJPsurv[forproj.styr:y,],2,median)) #median of each sim across years
qmean <- mean(EM_q.USJPsurv[forproj.styr:y,])
qmed <- median(EM_q.USJPsurv[forproj.styr:y,])
melted_EMqUSJPsrv <- melt(EM_q.USJPsurv[forproj.styr:y,],na.rm=FALSE,value.name="Catchability") #=c("year","sim")) how do I name var1 var2?

ggplot() + 
  geom_histogram(data=melted_EMqUSJPsrv,aes(Catchability),binwidth=1/10) + 
  geom_vline(aes(xintercept=qmed,color="EM_median",linetype="EM_median")) + 
  geom_vline(aes(xintercept=qmean, color="EM_mean", linetype="EM_mean")) +
  geom_vline(aes(xintercept=q_surv[2,1],linetype="OM_BS_q",color="OM_BS_q")) + #OM q values for LL survey
  geom_vline(aes(xintercept=q_surv[2,2],linetype="OM_AI_q",color="OM_AI_q")) + #OM q values for LL survey
  geom_vline(aes(xintercept=q_surv[2,3],linetype="OM_WG_q",color="OM_WG_q")) + #OM q values for LL survey
  geom_vline(aes(xintercept=q_surv[2,4],linetype="OM_CG_q",color="OM_CG_q")) + #OM q values for LL survey
  geom_vline(aes(xintercept=q_surv[2,5],linetype="OM_WY_q",color="OM_WY_q")) + #OM q values for LL survey
  geom_vline(aes(xintercept=q_surv[2,6],linetype="OM_EY_q",color="OM_EY_q")) + #OM q values for LL survey
  ggtitle("Catchability - USJP LL survey") + 
  scale_linetype_manual(name = "", values = c(EM_median = "solid", EM_mean = "dashed",
                                              OM_BS_q="solid",OM_AI_q="dashed",
                                              OM_WG_q="dotted",OM_CG_q="dotdash",
                                              OM_WY_q="longdash",OM_EY_q="twodash")) +
  scale_color_manual(name = "", values = c(EM_median="red",EM_mean="red",
                                           OM_BS_q="blue",OM_AI_q="blue",
                                           OM_WG_q="blue",OM_CG_q="blue",
                                           OM_WY_q="blue",OM_EY_q="blue")) 

EM_q.postifqfish[forproj.styr:y,]
colnames(EM_q.postifqfish) <- c(1:n.sims)
rownames(EM_q.postifqfish) <- c(1:n.year)
as.data.frame(EM_q.postifqfish)
EM_q.postifqfish[forproj.styr:y,]
EMQ.postIFQmean <-vector()
(EMQ.postIFQmean <- apply(EM_q.postifqfish[forproj.styr:y,],2,mean)) #mean of each sim across years
EMQ.postIFQmedian <-vector()
(EMQ.postIFQmedian <- apply(EM_q.postifqfish[forproj.styr:y,],2,median)) #median of each sim across years
qmean <- mean(EM_q.postifqfish[forproj.styr:y,])
qmed <- median(EM_q.postifqfish[forproj.styr:y,])
melted_EMqpostIFQ <- melt(EM_q.postifqfish[forproj.styr:y,],na.rm=FALSE,value.name="Catchability") #=c("year","sim")) how do I name var1 var2?

ggplot() + 
  geom_histogram(data=melted_EMqpostIFQ,aes(Catchability),binwidth=1/10) + 
  geom_vline(aes(xintercept=qmed,color="EM_median",linetype="EM_median")) + 
  geom_vline(aes(xintercept=qmean, color="EM_mean", linetype="EM_mean")) +
  geom_vline(aes(xintercept=q_fish[2,1],linetype="OM_BS_q",color="OM_BS_q")) + #OM q values for LL survey
  geom_vline(aes(xintercept=q_fish[2,2],linetype="OM_AI_q",color="OM_AI_q")) + #OM q values for LL survey
  geom_vline(aes(xintercept=q_fish[2,3],linetype="OM_WG_q",color="OM_WG_q")) + #OM q values for LL survey
  geom_vline(aes(xintercept=q_fish[2,4],linetype="OM_CG_q",color="OM_CG_q")) + #OM q values for LL survey
  geom_vline(aes(xintercept=q_fish[2,5],linetype="OM_WY_q",color="OM_WY_q")) + #OM q values for LL survey
  geom_vline(aes(xintercept=q_fish[2,6],linetype="OM_EY_q",color="OM_EY_q")) + #OM q values for LL survey
  ggtitle("Catchability - Post-IFQ fishery") + 
  scale_linetype_manual(name = "", values = c(EM_median = "solid", EM_mean = "dashed",
                                              OM_BS_q="solid",OM_AI_q="dashed",
                                              OM_WG_q="dotted",OM_CG_q="dotdash",
                                              OM_WY_q="longdash",OM_EY_q="twodash")) +
  scale_color_manual(name = "", values = c(EM_median="red",EM_mean="red",
                                           OM_BS_q="blue",OM_AI_q="blue",
                                           OM_WG_q="blue",OM_CG_q="blue",
                                           OM_WY_q="blue",OM_EY_q="blue")) 

EM_q.preifqfish[forproj.styr:y,]
colnames(EM_q.preifqfish) <- c(1:n.sims)
rownames(EM_q.preifqfish) <- c(1:n.year)
as.data.frame(EM_q.preifqfish)
EM_q.preifqfish[forproj.styr:y,]
EMQ.preIFQmean <-vector()
(EMQ.preIFQmean <- apply(EM_q.preifqfish[forproj.styr:y,],2,mean)) #mean of each sim across years
EMQ.preIFQmedian <-vector()
(EMQ.preIFQmedian <- apply(EM_q.preifqfish[forproj.styr:y,],2,median)) #median of each sim across years
qmean <- mean(EM_q.preifqfish[forproj.styr:y,])
qmed <- median(EM_q.preifqfish[forproj.styr:y,])
melted_EMqpreIFQ <- melt(EM_q.preifqfish[forproj.styr:y,],na.rm=FALSE,value.name="Catchability") #=c("year","sim")) how do I name var1 var2?

ggplot() + 
  geom_histogram(data=melted_EMqpreIFQ,aes(Catchability),binwidth=1/10) + 
  geom_vline(aes(xintercept=qmed,color="EM_median",linetype="EM_median")) + 
  geom_vline(aes(xintercept=qmean, color="EM_mean", linetype="EM_mean")) +
  geom_vline(aes(xintercept=q_fish[1,1],linetype="OM_BS_q",color="OM_BS_q")) + #OM q values for LL survey
  geom_vline(aes(xintercept=q_fish[1,2],linetype="OM_AI_q",color="OM_AI_q")) + #OM q values for LL survey
  geom_vline(aes(xintercept=q_fish[1,3],linetype="OM_WG_q",color="OM_WG_q")) + #OM q values for LL survey
  geom_vline(aes(xintercept=q_fish[1,4],linetype="OM_CG_q",color="OM_CG_q")) + #OM q values for LL survey
  geom_vline(aes(xintercept=q_fish[1,5],linetype="OM_WY_q",color="OM_WY_q")) + #OM q values for LL survey
  geom_vline(aes(xintercept=q_fish[1,6],linetype="OM_EY_q",color="OM_EY_q")) + #OM q values for LL survey
  ggtitle("Catchability - Pre-IFQ fishery") + 
  scale_linetype_manual(name = "", values = c(EM_median = "solid", EM_mean = "dashed",
                                              OM_BS_q="solid",OM_AI_q="dashed",
                                              OM_WG_q="dotted",OM_CG_q="dotdash",
                                              OM_WY_q="longdash",OM_EY_q="twodash")) +
  scale_color_manual(name = "", values = c(EM_median="red",EM_mean="red",
                                           OM_BS_q="blue",OM_AI_q="blue",
                                           OM_WG_q="blue",OM_CG_q="blue",
                                           OM_WY_q="blue",OM_EY_q="blue")) 





EM_q.forfish[forproj.styr:y,]#not used in the forward projecting OM sim anywhere, but still estimated in the EM
colnames(EM_q.forfish) <- c(1:n.sims)
rownames(EM_q.forfish) <- c(1:n.year)
as.data.frame(EM_q.forfish)
EM_q.forfish[forproj.styr:y,]
EMQ.forfishQmean <-vector()
(EMQ.forfishQmean <- apply(EM_q.forfish[forproj.styr:y,],2,mean)) #mean of each sim across years
EMQ.forfishmedian <-vector()
(EMQ.forfishmedian <- apply(EM_q.forfish[forproj.styr:y,],2,median)) #median of each sim across years
qmean <- mean(EM_q.forfish[forproj.styr:y,])
qmed <- median(EM_q.forfish[forproj.styr:y,])
melted_EMqforfish <- melt(EM_q.forfish[forproj.styr:y,],na.rm=FALSE,value.name="Catchability") #=c("year","sim")) how do I name var1 var2?

ggplot() + 
  geom_histogram(data=melted_EMqforfish,aes(Catchability),binwidth=1/10) + 
  geom_vline(aes(xintercept=qmed,color="EM_median",linetype="EM_median")) + 
  geom_vline(aes(xintercept=qmean, color="EM_mean", linetype="EM_mean")) +
  geom_vline(aes(xintercept=q_fish[4,1],linetype="OM_BS_q",color="OM_BS_q")) + #OM q values for LL survey
  geom_vline(aes(xintercept=q_fish[4,2],linetype="OM_AI_q",color="OM_AI_q")) + #OM q values for LL survey
  geom_vline(aes(xintercept=q_fish[4,3],linetype="OM_WG_q",color="OM_WG_q")) + #OM q values for LL survey
  geom_vline(aes(xintercept=q_fish[4,4],linetype="OM_CG_q",color="OM_CG_q")) + #OM q values for LL survey
  geom_vline(aes(xintercept=q_fish[4,5],linetype="OM_WY_q",color="OM_WY_q")) + #OM q values for LL survey
  geom_vline(aes(xintercept=q_fish[4,6],linetype="OM_EY_q",color="OM_EY_q")) + #OM q values for LL survey
  ggtitle("Catchability - Foreign fishery") + 
  scale_linetype_manual(name = "", values = c(EM_median = "solid", EM_mean = "dashed",
                                              OM_BS_q="solid",OM_AI_q="dashed",
                                              OM_WG_q="dotted",OM_CG_q="dotdash",
                                              OM_WY_q="longdash",OM_EY_q="twodash")) +
  scale_color_manual(name = "", values = c(EM_median="red",EM_mean="red",
                                           OM_BS_q="blue",OM_AI_q="blue",
                                           OM_WG_q="blue",OM_CG_q="blue",
                                           OM_WY_q="blue",OM_EY_q="blue")) 


#fit to indices##
EM_pred.srvRPN #US dom LL survey RPN, starts in 1990/yr 15
melted_EM_pred.srvRPN <- melt(EM_pred.srvRPN,na.rm=TRUE,value.name="EM.RPN") #=c("year","sim")) how do I name var1 var2?
head(melted_EM_pred.srvRPN)
#calc a matrix of medians across sims for EM
EMmed_matrix <- matrix(data=NA, nrow=length(1:n.year),ncol=length(1:n.year)) #across sims
EMmed_med <- vector()
for(y in forproj.styr:n.year) {
  for(n in 15:n.year){
    EMmed_matrix[y,n] <- median(EM_pred.srvRPN[y,n,],na.rm=TRUE) #median pred EM surv RPN for each year, across sims
  }}
for(j in 1:n.year){
EMmed_med[j] <- median(EMmed_matrix[,j],na.rm=TRUE)
}
#calc medians across sims for OM
OMmed <- vector() 
for(n in 15:n.year){
  OMmed[n] <- median(OM_Surv.RPN[n,],na.rm=TRUE) #median pred EM surv RPN for each year, across sims
}

#Plot EM & OM RPN, includes retrospective:
par(mfrow=c(1,1))
plot(EM_pred.srvRPN[forproj.styr,,1]~c(1:n.year),typ="l",ylim=c(0,2000),ylab="Survey RPN",xlab="Year",xlim=c(15,60))
for(y in forproj.styr:n.year){
for(i in 1:n.sims){
lines(EM_pred.srvRPN[y,,i]~c(1:n.year),typ="l")
}} 
  for(i in 1:n.sims){
lines(OM_Surv.RPN[15:n.year,i]~c(15:n.year),typ="l",col="red")
  }

#Plot EM and OM RPN terminal year only, across sims
plot(EM_pred.srvRPN[n.year,15:n.year,1]~c(15:n.year),typ="l",ylim=c(0,2000),ylab="Survey RPN",xlab="Year")
#for(y in forproj.styr:n.year){
  for(i in 1:n.sims){
    lines(EM_pred.srvRPN[n.year,15:n.year,i]~c(15:n.year),typ="l")
  }
for(i in 1:n.sims){
  lines(OM_Surv.RPN[15:n.year,i]~c(15:n.year),typ="l",col="red")
}

#EM with EM median plotted
plot(EM_pred.srvRPN[forproj.styr,,2]~c(1:n.year),typ="l",ylim=c(0,2000),ylab="Survey RPN",xlab="Year",xlim=c(15,60))
for(y in forproj.styr:n.year){
  for(i in 1:n.sims){
    lines(EM_pred.srvRPN[y,,i]~c(1:n.year),typ="l")
  }} 
lines(EMmed_med~c(1:n.year),typ="l",col="red",lwd=5) #median of the medians line

#PUT IT ALL ON ONE
#OM and EM with OM and EM medians
plot(EM_pred.srvRPN[forproj.styr,,2]~c(1:n.year),typ="l",ylim=c(0,2000),ylab="Survey RPN",xlab="Year",xlim=c(15,60))
for(y in forproj.styr:n.year){
  for(i in 1:n.sims){
    lines(EM_pred.srvRPN[y,,i]~c(1:n.year),typ="l",col="black") #all the EM RPNs
  }} 
for(i in 1:n.sims){
  lines(OM_Surv.RPN[15:n.year,i]~c(15:n.year),typ="l",col="red") #all the OM RPNs
}
lines(EMmed_med~c(1:n.year),typ="l",col="orange",lwd=5) #median of the medians line
lines(OMmed~c(1:n.year),typ="l",col="yellow",lwd=5) #median of the medians line


#residuals for terminal year OM vs EM for each sim
OM_Surv.RPN
EM_pred.srvRPN
resid.RPN <- matrix(data=NA, nrow=length(1:n.year),ncol=length(1:n.sims))
for (j in 1:n.year) {
  for(r in 1:n.sims) {
    resid.RPN[j,r] <- OM_Surv.RPN[j,r] - EM_pred.srvRPN[n.year,j,r]
  }
}

##
EM_pred.fishRPW  #US fishery RPW, starts in 1990/yr 15
OM_Fish.RPW
melted_EM_pred.fishRPW <- melt(EM_pred.fishRPW,na.rm=TRUE,value.name="EM.RPW") #=c("year","sim")) how do I name var1 var2?
head(melted_EM_pred.fishRPW)
#calc a matrix of medians across sims for EM
EMmed_matrix <- matrix(data=NA, nrow=length(1:n.year),ncol=length(1:n.year)) #across sims
EMmed_med <- vector()
for(y in forproj.styr:n.year) {
  for(n in 15:n.year){
    EMmed_matrix[y,n] <- median(EM_pred.fishRPW[y,n,],na.rm=TRUE) #median pred EM fishery RPN for each year, across sims
  }}
for(j in 1:n.year){
  EMmed_med[j] <- median(EMmed_matrix[,j],na.rm=TRUE)
}
#calc medians across sims for OM
OMmed <- vector() 
for(n in 15:n.year){
  OMmed[n] <- median(OM_Fish.RPW[n,],na.rm=TRUE) #median pred EM fishery RPN for each year, across sims
}

#Plot EM & OM RPN, includes retrospective:
plot(EM_pred.fishRPW[forproj.styr,,1]~c(1:n.year),typ="l",ylim=c(0,3000),ylab="Fishery RPW",xlab="Year",xlim=c(15,60))
for(y in forproj.styr:n.year){
  for(i in 1:n.sims){
    lines(EM_pred.fishRPW[y,,i]~c(1:n.year),typ="l")
  }} 
for(i in 1:n.sims){
  lines(OM_Fish.RPW[15:n.year,i]~c(15:n.year),typ="l",col="red")
}

#Plot EM and OM RPN terminal year only, across sims
plot(EM_pred.fishRPW[n.year,15:n.year,1]~c(15:n.year),typ="l",ylim=c(0,3000),ylab="Fishery RPN",xlab="Year")
#for(y in forproj.styr:n.year){
for(i in 1:n.sims){
  lines(EM_pred.fishRPW[n.year,15:n.year,i]~c(15:n.year),typ="l")
}
for(i in 1:n.sims){
  lines(OM_Fish.RPW[15:n.year,i]~c(15:n.year),typ="l",col="red")
}

#EM with EM median plotted
plot(EM_pred.fishRPW[forproj.styr,,2]~c(1:n.year),typ="l",ylim=c(0,3000),ylab="Fishery RPW",xlab="Year",xlim=c(15,60))
for(y in forproj.styr:n.year){
  for(i in 1:n.sims){
    lines(EM_pred.fishRPW[y,,i]~c(1:n.year),typ="l")
  }} 
lines(EMmed_med~c(1:n.year),typ="l",col="red",lwd=5) #median of the medians line

#PUT IT ALL ON ONE
#OM and EM with OM and EM medians
plot(EM_pred.fishRPW[forproj.styr,,2]~c(1:n.year),typ="l",ylim=c(0,3000),ylab="Fishery RPW",xlab="Year",xlim=c(15,60))
for(y in forproj.styr:n.year){
  for(i in 1:n.sims){
    lines(EM_pred.fishRPW[y,,i]~c(1:n.year),typ="l",col="black") #all the EM RPNs
  }} 
for(i in 1:n.sims){
  lines(OM_Fish.RPW[15:n.year,i]~c(15:n.year),typ="l",col="red") #all the OM RPNs
}
lines(EMmed_med~c(1:n.year),typ="l",col="orange",lwd=5) #median of the medians line
lines(OMmed~c(1:n.year),typ="l",col="yellow",lwd=5) #median of the medians line


#residuals for terminal year OM vs EM for each sim
OM_Fish.RPW
EM_pred.fishRPW
resid.RPW <- matrix(data=NA, nrow=length(1:n.year),ncol=length(1:n.sims))
for (j in 1:n.year) {
  for(r in 1:n.sims) {
    resid.RPW[j,r] <- OM_Fish.RPW[j,r] - EM_pred.fishRPW[n.year,j,r]
  }
}


#recruitment##
EM_predrec[,,1] #EM recruitment estimates (dim - year, year, sim) year 1 is 1977 for EM?
rec_mill <- rec/1000000
rec #OM recruitment
#calc a matrix of medians across sims
EMmed_matrix <- matrix(data=NA, nrow=length(1:n.year),ncol=length(1:n.year)) #across sims
EMmed_med <- vector()
for(y in forproj.styr:n.year) {
  for(n in 15:n.year){
    EMmed_matrix[y,n] <- median(EM_predrec[y,n,],na.rm=TRUE) #median pred EM surv RPN for each year, across sims
  }}
for(j in 1:n.year){
  EMmed_med[j] <- median(EMmed_matrix[,j],na.rm=TRUE)
}
#calc medians across sims for OM
OMmed <- vector() 
for(n in 15:n.year){
  OMmed[n] <- median(rec_mill[,n],na.rm=TRUE) #median pred EM surv RPN for each year, across sims
}

#Plot EM & OM recruitment, includes retrospective:
plot(EM_predrec[forproj.styr,1:n.year,1]~c(1:n.year),typ="l",ylim=c(0,100),ylab="Recruitment (millions)",xlab="Year",xlim=c(1,60))
for(y in forproj.styr:n.year){
  for(i in 1:n.sims){
    lines(EM_predrec[y,1:n.year,i]~c(1:n.year),typ="l")
  }} 
for(i in 1:n.sims){
  lines(rec_mill[i,1:n.year]~c(1:n.year),typ="l",col="red") #OM generated recruitment (not used in years 1-43 for EM)
}
#Plot EM and OM recruitment terminal year only, across sims
plot(EM_predrec[n.year,1:n.year,1]~c(1:n.year),typ="l",ylim=c(0,100),ylab="Recruitment (millions)",xlab="Year")
for(i in 1:n.sims){
  lines(EM_predrec[n.year,1:n.year,i]~c(1:n.year),typ="l") #this is the EM estimated recruitment from the final year EM run
}
for(i in 1:n.sims){
  lines(rec_mill[i,44:n.year]~c(44:n.year),typ="l",col="red") #this is the OM generated recruitment
}
lines(cond.rec$Recruitment~c(2:43),typ="l",col="blue",lwd=3) #this is the summed recruitment input into the OM 

#EM with EM median plotted
plot(EM_predrec[forproj.styr,,2]~c(1:n.year),typ="l",ylim=c(0,100),ylab="Recruitment (millions)",xlab="Year",xlim=c(15,60))
for(y in forproj.styr:n.year){
  for(i in 1:n.sims){
    lines(EM_predrec[y,,i]~c(1:n.year),typ="l")
  }} 
lines(EMmed_med~c(1:n.year),typ="l",col="red",lwd=5) #median of the medians line
#PUT IT ALL ON ONE
#OM and EM with OM and EM medians
plot(EM_predrec[forproj.styr,,2]~c(1:n.year),typ="l",ylim=c(0,100),ylab="Recruitment (millions)",xlab="Year",xlim=c(15,60))
for(y in forproj.styr:n.year){
  for(i in 1:n.sims){
    lines(EM_predrec[y,,i]~c(1:n.year),typ="l",col="black") #all the EM RPNs
  }} 
for(i in 1:n.sims){
  lines(rec_mill[i,44:n.year]~c(44:n.year),typ="l",col="red") #all the OM RPNs
}
lines(EMmed_med~c(1:n.year),typ="l",col="orange",lwd=5) #median of the medians line
lines(OMmed[44:n.year]~c(44:n.year),typ="l",col="yellow",lwd=5) #median of the medians line


#residuals for terminal year OM vs EM for each sim
resid.rec <- matrix(data=NA, nrow=length(1:n.year),ncol=length(1:n.sims))
for (j in 1:n.year) {
  for(r in 1:n.sims) {
    resid.rec[j,r] <- rec_mill[r,j] - EM_predrec[n.year,j,r]
  }
}




#fit to age comps##
age_likelihood[,forproj.styr:y,] #likelihoods table




##### Performance metrics things
################################

#SSB##
ssb[,a,y,m,i] #OM ssb
EM_spbiom[y,2:y,i] #EM ssb
#calc a matrix of medians across sims for EM
EMmed_matrix <- matrix(data=NA, nrow=length(1:n.year),ncol=length(1:n.year)) #across sims
EMmed_med <- vector()
EMmean_matrix <- matrix(data=NA, nrow=length(1:n.year),ncol=length(1:n.year))
EMmean_mean <- vector()

for(y in forproj.styr:n.year) {
  for(n in 1:n.year){
    EMmed_matrix[y,n] <- median(EM_spbiom[y,n,],na.rm=TRUE) #median pred EM fishery RPN for each year, across sims
    EMmean_matrix[y,n] <- mean(EM_spbiom[y,n,],na.rm=TRUE)
    }}
for(j in 1:n.year){
  EMmed_med[j] <- median(EMmed_matrix[,j],na.rm=TRUE)
  EMmean_mean[j] <- mean(EMmean_matrix[,j],na.rm=TRUE)
}
#EM standard devs matrix, upper and lower ~95% CIs using mean of means
EM_sd_matrix <- matrix(data=NA, nrow=length(1:n.year),ncol=length(1:n.year)) #across sims
EM_sd_mean <- vector()
for(y in forproj.styr:n.year) {
  for(n in 1:n.year){
    EM_sd_matrix[y,n] <- sd(EM_spbiom[y,n,],na.rm=TRUE) #median pred EM fishery RPN for each year, across sims
  }}
for(j in 1:n.year){
  EM_sd_mean[j] <- mean(EM_sd_matrix[,j],na.rm=TRUE) #mean of the SDs
}
UCI_EM <- vector()
LCI_EM <- vector()
UCI_EM <- EMmean_mean + (2*EM_sd_mean)
LCI_EM <- EMmean_mean - (2*EM_sd_mean)  
#calc medians and means across sims for OM
ssb_summed <- matrix(data=NA,nrow=length(1:n.year), ncol=length(1:n.sims))#ssb summed over ages, areas, sexes (so leaves a year x sim matrix)
ssb_summed<- apply(ssb,c(3,5),sum)
OMmed <- vector() 
OMmean <- vector()
for(n in 1:n.year){
  OMmed[n] <- median(ssb_summed[n,],na.rm=TRUE) #median pred EM fishery RPN for each year, across sims
  OMmean[n] <- mean(ssb_summed[n,],na.rm=TRUE) #median pred EM fishery RPN for each year, across sims
  }
#OM standard devs matrix
OM_sd_vector <- vector() #across sims
  for(n in 1:n.year){
    OM_sd_vector[n] <- sd(ssb_summed[n,],na.rm=TRUE) #median pred EM fishery RPN for each year, across sims
  }
#PUT IT ALL ON ONE
#OM and EM with OM and EM medians
plot(EM_spbiom[forproj.styr,,2]~c(1:n.year),typ="l",ylim=c(0,500),ylab="SSB",xlab="Year",xlim=c(1,60))
for(y in forproj.styr:n.year){
  for(i in 1:n.sims){
    lines(EM_spbiom[y,,i]~c(1:n.year),typ="l",col="black") #all the EM RPNs
  }} 
for(i in 1:n.sims){
  lines(ssb_summed[,i]~c(1:n.year),typ="l",col="red") #all the OM RPNs
}
lines(EMmed_med~c(1:n.year),typ="l",col="orange",lwd=5) #median of the medians line
lines(OMmed~c(1:n.year),typ="l",col="yellow",lwd=5) #median of the medians line
#CI plot
plot(EMmed_med~c(1:n.year),typ="l",col="black",lwd=5,ylim=c(0,500),ylab="SSB",xlab="Year",xlim=c(1,60)) #median of EM
lines(UCI_EM~c(1:n.year),typ="l",col="black",lwd=1) #95% CI of mean
lines(LCI_EM~c(1:n.year),typ="l",col="black",lwd=1) #95% CI of mean
lines(OMmed~c(1:n.year),typ="l",col="red",lwd=5) #OM median of the medians line

#compare terminal EM year estimates to previous year estimates (EM)
plot(EM_spbiom[n.year,,1]~c(1:n.year),typ="l",ylim=c(0,500),ylab="SSB",xlab="Year",xlim=c(1,60))
for(y in forproj.styr:n.year-1){
  for(i in 1:n.sims){
    lines(EM_spbiom[y,,i]~c(1:n.year),typ="l",col="red") #all the EM RPNs
  }} 
  for(i in 1:n.sims){
    lines(EM_spbiom[n.year,,i]~c(1:n.year),typ="l",col="black") #all the EM RPNs
  } 

#ssb residuals
resid.ssb <- matrix(data=NA, nrow=length(1:n.year),ncol=length(1:n.sims))
for (j in 1:n.year) {
  for(r in 1:n.sims) {
    resid.ssb[j,r] <- ssb_summed[j,r] - EM_spbiom[n.year,j,r]
  }
}
#percent diff
resid.ssb2 <- matrix(data=NA, nrow=length(1:n.year),ncol=length(1:n.sims))
for (j in 1:n.year) {
  for(r in 1:n.sims) {
    resid.ssb[j,r] <- (ssb_summed[j,r] - EM_spbiom[n.year,j,r])/((ssb_summed[j,r]+EM_spbiom[n.year,j,r])/2)*100
  }
}

#OM true N by area proportion vs EM apportioned TAC by area for terminal year EMs and OMs
apportioned_C
EM_TAC_prop <- array(data=NA,dim=c(n.year,n.area,n.sims),dimnames=list(years,areas,sims))
EM_TAC_prop <- apply(apportioned_C,c(1,3,4),sum)
EM_TAC_prop2 <- prop.table(EM_TAC_prop,c(1,3)) #this is the EM proportions of TAC by area for each year and sim.
OM_true_prop <- array(data=NA,dim=c(n.year,n.area,n.sims),dimnames=list(years,areas,sims))
OM_true_prop <- apply(N,c(2,4,5),sum)
OM_true_prop2 <- prop.table(OM_true_prop,c(1,3))

rpd_ssb <- ((EM_TAC_prop2-OM_true_prop2)/OM_true_prop2)*100
mean_rpd_ssb<-matrix(data=NA,nrow=length(1:n.year),ncol=length(1:n.area) )
for(y in 1:n.year) {
  for(m in 1:n.area){
mean_rpd_ssb[y,m] <- mean(abs(rpd_ssb[y,m,])) #mean absolute RPD
}}
#median_rpd_ssb
boxplot(mean_rpd_ssb,main="absolute RPD between OM and EM proportion SSB, by area", ylab="Mean RPD",xlab="Area") #mean relative percent difference between apportioned TAC and OM spatial abundance by area

#OM true N vs EM apportioned TAC for areas combined
ssb_summed[44:n.year,] #OM ssb summed across areas, ages, sexes
EM_spbiom[n.year,44:y,] #EM ssb terminal year (n.year) run only
rpd_ssb2 <- ((EM_spbiom[n.year,44:y,]-ssb_summed[44:n.year,])/ssb_summed[44:n.year,])*100
melted_rpd_ssb2 <- melt(rpd_ssb2,na.rm=TRUE,value.name="RPD") 
head(melted_rpd_ssb2)

boxplot(melted_rpd_ssb2[,3])
boxplot(abs(melted_rpd_ssb2[,3]))

#catch##
#sum to year and sim matrices
apportioned_C #OM apportioned catch; years, gears, areas, sims
sum_appC <- matrix(data=NA,nrow=length(1:n.year),ncol=length(1:n.sims))
sum_appC <- apply(apportioned_C,c(1,4),sum)

OM_fixed_catch  #2 OM - US fixed post-IFQ, based on harvest
OM_trawl_catch  #3 OM - US trawl, based on harvest
sum_OM_catch <- matrix(data=NA,nrow=length(1:n.year),ncol=length(1:n.sims))
for(y in 1:n.year){
  for(i in 1:n.sims) {
  sum_OM_catch[y,i] <- sum(OM_fixed_catch[y,i],OM_trawl_catch[y,i])
  }}

C.n # OM catch in numbers, dim= sex,year,age,area,sim
sum_C.n <- matrix(data=NA,nrow=length(1:n.year),ncol=length(1:n.sims))
sum_C.n <- apply(C.n,c(2,5),sum)

C.b # OM catch in biomass
sum_C.b <- matrix(data=NA,nrow=length(1:n.year),ncol=length(1:n.sims))
sum_C.b <- apply(C.b,c(2,5),sum)

EM_predcatch_fixedgear
EM_predcatch_trawlgear
sum_EM_predcatch <- matrix(data=NA,nrow=c(length(1:n.year)),ncol=c(length(1:n.sims)),dimnames=list(years,sims)) #summed over gears
for(y in 1:n.year){
  for(i in 1:n.sims) {
    sum_EM_predcatch[y,i] <- sum(EM_predcatch_fixedgear[n.year,y,i],EM_predcatch_trawlgear[n.year,y,i])
  }}

plot(sum_appC[,1]~c(1:n.year),typ="l",lwd=3)
lines(sum_OM_catch[,1]~c(1:n.year),typ="p",col="red",lwd=3)
lines(sum_C.b[,1]~c(1:n.year),typ="l",col="blue",lwd=3)
lines(sum_EM_predcatch[,1]~c(1:n.year),typ="l",col="green",lwd=3)


plot(sum_appC[,1]~c(1:n.year),typ="l")
for(i in 1:n.sims){
  lines(sum_appC[,i]~c(1:n.year),typ="l")
}


#median apportioned (+- ~95% CIS of mean) TAC by area
apportioned_C  #dim:  years, gears, areas, sims
#sum over gears for total app.catch by area, year, sim
apportioned_C_sum <- array(data=NA,dim=c(n.year,n.area,n.sims),dimnames=list(years,areas,sims))
apportioned_C_sum <- apply(apportioned_C,c(1,3,4),sum)
#find the median apportioned catch by area for each final EM run (year n.year) across sims
med_appC <-  vector()#matrix(data=NA,nrow=c(length(1:n.year)),ncol=c(length(1:n.sims)),dimnames=list(years,sims))
#for(y in 1:n.year) {
for(m in 1:n.area) {
  med_appC[m] <- median(apportioned_C_sum[n.year,m,])
}#}

melted_appC <- melt(apportioned_C_sum[,,],na.rm=TRUE,value.name="TAC")  #
ggplot(melted_appC,aes(x=Var1,y=TAC,group=Var1))+
  geom_boxplot()

#median apportioned (+- ~95% CIS) TAC for all areas summed
apportioned_C_sum2 <- matrix(data=NA,nrow=c(length(1:n.year)),ncol=c(length(1:n.sims)),dimnames=list(years,sims))
apportioned_C_sum2 <- apply(apportioned_C_sum,c(1,3),sum)
melted_appC2 <- melt(apportioned_C_sum2[n.year,],na.rm=FALSE,value.name="TAC")  #terminal year only
median(melted_appC2[,1])
boxplot(melted_appC2[,1])

#median EM catch (+- ~95% CIs of mean) catch for all areas summed
sum_EM_predcatch
melted_sum_EMpredcatch <- melt(sum_EM_predcatch[n.year,],na.rm=FALSE,value.name="EMcatch")
median(melted_sum_EMpredcatch$EMcatch)
boxplot(melted_sum_EMpredcatch)

#proportion of years (terminal year of EM runs) and sims where apportioned TAC falls below threshold
threshold <- 2.0 #thousand mt
apportioned_C_sum 
app_num <- array(data=NA,dim=c(n.year,n.area,n.sims),dimnames=list(years,areas,sims))
for(i in 1:n.sims){
  for(m in 1:n.area){
    for(y in forproj.styr:n.year){
      if(apportioned_C_sum[y,m,i] < threshold) { #0 if below threshold
        app_num[y,m,i] <- 0      
      } else {
      app_num[y,m,i] <- 1        #if above threshold
      }
    }
  }
}
#proportion years in each area above threshold across all years and sims 
temp_a <- vector() 
temp_a <- apply(app_num[forproj.styr:n.year,,],2,sum)
(temp_a1 <- temp_a/(n.sims*(length(forproj.styr:n.year))) ) 




#Proportion years/sims where of the catch in area x is more than 50% (or a changeable value) fish younger than the age @50% mature
#test.age <- 4

#Mean, median (+CIs) of age and length (converted from age?) for catches (apportioned TAC) in each EM area across years and sims

#ABC stability
#Median (across sims) change in TAC to each area from year to year (by area and for areas combined)
#by area
apportioned_C_sum[forproj.styr:n.year,,1]
diff_apport <- array(data=NA,dim=c(length(1:n.year),n.area,n.sims),dimnames=list(1:n.year,1:n.area,1:n.sims))
for(i in 1:n.sims){
  for(m in 1:n.area){
  for(y in 1:(n.year-1)){
    diff_apport[y+1,m,i] <- (abs(apportioned_C_sum[y,m,i]-apportioned_C_sum[y+1,m,i])/(abs(apportioned_C_sum[y,m,i]+apportioned_C_sum[y+1,m,i])/2))*100
  }}}
diff_apport[(forproj.styr):n.year,,]
diff_apport_mean <- vector()
diff_apport_med <- vector()
for(m in 1:n.area){
  diff_apport_mean[m] <- mean(diff_apport[(forproj.styr):n.year,m,]) #may need to change years over which mean and median are taken
  diff_apport_med[m] <- median(diff_apport[(forproj.styr):n.year,m,])
}
diff_apport[,6,]
#areas combined
apportioned_C_sum2
diff_apport <- matrix(data=NA,nrow=length(1:n.year),ncol=n.sims,dimnames=list(1:n.year,1:n.sims))
for(i in 1:n.sims){
for(y in 1:(n.year-1)){
  diff_apport[y+1,i] <- (abs(apportioned_C_sum2[y,i]-apportioned_C_sum2[y+1,i])/(abs(apportioned_C_sum2[y,i]+apportioned_C_sum2[y+1,i])/2))*100
  }}
diff_apport[(forproj.styr):n.year,]
mean(diff_apport[(forproj.styr):n.year,]) #may need to change years over which mean and median are taken
median(diff_apport[(forproj.styr):n.year,])

#Catch at age * price @ age/size by area and for all areas (OM) and compare to EM catch at age * price







#yield##

#Bx%##
#actual vs predicted
EM_B40[forproj.styr:n.year,]
ssb_summed
#OM biomass by area, year, sim
B_summed <- array(data=NA,dim=c(n.year,n.area,n.sims),dimnames=list(years,areas,c(1:n.sims)))
B_summed <- apply(B,c(2,4,5),sum) #biomass by area, year, sim
OM_B_mean <- matrix(data=NA, nrow=length(1:n.year),ncol=length(1:n.sims))
OM_B_mean <- apply(B_summed,c(1:2),mean) #mean across sims for each year and area
OM_B_med <- matrix(data=NA, nrow=length(1:n.year),ncol=length(1:n.sims))
OM_B_med <- apply(B_summed,c(1:2),median) #median across sims for each year and area
#OM biomass by year, sim 
B_summed2 <- matrix(data=NA, nrow=length(1:n.year),ncol=length(1:n.sims))
B_summed2 <- apply(B,c(2,5),sum) #biomass by year, sim 
OM_B_mean2 <- vector()
OM_B_mean2 <- apply(B_summed2,1,mean) #mean across sims for each year
OM_B_med2 <- vector()
OM_B_med2 <- apply(B_summed2,1,median)

plot(OM_B_med2~years,typ="l",lwd=1,col="black")#,ylim=c(0,500))
lines(OM_B_mean2~years,typ="l",lwd=3,col="red")

#proportion of years below B/B40 EM-EM
EM_B40
EM_spbiom[n.year,,]
EM_bratio <- matrix(data=NA,nrow=length(1:n.year),ncol=length(1:n.sims))
for(i in 1:n.sims){
  for(y in 1:n.year){
EM_bratio[y,i] <- EM_spbiom[n.year,y,i]/EM_B40[y,i]
}}
#prop of years overfished (across years and sims)
#mean ratio across years and sims
#plot b/b40 for all sims over time w horiz line for 1



#proportion of years were B/B40>1 but in that year true B/B40 is <1

EM_B40[y,i] <- get_ABC$B40
EM_SBF40[y,i] <- get_ABC$SBF40
EM_SBF35[y,i] <- get_ABC$SBF35
#EM_SBF0 <- get_ABC$SBF0

ABC_projection[y,i] <- get_ABC$ABC_proj[1]
EM_depletion1[y,i] <- get_ABC$Depletion
EM_depletion2[y,i] <- (apply(ssb[,,,,i],3,sum)[y])/EM_B40[y,i]  #calculated quantity EM endyr spawnbiom / B40
EM_spbiom[y,2:y,i] <- get_ABC$spawn_biom #rows are OM year loops, cols are years within an OM loop, 3rd dimension is sim



#Fx%##


## mean and median age at harvest from EM (all areas combined) (and roughly mapped to size)
## mean and median catch/ABC for each area and overall


#Create objects with N.apportionment dimensions
#load("C:/Repositories/Sablefish_ApportionmentStrategies/admb/Single_area/apportionment1.RData")
#gather all the apportionment-type specfic output here and save into objects with an apportionment dimension added. 
#Save those objects so we can add 
#to them in other workspaces.
#call those objects back in in the rmd file