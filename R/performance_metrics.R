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
for(j in 1:60){
EMmed_med[j] <- median(EMmed_matrix[,j],na.rm=TRUE)
}
#calc medians across sims for OM
OMmed <- vector() 
for(n in 15:n.year){
  OMmed[n] <- median(OM_Surv.RPN[n,],na.rm=TRUE) #median pred EM surv RPN for each year, across sims
}

#Plot EM & OM RPN, includes retrospective:
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
for(j in 1:60){
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
med_matrix <- matrix(data=NA, nrow=length(1:n.year),ncol=length(1:n.year)) #across sims
med_med <- vector()
for(y in forproj.styr:n.year) {
  for(n in 15:n.year){
    med_matrix[y,n] <- median(EM_pred.fishRPW[y,n,],na.rm=TRUE) #median pred EM surv RPN for each year, across sims
  }}
for(j in 1:60){
  med_med[j] <- median(med_matrix[,j],na.rm=TRUE)
}

plot(EM_predrec[forproj.styr,1:n.year,1]~c(1:n.year),typ="l",ylim=c(0,100),ylab="Recruitment (millions)",xlab="Year",xlim=c(1,60))
for(y in forproj.styr:n.year){
  for(i in 1:n.sims){
    lines(EM_predrec[y,1:n.year,i]~c(1:n.year),typ="l")
  }} 
for(i in 1:n.sims){
  lines(rec_mill[i,1:n.year]~c(1:n.year),typ="l",col="red") #OM generated recruitment (not used in years 1-43 for EM)
}

#terminal year only, across sims
plot(EM_predrec[n.year,1:n.year,1]~c(1:n.year),typ="l",ylim=c(0,100),ylab="Recruitment (millions)",xlab="Year")
#for(y in forproj.styr:n.year){
for(i in 1:n.sims){
  lines(EM_predrec[n.year,1:n.year,i]~c(1:n.year),typ="l") #this is the EM estimated recruitment from the final year EM run
}
for(i in 1:n.sims){
  lines(rec_mill[i,44:n.year]~c(44:n.year),typ="l",col="red") #this is the OM generated recruitment
}
lines(cond.rec$Recruitment~c(2:43),typ="l",col="blue",lwd=3) #this is the summed recruitment input into the OM 



plot(EM_pred.fishRPW[forproj.styr,,2]~c(1:n.year),typ="l",ylim=c(0,3000),ylab="Fishery RPW",xlab="Year",xlim=c(15,60))
for(y in forproj.styr:n.year){
  for(i in 1:n.sims){
    lines(EM_pred.fishRPW[y,,i]~c(1:n.year),typ="l")
  }} 
lines(med_med~c(1:n.year),typ="l",col="red",lwd=5) #median of the medians line

#residuals for terminal year OM vs EM for each sim
OM_Fish.RPW
EM_pred.fishRPW
resid.RPW <- matrix(data=NA, nrow=length(1:n.year),ncol=length(1:n.sims))
for (j in 1:n.year) {
  for(r in 1:n.sims) {
    resid.RPW[j,r] <- OM_Fish.RPW[j,r] - EM_pred.fishRPW[n.year,j,r]
  }
}












#fit to age comps##
age_likelihood[,forproj.styr:y,] #likelihoods table




##### Performance metrics things
################################

#SSB##

#catch##
obs vs pred


#yield##

#Bx%##

#Fx%##




