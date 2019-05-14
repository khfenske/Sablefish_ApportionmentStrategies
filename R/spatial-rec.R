#' Spatial Recruitment Simulator
#'
#' @param input.rec Single GOA-BSAI recruitment value for a year - after process error rec. dev. added
#' @param area.props Vector of length equal to number of areas, specifying relative contribution. Will be standardized to proportions.
#' @param ss Multinomial sample size - controls level of variability in proportions around area.props
#' @param seed Seed for setting random multinomial draw
#'
#' @return Vector of recruitment by area, with random apportionment around specififed area.props
spatial_rec <- function(input.rec, area.props, ss, seed) {
  #spatial_rec <- function(input.rec, area.props=c(1,2,3,1,1,1), ss=100, seed=1) {  #why are values specified in this line for area.props and ss and seed? Can't they be called in the actual application of the function?
    
    set.seed(seed)
  #Standardize Area Proportions
  std.area.props <- area.props/sum(area.props)
  #Multinomial Sample
  obs.prop <- rmultinom(1, size=ss, prob=std.area.props)[,1]
  std.obs.prop <- obs.prop/sum(obs.prop)
  #Calculate recruitment by area
  rec.area <- input.rec * std.obs.prop
  
  return(rec.area)
}

# TESTING ============================
# Uncomment me and run multiple times to see how input ss impacts variation in rec. apportionment

# input.rec <- 1e3
# area.props <- c(1,2,3,1,1,1)
# seed <- 1
# 
# 
# #True Proportions
# true.props <- area.props/sum(area.props) * input.rec
# 
# #Trials at different sample sizes ss
# trial.ss <- c(5,10,25,50,100,1000)
# n.trial.ss <- length(trial.ss)
# 
# #Loop through and generate recruitments
# trial.output <- array(dim=c(n.trial.ss, length(area.props)), dimnames=list(trial.ss, c(1:length(area.props))))
# t <- 1
# for(t in 1:n.trial.ss) {
#   trial.output[t,] <- spatial_rec(input.rec=input.rec, area.props=area.props, ss=trial.ss[t], seed=sample(1:100, 1))
# }
# #Add true proportions
# trial.output.2 <- rbind(true.props, trial.output)
# 
# barplot(t(trial.output.2), col=rainbow(length(area.props)),
#           xlab='Input Multinomial Sample Size (ss argument)')
# 
