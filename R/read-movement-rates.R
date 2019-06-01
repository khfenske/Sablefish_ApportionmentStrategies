#' Read in movement probabilities, convert to a 3D array, and standardize to 1 within: area from and age
#'
#' @param input.file String naming the input .xlsx file
#'
#' @return 3D array of movement rates [area from, area to, age]
#' @export
#'
#' @examples
read_movement_rates <- function(input.file="Sablefish_Input.xlsx") {
  ### TESTING ###
  # input.file="Sablefish_Input.xlsx"
  
  # Read in data
  dat.move <- read.xlsx(file=file.path("data", input.file), sheetName='Movement', stringsAsFactors=FALSE)
  head(dat.move)
  
  # Get lookup table at the side
  move.lookup <- data.frame(na.omit(dat.move[-1,7:8]))
  names(move.lookup) <- c('area','number')
  # Convert number back to numeric
  move.lookup$number <- as.numeric(move.lookup$number)
  
  # Create vector of area names
  area.names <- vector(length=n.area)
  for(i in 1:n.area) {
    area.names[i] <- move.lookup$area[move.lookup$number==i]
  }
  
  # Define Output Object
  prob.move <- array(data=NA, dim=c(n.area,n.area,n.age), dimnames=list(area.names,area.names,ages))
  
  # Remove the side sheet
  dat.move2 <- dat.move[,1:6]
  
  # Populate movement array
  a <- 1
  for(a in 1:n.age) {
    # print(a)
    from <- 1
    for(from in 1:n.area) {
      to <- 1
      for(to in 1:n.area) {
        prob.move[from,to,a] <- dat.move2$Pred[dat.move2$From==area.names[from] & #Use Pred
                                               dat.move2$To==area.names[to] &
                                               dat.move2$Age==ages[a]]
      } #to
      #Standardize to 1
      prob.move[from,,a] <- prob.move[from,,a]/sum(prob.move[from,,a])
    } # from
  } # age
  
  #Ensure all rows sum to 1.0
  # apply(prob.move, c(1,3), sum)
  
  # Return Matrix of Movement Probabilities
  return(prob.move)
}