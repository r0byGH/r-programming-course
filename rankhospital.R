rankhospital <- function(state, outcome, ranking) {
  
  outcomes <- c(11,17,23)
  names(outcomes) <- c("heart attack", "heart failure", "pneumonia")
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  
  ## Check that state and outcome are valid
  if (!(state %in% data[["State"]]))  stop("invalid state")
  if (!(outcome %in% names(outcomes))) stop("invalid outcome")
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  # col2 contains names, 11, 17, 23 data for the 3 deseases values
  ##
  
  ncol <- 2
  dcol <- outcomes[[outcome]]
  
  data[,dcol] <- suppressWarnings(as.numeric(data[,dcol]))
  
  ##extract only relevant data: names, rate
  hospitals <- data[data$State == state, c(ncol, dcol)]
  
  #order by rate, name and exclude NAs
  hospitals <-  hospitals[order(hospitals[,2], hospitals[,1], na.last = NA),]

  if (ranking == "best") {
    hospitals[1, 1]
  } else if (ranking == "worst") {
    hospitals[nrow(hospitals), 1]
  } else if (ranking <= nrow(hospitals)) {
    hospitals[ranking, 1]
  } else {
    NA
  }
  
  
}