best <- function(state, outcome) {
  
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
  
  ncol = 2
  dcol = outcomes[[outcome]]
  
  data[,dcol] <- suppressWarnings(as.numeric(data[,dcol]))
  diseasemin <- min(data[data$State == state,dcol], na.rm = TRUE)
  ##names <- data[!is.na(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack) & data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == diseasemin & data$State == state, ][2]
  names <- data[!is.na(data[,dcol]) & data[,dcol] == diseasemin & data$State == state, ][,ncol]
  sort(names)[1]
  
}
