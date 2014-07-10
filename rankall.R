rankall <- function(outcome, num = "best") {
  outcomes <- c(11,17,23)
  names(outcomes) <- c("heart attack", "heart failure", "pneumonia")

  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that outcome are valid
  if (!(outcome %in% names(outcomes))) stop("invalid outcome")

  ## data column is numeric
  ncol <- 2
  dcol <- outcomes[[outcome]]
  data[,dcol] <- suppressWarnings(as.numeric(data[,dcol]))
  
  states <- sort(unique(data[,"State"]))
  df <- data.frame(hospital = vector("character", length = length(states)), state = states, stringsAsFactors = FALSE) 

  ## For each state, find the hospital of the given rank
  for (s in states) {
    hospitals <- data[data$State == s, c(ncol, dcol)]
    
    #order by rate, name and exclude NAs
    hospitals <-  hospitals[order(hospitals[,2], hospitals[,1], na.last = NA),]
    
    if (num == "best") {
      df[df$state == s, "hospital"] = hospitals[1, 1]
    } else if (num == "worst") {
      df[df$state == s, "hospital"] = hospitals[nrow(hospitals), 1]
    } else if (num <= nrow(hospitals)) {
      df[df$state == s, "hospital"] = hospitals[num, 1]
    } else {
      df[df$state == s, "hospital"] = NA
    }
  
  }
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
   df
}