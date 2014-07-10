complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  ##x <- data.frame(numeric(0), numeric(0)) - sembra non serva inizializzare a vuotocomplete("specdata", 1)
  for(i in id) {
    v <- read.csv(paste(directory,  "/", formatC(i, width=3, flag="0") , ".csv", sep = ""))
    good <- complete.cases(v)
    x <- rbind(x, c(i, nrow(v[good,])))
  }
  colnames(x) <- c("id", "nobs")
  x
}