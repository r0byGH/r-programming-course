corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  filenames <- list.files(directory, pattern="*.csv")
  fullfilenames <- paste(directory, filenames, sep="/")
  
  cc <- vector(mode="numeric", length=0)
  for(f in fullfilenames){
    v <- read.csv(f)
    good <- complete.cases(v)
    if(nrow(v[good,]) > threshold){
      cc <- c(cc, cor(v[good,]$sulfate, v[good,]$nitrate))  
    }
  }
  cc
  
}