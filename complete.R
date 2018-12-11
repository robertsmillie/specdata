complete <- function(directory, id=1:332) {
  library(stringr)
  library(withr)
  ids<-numeric()
  nobs<-numeric()
  for(j in id) {
    i<-j
    i <- with_options(
      c(scipen = 999), 
      str_pad(i, 3, pad = "0")
    )
    i <- paste(i, ".csv", sep = "")
    mydata <- read.csv(i, header = TRUE)
    mydata <- mydata[complete.cases(mydata),]
    ids <- c(ids, j)
    nobs <- c(nobs, length(mydata[["Date"]]))
  }
  data.frame("id" = ids, "nobs" = nobs)
}