corr <- function(directory, threshold=0) {
  n <- complete(directory)[["nobs"]]
  cors <- numeric()
  for (j in 1:332) {
    if (n[j] > threshold) {
      i<-j
      i <- with_options(
        c(scipen = 999), 
        str_pad(i, 3, pad = "0")
      )
      i <- paste(i, ".csv", sep = "")
      mydata <- read.csv(i, header = TRUE)
      mydata <- mydata[complete.cases(mydata),]
      cors <- c(cors, cor(mydata[c(2,3)])[1,2])
    }
  }
  cors
}