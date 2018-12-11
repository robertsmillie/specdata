pollutantmean<- function(directory, pollutant, id=1:332) {
  library(stringr)
  library(withr)
  means<-numeric()
  entries<-numeric()
  for(j in id) {
    i<-j
    i <- with_options(
      c(scipen = 999), 
      str_pad(i, 3, pad = "0")
    )
    i <- paste(i, ".csv", sep = "")
    mydata <- read.csv(i, header = TRUE)
    column <- mydata[[pollutant]]
    mean1 <- mean(column, na.rm = TRUE)
    entrycheck <- !is.na(column)
    entryno <- sum(entrycheck)
    if(!is.na(mean1)){
      means <- c(means, mean1)
      entries <- c(entries, entryno)
    }
  }  
  finalmean <- sum(means*entries)/sum(entries)
  finalmean
  
}