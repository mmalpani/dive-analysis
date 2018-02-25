library(mosaic)
library(ggplot2)
library(dplyr)
library(manipulate)
library(lubridate)

setwd("/Users/megs/Desktop/Brown University/Junior Spring/Independent Study/Data_Analysis")
fullTrack <- read.csv("AnnotatedDiveData.csv")

annotateDiveDataComplicatedVersion <- function(trackData) {
  #trackData <- filter(trackData, Depth >= -2) #get rid of any bad data points
  
  # Combine date and time and add to the full track
  #combinedTime <- paste("2000/", trackData$Date, sep="")
  #combinedTime <- paste(combinedTime, trackData$Time) 
  #combinedTime <- ymd_hms(combinedTime)
  #trackData$Combined.Time <- combinedTime
  
  #for (i in 254017:nrow(trackData)) { #new year data
  #  trackData$Combined.Time[i] = trackData$Combined.Time[i] + years(1)
  # }
  
  #trackData <- filter(trackData, Depth >= 0)
  
  #Count and annotate dives.
  trackData$Dive.Number <- 0;
  minDelta <- 30;
  threshold <- 50;
  startingIndex <- 1;
  startingDepth <- trackData$Depth[startingIndex];
  maxDepth <- startingDepth;
  onDive <- 0; #0 is false, and 1 is true
  diveCounter <- 0;
  i <- startingIndex+1;
  
  while (i <= nrow(trackData)) {
    while (onDive == 0 && i <= nrow(trackData)) { #not on dive
      currentDepth <- trackData$Depth[i];
      if (currentDepth <= startingDepth) {
        #also adjust starting depth if it isnt changing by a certain amount within a certain span of time.
        # could use a pre-Start counter but I feel like ther are better approaches?
        startingDepth <- currentDepth;
        startingIndex <- i;
      } else if ((currentDepth - startingDepth)  >= minDelta) { 
        onDive <- 1;
        maxDepth <- currentDepth;
      }
      i <- i+1;
    }
    
    previousDepth <- trackData$Depth[i-1];
    onInitDescent <- 1;
    while (onDive == 1 && i <= nrow(trackData)) { #on dive
      currentDepth <- trackData$Depth[i]
      if (currentDepth > maxDepth) {
        maxDepth <- currentDepth;
      } else {
        onInitDescent <- 0; #no long on the initDescent
      }
      # if initDescent is over, passed a depth less than 50m 
      # is now descending again, and increased by the delta required,
      # dive is over.
      if ((onInitDescent == 0) &&
          (previousDepth <= threshold) && 
          (currentDepth >= previousDepth) && 
          (previousDepth < maxDepth - minDelta)) {
        onDive <- 0;
        diveCounter <- diveCounter+1;
        trackData$Dive.Number[startingIndex:(i-1)] <- diveCounter;
        startingIndex <- i-1;
        startingDepth <- trackData$Depth[startingIndex];
      } else {
        previousDepth <- currentDepth;
        i <- i+1;
      }
    }
  }
  
  write.csv(trackData, file = "AnnotatedDiveData.csv", row.names=FALSE)
  cat(sprintf("Number of dives is: %d", diveCounter))
}


annotateDiveDataComplicatedVersion(fullTrack)


