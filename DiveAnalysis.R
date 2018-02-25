library(mosaic)
library(ggplot2)
library(dplyr)
library(manipulate)
library(lubridate)
library(ggpubr)

setwd("/Users/megs/Desktop/Brown University/Junior Spring/Independent Study/Data_Analysis")
fullTrack <- read.csv("AnnotatedDiveData.csv")

# Combine date and time and add to the full track
fullTrack$Combined.Time <- ymd_hms(fullTrack$Combined.Time) #convert to time object

diveStats <- read.csv("DiveDataStatistics.csv")

#This method plots the Depth vs. Time graph for the desired subset of the data
graphDiveDataSubset <- function(trackData, diveNumStart, diveNumEnd) {
  #Graphing a specific dive
  diveData <- filter(trackData, between(Dive.Number, diveNumStart, diveNumEnd))
  dataForPlot <- data.frame(diveData$Combined.Time, diveData$Depth)
  graph <- ggplot(dataForPlot, aes(x=diveData.Combined.Time, y=diveData.Depth, col=diveData$Dive.Number)) + 
    geom_line() + scale_y_reverse() 
  return(graph)
}

#This method plots the water column for a set of dive data.
temperatureAnalysis <- function(trackData, diveNumStart, diveNumEnd) {
  diveData <- filter(trackData, between(Dive.Number, diveNumStart, diveNumEnd))
  dataForPlot <- data.frame(diveData$External.Temperature, diveData$Depth)
  #thirdOrderPoly <- lm(diveData$Depth ~ poly(diveData$External.Temperature, 3, raw=TRUE))
  graph <- ggplot(dataForPlot, aes(x=diveData.External.Temperature, y=diveData.Depth)) + geom_point() + scale_y_reverse()
  #test this out.
  third <- stat_smooth(method="lm", se=TRUE, fill=NA, formula=y ~ poly(x, 3, raw=TRUE),colour="blue")
  
  #use calculus to get line inflection point.
  graph + third
  #third
}


# This method fully analyzes each dive for start time, end time, avg duration, 
# avg depth, max depth, min depth, avg temp, max temp, min temp, 
# and will eventually have  % time at each level of the water column 
# (mixed layer, thermocline, deep layer)
fullDiveAnalysis <- function(trackData) {
  
  numDives = max(trackData$Dive.Number)
  
  #Initilizing vectors
  
  #Basic Dive Information
  Start.Time <- vector(mode="character", length=numDives);
  End.Time <- vector(mode="character", length=numDives);
  Dive.Duration <- vector(mode="character", length=numDives); #in minutes
  Average.Depth <- vector(mode="integer", length=numDives);
  Min.Depth <- vector(mode="integer", length=numDives);
  Max.Depth <- vector(mode="integer", length=numDives);
  
  #Dive Profile Characterization
  Percentage.Surface <- vector(mode="integer", length=numDives); #0-20m
  Percentage.Shallow <- vector(mode="integer", length=numDives); #20-100
  Percentage.Middle <- vector(mode="integer", length=numDives); #100-200
  Percentage.Deep <- vector(mode="integer", length=numDives); #200-500
  Percentage.VeryDeep <- vector(mode="integer", length=numDives); #500-beyond
  
  #Thermocline Analysis
  Surface.Temperature <- vector(mode="integer", length=numDives);
  Min.Temperature <- vector(mode="integer", length=numDives);
  Thermocline.Depth <- vector(mode="integer", length=numDives);
  Thermocline.Temperature <- vector(mode="integer", length=numDives);
  Percentage.Above.Thermocline <- vector(mode="integer", length=numDives);
  Percentage.Below.Thermocline <- vector(mode="integer", length=numDives);
  
  for (i in 1:numDives) {
    diveData <- filter(trackData, between(Dive.Number, i, i))
    length <- nrow(diveData)
    
    #Basic Dive Informaiton
    Start.Time[i] <- toString(diveData$Combined.Time[1])
    End.Time[i] <- toString(diveData$Combined.Time[nrow(diveData)])
    Dive.Duration[i] = difftime(diveData$Combined.Time[nrow(diveData)], diveData$Combined.Time[1], units="mins") #need this to specific hours or minutes..
    Average.Depth[i] <- mean(diveData$Depth)
    Min.Depth[i] <- min(diveData$Depth)
    Max.Depth[i] <- max(diveData$Depth)
    
    # Dive Profile Characteristics
    Percentage.Surface[i] <- nrow(filter(diveData, Depth < 30))/length*100
    Percentage.Shallow[i] <- nrow(filter(diveData, between(Depth, 31, 100)))/length*100
    Percentage.Middle[i] <-  nrow(filter(diveData, between(Depth, 101, 200)))/length*100
    Percentage.Deep[i] <-  nrow(filter(diveData, between(Depth, 201, 500)))/length*100
    Percentage.VeryDeep[i] <- nrow(filter(diveData, Depth > 501))/length*100 #500-beyond
    
    #Temperature/Thermocline Analysis
    surface <- filter(diveData, between(Depth, 0, 50))
    Surface.Temperature[i] <- mean(surface$External.Temperature)
    Min.Temperature[i] <- min(diveData$External.Temperature)
    
    #Data wanted before & after
    extraDuration <- (360 - Dive.Duration[i])/2;
    extraDuration <- max(extraDuration, 0);
    thermoclineData <- filter(trackData, between(Combined.Time, 
                                                 diveData$Combined.Time[1] - extraDuration, 
                                                 diveData$Combined.Time[nrow(diveData)] + extraDuration))
    
    Thermocline.Depth[i] <- 0 #fill out
    Thermocline.Temperature[i] <- 0 #fill out
    Percentage.Above.Thermocline[i] <- 0 #fill out
    Percentage.Below.Thermocline[i] <- 0 #fill out
    
     #TODOS
    # Get thermocline depth for each dive 
    # don't want all the surface data to create a weird thermocline graph (normalize it - only if its past a certain depth?)
    # Get % of time above and below the thermocline
    # if dive doesn't go past 50m, just say the whole thing is above the thermocline and don't put anything there.
    
  }
  finalResults <- data.frame(Start.Time, End.Time, Dive.Duration, 
                             Average.Depth, Min.Depth, Max.Depth, 
                             Percentage.Surface, Percentage.Shallow, 
                             Percentage.Middle, Percentage.Deep, 
                             Percentage.VeryDeep, Surface.Temperature,
                             Min.Temperature, Thermocline.Depth,
                             Thermocline.Temperature, Percentage.Above.Thermocline,
                             Percentage.Below.Thermocline)
  write.csv(finalResults, file = "AdvancedDiveDataStatistics3_addedparams.csv")
}

getThermoclineData <- function(trackData, diveData) {
  thermoclineData <- filter(trackData, between(Combined.Time, 
                                               diveData$Combined.Time[1] - hours(2), 
                                               diveData$Combined.Time[nrow(diveData)] + hours(2)))
  
}

#This method plots the kmeans results with increasing cluster numbers 
kmeansAnalysis <- function(diveData) {
  diveData <- select(diveData, Average.Depth, Max.Depth, 
                     Percentage.Surface, Percentage.Shallow, Percentage.Middle, 
                     Percentage.Deep, Percentage.VeryDeep)
  diveData <- apply(diveData,2,function(x) x/max(x));
  numIterations <- 10
  values <- numeric(numIterations)
  for(i in 1:numIterations) {
    values[i] <-kmeans(diveData, i, iter.max=50)$tot.withinss
  }
  plot(values)
}

#Gives you the k means 
kmeansAnalysisSpecific <- function(diveData, clusterNum) {
  diveData <- select(diveData, Dive.Duration, Average.Depth, Max.Depth, 
                     Percentage.Surface, Percentage.Shallow, Percentage.Middle, 
                     Percentage.Deep, Percentage.VeryDeep)
  model <- kmeans(diveData, clusterNum)
  #results <- apply(diveData, 1, function(x) clusters(x, model$centers));
  results <- clusters(diveData, model$centers)
  print(model$centers)
  print(table(results))
}

clusters <- function(x, centers) {
  # compute squared euclidean distance from each sample to each cluster center
  tmp <- sapply(seq_len(nrow(x)),
                function(i) apply(centers, 1,
                                  function(v) sum((x[i, ]-v)^2)))
  max.col(-t(tmp))  # find index of min distance
}

kmeansAnalysisSpecific(diveStats, 6)
#kmeansAnalysis(diveStats)
#fullDiveAnalysis(fullTrack)  
#temperatureAnalysis(fullTrack, 4000, 4500)

#all surface ones
#graphDiveDataSubset(fullTrack, 1, 10)
#graphDiveDataSubset(fullTrack, 4027, 4027)
#graphDiveDataSubset(fullTrack, 5275, 5275)
#graphDiveDataSubset(fullTrack, 5274, 5274)
#graphDiveDataSubset(fullTrack, 5278, 5278)
#graphDiveDataSubset(fullTrack, 5269, 5269)

#interesting lengthy periods at depth
#graphDiveDataSubset(fullTrack, 5772, 5772)
#graphDiveDataSubset(fullTrack, 130, 140)

#deep dives
#graphDiveDataSubset(fullTrack, 5365, 5365)
#graphDiveDataSubset(fullTrack, 5372, 5372)
#graphDiveDataSubset(fullTrack, 7512, 7512)
#graphDiveDataSubset(fullTrack, 1525, 1525)




