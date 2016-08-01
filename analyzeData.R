#Load in libararies
library(lme4)
library(lmerTest)
library(plyr)
library(MASS)

#Need the lmerTest to get the p-values. Uses Satterhwaite approximation to compute df and pvalues
#http://mindingthebrain.blogspot.com/2014/02/three-ways-to-get-parameter-specific-p.html
require(lmerTest)

#Clear Workspace
rm(list=ls())

#Load in the data
dataTable = read.csv("C:/Users/Baruch Spinoza/Desktop/meetup/fire.data.v2.csv")

#Get only data from tulsa stations
tulsa.dataTable = dataTable[dataTable$Fire.Station.Code == 'Tulsa',c(1,5,11,14)]

#Region by Code Type
reg.x.code = table(tulsa.dataTable$Region,tulsa.dataTable$Call.Code)

#Region by Fire Stat
stats.x.code = table(tulsa.dataTable$Fire.Station.Number, tulsa.dataTable$Call.Code)
stats.x.code = as.data.frame.matrix(stats.x.code)

temp = 0; 

for (Index in 1:nrow(stats.x.code)) {
  stats.x.code[Index,7] = sum(stats.x.code[Index,1:6])
  print(Index)
}


per.stats.x.code = stats.x.code
for (nrow in 1:nrow(stats.x.code)) {
  for (ncol in 1:6) {
  per.stats.x.code[nrow,ncol] = stats.x.code[nrow,ncol]/stats.x.code[nrow,7]
  print(Index)
  }
}
boxplot(per.stats.x.code[,1:6])

