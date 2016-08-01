#Load in libararies
library(lme4)
library(lmerTest)
library(plyr)
library(MASS)
library(reshape)
library(ggplot2)

#Need the lmerTest to get the p-values. Uses Satterhwaite approximation to compute df and pvalues
#http://mindingthebrain.blogspot.com/2014/02/three-ways-to-get-parameter-specific-p.html
require(lmerTest)

#Clear Workspace
rm(list=ls())

#Load in the data
dataTable = read.csv("C:/Users/Baruch Spinoza/Desktop/meetup/sing.fire.data.v3.csv")

#Compute frequncy for each station for each code

#Need to create table listing fire station as row and code as column 
region.count = table(dataTable$Region)

#Test to see if the frequencies are different from a null of 25%
chi.region.count = chisq.test(region.count, p = c(.25,.25,.25,.25))

#Compute ratio between region count and total count to interpret chi result
ratio.region.count = region.count/sum(region.count)

#Display result in a plot
barplot(ratio.region.count)
#plot a line to illustrate the null (%25 prob)
abline(line(seq(1,4,1),c(.25,.25,.25,.25)))

#Breakdown high and low region to see if there are call differences

##################################################
#select observtions with the two Highest frequency
##################################################  

  subTable = dataTable[(dataTable$Region == 'NW' | dataTable$Region == "SE"),]
  #Update Region to refelct the two remaining levels
  subTable$Region = factor(subTable$Region)

  #Make a table contianing counts for each call type 
  subTable.count = table(subTable$Region,subTable$Call.Code)

  #Peforme chi comparing regions according to call type
  chi.alarm = chisq.test(c(subTable.count[1,1],subTable.count[2,1]))
  chi.fires = chisq.test(c(subTable.count[1,2],subTable.count[2,2]))
  chi.hazrd = chisq.test(c(subTable.count[1,3],subTable.count[2,3]))
  chi.helth = chisq.test(c(subTable.count[1,4],subTable.count[2,4]))
  chi.Invst = chisq.test(c(subTable.count[1,5],subTable.count[2,5]))
  chi.Motor = chisq.test(c(subTable.count[1,6],subTable.count[2,6]))

  #Melt subTable.count into long format
  subTable.count = melt(subTable.count,id=c("Alarm","Fire","Hazard","Health","Investigate","Motor"))

  ggplot(subTable.count, aes(Var.2 ,value, fill = Var.1 )) +
    geom_bar(stat="identity", position = "dodge") +
    scale_fill_manual(values=c("#CC6666", "#9999CC", "#66CC99"))

##################################################
#select observtions with the two Lowest frequency
##################################################  

  subTable = dataTable[(dataTable$Region == 'NE' | dataTable$Region == "SW"),]
  #Update Region to refelct the two remaining levels
  subTable$Region = factor(subTable$Region)
  
  #Make a table contianing counts for each call type 
  subTable.count = table(subTable$Region,subTable$Call.Code)
  
  #Peforme chi comparing regions according to call type
  chi.alarm = chisq.test(c(subTable.count[1,1],subTable.count[2,1]))
  chi.fires = chisq.test(c(subTable.count[1,2],subTable.count[2,2]))
  chi.hazrd = chisq.test(c(subTable.count[1,3],subTable.count[2,3]))
  chi.helth = chisq.test(c(subTable.count[1,4],subTable.count[2,4]))
  chi.Invst = chisq.test(c(subTable.count[1,5],subTable.count[2,5]))
  chi.Motor = chisq.test(c(subTable.count[1,6],subTable.count[2,6]))
  
  #Melt subTable.count into long format
  subTable.count = melt(subTable.count,id=c("Alarm","Fire","Hazard","Health","Investigate","Motor"))
  
  ggplot(subTable.count, aes(Var.2 ,value, fill = Var.1 )) +
    geom_bar(stat="identity", position = "dodge") +
    scale_fill_manual(values=c("#CC6666", "#9999CC", "#66CC99"))
  
  
  #select observtions with the two highest frequency
  
  subTable = dataTable[(dataTable$Region == 'NE' | dataTable$Region == "SW"),]
  #Update Region to refelct the two remaining levels
  subTable$Region = factor(subTable$Region)
  
  #Make a table contianing counts for each call type 
  subTable.count = table(subTable$Region,subTable$Call.Code)
  
  #Peforme chi comparing regions according to call type
  chi.alarm = chisq.test(c(subTable.count[1,1],subTable.count[2,1]))
  chi.fires = chisq.test(c(subTable.count[1,2],subTable.count[2,2]))
  chi.hazrd = chisq.test(c(subTable.count[1,3],subTable.count[2,3]))
  chi.helth = chisq.test(c(subTable.count[1,4],subTable.count[2,4]))
  chi.Invst = chisq.test(c(subTable.count[1,5],subTable.count[2,5]))
  chi.Motor = chisq.test(c(subTable.count[1,6],subTable.count[2,6]))
  
  #Melt subTable.count into long format
  subTable.count = melt(subTable.count,id=c("Alarm","Fire","Hazard","Health","Investigate","Motor"))
  
  ggplot(subTable.count, aes(Var.2 ,value, fill = Var.1 )) +
    geom_bar(stat="identity", position = "dodge") +
    scale_fill_manual(values=c("#CC6666", "#9999CC", "#66CC99"))
  
  #select observtions with the two highest frequency
  
  subTable = dataTable[(dataTable$Region == 'NW' | dataTable$Region == "SE"),]
  #Update Region to refelct the two remaining levels
  subTable$Region = factor(subTable$Region)
  
  #Make a table contianing counts for each call type 
  subTable.count = table(subTable$Region,subTable$Call.Code)
   
  #Peforme chi comparing regions according to call type
  chi.alarm = chisq.test(c(subTable.count[1,1],subTable.count[2,1]))
  chi.fires = chisq.test(c(subTable.count[1,2],subTable.count[2,2]))
  chi.hazrd = chisq.test(c(subTable.count[1,3],subTable.count[2,3]))
  chi.helth = chisq.test(c(subTable.count[1,4],subTable.count[2,4]))
  chi.Invst = chisq.test(c(subTable.count[1,5],subTable.count[2,5]))
  chi.Motor = chisq.test(c(subTable.count[1,6],subTable.count[2,6]))
  
  #Melt subTable.count into long format
  subTable.count = melt(subTable.count,id=c("Alarm","Fire","Hazard","Health","Investigate","Motor"))
  
  ggplot(subTable.count, aes(Var.2 ,value, fill = Var.1 )) +
    geom_bar(stat="identity", position = "dodge") +
    scale_fill_manual(values=c("#CC6666", "#9999CC", "#66CC99"))
  
##################################################
##########Examine code type frequencies###########
##################################################
  
 code.count = table(dataTable$Call.Code)
  
  