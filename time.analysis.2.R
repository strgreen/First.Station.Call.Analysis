#Load in libararies
library(lme4)
library(lmerTest)
library(plyr)
library(MASS)
library(reshape)
library(ggplot2)


#Clear Workspace
rm(list=ls())

#Load in the data
dataTable = read.csv("C:/Users/Baruch Spinoza/Desktop/meetup/sing.fire.data.v3.csv")

#change name of distance varaible
names(dataTable)[15] = "Distance"

#Change order of levels in region factor
dataTable$Day = relevel(dataTable$Day, ref = "Sunday")
dataTable$Day = relevel(dataTable$Day, ref = "Saturday")
dataTable$Day = relevel(dataTable$Day, ref = "Friday")
dataTable$Day = relevel(dataTable$Day, ref = "Thursday")
dataTable$Day = relevel(dataTable$Day, ref = "Wednesday")
dataTable$Day = relevel(dataTable$Day, ref = "Tuesday")
dataTable$Day = relevel(dataTable$Day, ref = "Monday")

#Okay lets remove outliers
#remove outliers from distance data
dataTable = dataTable[dataTable$Distance < sd(dataTable$Distance) + mean(dataTable$Distance),]
out.dataTable = dataTable[dataTable$Distance < sd(dataTable$Distance) + mean(dataTable$Distance),]

#Need to create table listing fire station as row and code as column 
day.count = table(out.dataTable$Day)

#Test to see if the frequencies are different from a null of 14%
chi.day.count = chisq.test(day.count)
#sig in frequency

#Identify test each day with the highest day
  #Monday - Friday #Not sig
  mon.test = chisq.test(c(day.count[5],day.count[1]))
  #Tuesday - Friday  #Sig
  tue.test = chisq.test(c(day.count[5],day.count[2]))
  #Wednesday - Friday #Not sig
  wed.test = chisq.test(c(day.count[5],day.count[3]))
  #Thursday - Friday #Not Sig
  thu.test = chisq.test(c(day.count[5],day.count[4]))
  #Saturday - Friday #Sig
  sat.test = chisq.test(c(day.count[5],day.count[6]))
  #Sunday - Friday #Sig
  sun.test = chisq.test(c(day.count[5],day.count[7]))
    
##################################
####Interact day and code type####  
##################################  

day.code.count = table(out.dataTable$Day,out.dataTable$Call.Code)  

#Overall test show sig  
chi.day.x.code = chisq.test(day.code.count)

#Test Alarm: Sig
chisq.test(day.code.count[,1])
#Test Fire: No Sig
chisq.test(day.code.count[,2])
#Test Hazard: Sig
chisq.test(day.code.count[,3])
#Test Health: no Sig
chisq.test(day.code.count[,4])
#Test Investigate: no Sig
chisq.test(day.code.count[,5])
#Test Motor: sig
chisq.test(day.code.count[,6])

  chisq.test(c(day.code.count[5,6],day.code.count[4,6]))
    
    
