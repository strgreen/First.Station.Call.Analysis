#Load in libararies
library(lme4)
library(lmerTest)
library(plyr)
library(MASS)
library(reshape)
library(ggplot2)
library(ez)
library(plotrix)

#Need the lmerTest to get the p-values. Uses Satterhwaite approximation to compute df and pvalues
#http://mindingthebrain.blogspot.com/2014/02/three-ways-to-get-parameter-specific-p.html
require(lmerTest)

#Clear Workspace
rm(list=ls())

#Load in the data
#dataTable = read.csv("C:/Users/Baruch Spinoza/Google Drive/Meetup/sing.fire.data.v3.csv")
dataTable = read.csv("C:/Users/Steve Green/Google Drive/Meetup/sing.fire.data.v3.csv")
#dataTable = read.csv("G:/meetup/sing.fire.data.v3.csv")
#dataTable = read.csv("J:/meetup/sing.fire.data.v3.csv")


#change name of distance varaible
names(dataTable)[15] = "Distance"

#Plot to show ouliers 
plot(dataTable$Distance,dataTable$Distance)

#remove outliers from distance data
dataTable = dataTable[dataTable$Distance < sd(dataTable$Distance) + mean(dataTable$Distance),]
out.dataTable = dataTable[dataTable$Distance < sd(dataTable$Distance) + mean(dataTable$Distance),]

#Plot to show ouliers 
plot(out.dataTable$Distance,out.dataTable$Distance)

#Lets see if average distance between station and call in each region is different

#Change reference 
out.dataTable$Region = relevel(out.dataTable$Region, ref = "SE")

#Linear mixed models
lm.model.region = lm('Distance ~ 1 + Region', out.dataTable) 
lm.model.region = lmer('Distance ~ 1 + Region + (1 | Fire.Station)', out.dataTable)
summary(lm.model.region)

lm.model.code = lm('Distance ~ 1 + Call.Code ', out.dataTable)
summary(lm.model.code)

out.dataTable$Call.Code = relevel(out.dataTable$Call.Code, ref = "Investigate")
lm.model.code = lmer('Distance ~ 1 + Call.Code + (1 | Fire.Station)', out.dataTable)
summary(lm.model.code)



mn.dist.reg = aggregate(mn.dist.station.x.reg.x.code$Mean.Distance,
   list(mn.dist.station.x.reg.x.code$Region), 
   FUN = "mean")        

sd.dist.reg = aggregate(mn.dist.station.x.reg.x.code$Mean.Distance,
   list(mn.dist.station.x.reg.x.code$Region), 
   FUN = "std.error")        


limits = aes(ymax = mn.dist.reg[,2] +  sd.dist.reg[,2], 
             ymin = mn.dist.reg[,2] -  sd.dist.reg[,2])


ggplot(mn.dist.reg, aes(Group.1 ,x, fill = Group.1 )) +
  geom_bar(stat="identity", position = "dodge") +
  geom_errorbar(limits,position="dodge",width = .25) +
  scale_fill_manual(values=c(rgb(255,130,080, maxColorValue = 255), 
                             rgb(255,090,030, maxColorValue = 255),
                             rgb(255,060,000, maxColorValue = 255),
                             rgb(255,090,030, maxColorValue = 255) ))

limits = aes(ymax = mn.dist.code[,2] +  sd.dist.code[,2], 
             ymin = mn.dist.code[,2] -  sd.dist.code[,2])



ggplot(mn.dist.code, aes(Group.1 ,x, fill = Group.1 )) +
  geom_bar(stat="identity", position = "dodge") +
  geom_errorbar(limits,position="dodge",width = .25) +
  scale_fill_manual(values=c(rgb(255,130,080, maxColorValue = 255), 
                             rgb(255,090,030, maxColorValue = 255),
                             rgb(255,060,000, maxColorValue = 255),
                             rgb(255,090,030, maxColorValue = 255),
                             rgb(200,050,000, maxColorValue = 255),
                             rgb(150,040,000, maxColorValue = 255) ))
                    




#############################
#Breakdown according to region 

reg.out.dataTable = out.dataTable[out.dataTable$Region == "NE",]

reg.out.dataTable$Call.Code = relevel(reg.out.dataTable$Call.Code, ref = "Alarm")
lm.model.code = lmer('Distance ~ 1 + Call.Code + (1 | Fire.Station)', reg.out.dataTable)
summary(lm.model.code)

mn.dist.code = aggregate(reg.out.dataTable$Distance,
                        list(reg.out.dataTable$Call.Code), 
                        FUN = "mean")        

sd.dist.code = aggregate(reg.out.dataTable$Distance,
                        list(reg.out.dataTable$Call.Code), 
                        FUN = "std.error")        

limits = aes(ymax = mn.dist.code[,2] +  sd.dist.code[,2], 
             ymin = mn.dist.code[,2] -  sd.dist.code[,2])



ggplot(mn.dist.code, aes(Group.1 ,x, fill = Group.1 )) +
  geom_bar(stat="identity", position = "dodge") +
  geom_errorbar(limits,position="dodge",width = .25) +
  scale_fill_manual(values=c(rgb(255,130,080, maxColorValue = 255), 
                             rgb(255,090,030, maxColorValue = 255),
                             rgb(255,060,000, maxColorValue = 255),
                             rgb(255,090,030, maxColorValue = 255),
                             rgb(200,050,000, maxColorValue = 255),
                             rgb(150,040,000, maxColorValue = 255) ))



#############################
#Breakdown according to call type

reg.out.dataTable = out.dataTable[out.dataTable$Call.Code == "Investigate",]

reg.out.dataTable$Call.Code = relevel(reg.out.dataTable$Region, ref = "SE")
lm.model.code = lmer('Distance ~ 1 + Call.Code + (1 | Fire.Station)', reg.out.dataTable)
summary(lm.model.code)

mn.dist.code = aggregate(reg.out.dataTable$Distance,
                         list(reg.out.dataTable$Call.Code), 
                         FUN = "mean")        

sd.dist.code = aggregate(reg.out.dataTable$Distance,
                         list(reg.out.dataTable$Call.Code), 
                         FUN = "std.error")        

limits = aes(ymax = mn.dist.code[,2] +  sd.dist.code[,2], 
             ymin = mn.dist.code[,2] -  sd.dist.code[,2])



ggplot(mn.dist.code, aes(Group.1 ,x, fill = Group.1 )) +
  geom_bar(stat="identity", position = "dodge") +
  geom_errorbar(limits,position="dodge",width = .25) +
  scale_fill_manual(values=c(rgb(255,130,080, maxColorValue = 255), 
                             rgb(255,090,030, maxColorValue = 255),
                             rgb(255,060,000, maxColorValue = 255),
                             rgb(255,090,030, maxColorValue = 255)))


