#library(lme4)
#library(lmerTest)
library(ggplot2)

#Clear Workspace
rm(list=ls())

par(mfrow=c(1,1))

#Load in the data
dataTable = read.csv("C:/Users/Steve Green/Google Drive/Meetup/sing.fire.data.v3.csv")
#dataTable = read.csv("C:/Users/Baruch Spinoza/Google Drive/Meetup/sing.fire.data.v3.csv")
#dataTable = read.csv("C:/Users/sgreen/Google Drive/Meetup/sing.fire.data.v3.csv")

#Change order of levels in region factor
dataTable$Region = relevel(dataTable$Region, ref = "SE")
dataTable$Region = relevel(dataTable$Region, ref = "NE")
dataTable$Region = relevel(dataTable$Region, ref = "SW")
dataTable$Region = relevel(dataTable$Region, ref = "NW")

#Change order of levels in code type factor

dataTable$Call.Code = relevel(dataTable$Call.Code, ref = "Investigate")
dataTable$Call.Code = relevel(dataTable$Call.Code, ref = "Hazard")
dataTable$Call.Code = relevel(dataTable$Call.Code, ref = "Health")
dataTable$Call.Code = relevel(dataTable$Call.Code, ref = "Fire")
dataTable$Call.Code = relevel(dataTable$Call.Code, ref = "Motor")
dataTable$Call.Code = relevel(dataTable$Call.Code, ref = "Alarm")

#change name of distance varaible
names(dataTable)[15] = "Distance"

#Lets just look at the data. See if there is anything odd. 
hist(dataTable$Distance)

#Okay thats odd. The plot shows almost all observations in the first bin.
#This is likley because of outliers
plot(dataTable$Distance, dataTable$Distance)

#Okay lets remove outliers
#remove outliers from distance data
dataTable = dataTable[dataTable$Distance < sd(dataTable$Distance) + mean(dataTable$Distance),]
out.dataTable = dataTable[dataTable$Distance < sd(dataTable$Distance) + mean(dataTable$Distance),]

#Reinspect
plot(out.dataTable$Distance, out.dataTable$Distance)
hist(out.dataTable$Distance)

#Data Looks good. Lets do some linear regressions

#######################################################################
######################  Region Analysis  ##############################
######################################################################

lm.fit = lm('Distance ~ as.numeric(Region) + 1', out.dataTable )
#Go to power point
summary(lm.fit)

#Plot to see what the data looks like
plot(out.dataTable$Distance~as.numeric(out.dataTable$Region), 
     main = "Effect of Region On Distance", xlab = "Region", ylab = "Distance", xaxt = 'n' )
axis(side=1, at = out.dataTable$Region, labels = out.dataTable$Region)
abline(lm(out.dataTable$Distance~as.numeric(out.dataTable$Region)))

#compute means of each region for ggplot2
mn.dist.reg = aggregate(out.dataTable$Distance,list(out.dataTable$Region), FUN = "mean")

ggplot(mn.dist.reg, aes(Group.1 ,x, fill = Group.1 )) +
  geom_bar(stat="identity", position = "dodge") +
  ggtitle("Region on Distance") +
  labs(x ="Region", y = "Distance") +
  scale_fill_manual(values=c(rgb(255,130,080, maxColorValue = 255), 
                             rgb(255,090,030, maxColorValue = 255),
                             rgb(255,060,000, maxColorValue = 255),
                             rgb(255,090,030, maxColorValue = 255) ))


#breakdown to compare all regions against lowest region 
lm.fit = lm('Distance ~ Region + 1', out.dataTable)
summary(lm.fit)

######################  Region Analysis  ##############################
#######################################################################

#######################################################################
######################  Call Code Analysis  ###########################

lm.fit = lm('Distance ~ as.numeric(Call.Code) + 1', out.dataTable )
summary(lm.fit)

#Plot to see what the data looks like
plot(out.dataTable$Distance~as.numeric(out.dataTable$Call.Code), 
     main = "Effect of Region On Distance", xlab = "Region", ylab = "Distance", xaxt = 'n' )
axis(side=1, at = out.dataTable$Call.Code, labels = out.dataTable$Call.Code)
abline(lm(out.dataTable$Distance~as.numeric(out.dataTable$Call.Code)))

lm.fit = lm('Distance ~ Call.Code + 1', out.dataTable)
summary(lm.fit)

######################  Call Code Analysis  ###########################
#######################################################################

lm.fit = lm('Distance ~ Call.Code + Region + 1', out.dataTable)
summary(lm.fit)
