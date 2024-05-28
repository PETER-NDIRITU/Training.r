   #    Practical 1 - Introduction
setwd("C:/Users/thuku/OneDrive/Documents/GeoSpatial Developments/R Studio/Intro_R") # nolint
Ethnicity <- read.csv("KS201EW_oa11.csv") # nolint
Rooms <- read.csv("KS401EW_oa11.csv") #nolint
Qualifications <- read.csv("KS501EW_oa11.csv") # nolint
Employment <- read.csv("KS601EW_oa11.csv") # nolint

#view the data
View(Employment)
View(Rooms)
View(Qualifications)
View(Ethnicity)

#See the columns within Employment dataset
names(Employment)
Employment <- Employment[,c(1,20)]  #column selection
Ethnicity <- Ethnicity[,c(1,21)]
Rooms <- Rooms[,c(1,13)]
Qualifications <- Qualifications[,c(1,20)]

#we rename the columns
names(Employment)[2] <-"Unemployed"
names(Employment)

# we can however rename both columns;
names(Ethnicity) <- c("OA", "White_British") # nolint
names(Rooms) <- c("OA", "Low_Occupancy") # nolint
names(Employment) <- c("OA", "Unemployed") # nolint
names(Qualifications) <- c("OA", "Qualification") # nolint
names(Qualifications)
View(Qualifications)

#lets now merge our datasets
merge_data_1 <- merge(Ethnicity, Rooms, by="OA")
merge_data_2 <- merge(merge_data_1, Employment, by="OA")
Census.Data <- merge(merge_data_2, Qualifications, by="OA")

#remove the merged data
rm(merge_data_1, merge_data_2)

# lets now export our data
write.csv(Census.Data, "practical_data.csv", row.names=F)
View(Census.Data)


   #  Practical 2 - Data Exploration

Census.Data <- read.csv("practical_data.csv")
print(Census.Data)
print(Census.Data[1:20, 1:5])
#dataset statistics
summary(Census.Data)
head(Census.Data)
tail(Census.Data)
ncol(Census.Data)
nrow(Census.Data)
names(Census.Data)
mean(Census.Data$White_British)
median(Census.Data$Low_Occupancy)
range(Census.Data$Qualification)

#Plots
hist(Census.Data$Qualification, breaks=20, col="#3224af", main="%Qualification", xlab="Percentage")
boxplot(Census.Data[,2:5], col="yellow")

library(vioplot)
vioplot(Census.Data$Unemployed, Census.Data$White_British, Census.Data$Qualification, ylim=c(0,100), col="dodgerblue", rectCol="dodgerblue3", colMed="dodgerblue4",
names=c("Unemployed", "White_British",  "Qualifications"))


         
#####   Practical 3 ---  Bivariate Plots
setwd("C:/Users/thuku/OneDrive/Documents/GeoSpatial Developments/R Studio/Bivariate Plots")
Census.Data <-read.csv("practical_data.csv")
View(Census.Data)

#simple scatter plot
symbols(Census.Data$Unemployed, Census.Data$Qualification, circles = Census.Data$White_British, fig = "white", bg="purple", inches = 0.2)

#lm is used in plotting linear models, like for our case Regression model was used
symbols(Census.Data$Unemployed, Census.Data$Qualification, circles = Census.Data$White_British, xlab="%in full employmeny", ylab="%with a Qualification", fig = "white", bg="purple", inches = 0.2) + abline(lm(Census.Data$Qualification~ Census.Data$Unemployed), col="red")

#we have various line types
symbols(Census.Data$Unemployed, Census.Data$Qualification, circles = Census.Data$White_British, xlab="%in full employmeny", ylab="%with a Qualification", fig = "white", bg="purple", inches = 0.2) + abline(lm(Census.Data$Qualification~ Census.Data$Unemployed), col="red", lwd=2, lty=2)

#lets utilize the ggplot2 package
library(ggplot2)
p <-ggplot(Census.Data, aes(Unemployed, Qualification))
p + geom_point()

p <-ggplot(Census.Data, aes(Unemployed, Qualification))
p + geom_point(aes(colour = White_British, size = Low_Occupancy))




##########   finding relationships ins R--------PRACTICAL 4


