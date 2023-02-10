#First Density Compare Calculation 

library(sm)
library(plyr)
Amazon_Capstone_Project <- read.csv("~/Downloads/Train.csv")
Amazon_Capstone_Project$Mode_of_Shipment <- revalue(x = Amazon_Capstone_Project$Mode_of_Shipment, c("Flight" = 0, "Ship" = 1, "Road" = 2))
x <- Amazon_Capstone_Project$Mode_of_Shipment
y <- Amazon_Capstone_Project$Customer_rating
x.f <- factor(x, levels=c(0,1,2),labels=c("Flight", "Ship", "Road"))
sm.density.compare(y, x, xlab="Customer Rating")
colfill<-c(2:(2+length(levels(x.f))))
legend(locator(1),levels(x.f),fill=colfill)

# Second Density Compare Calculation 

library(sm)
library(plyr)
Amazon_Capstone_Project <- read.csv("~/Downloads/Train.csv")
Amazon_Capstone_Project$Mode_of_Shipment <- revalue(x = Amazon_Capstone_Project$Mode_of_Shipment, c("Flight" = 0, "Ship" = 1, "Road" = 2))
x <- Amazon_Capstone_Project$Mode_of_Shipment
y <- Amazon_Capstone_Project$Prior_purchases
x.f <- factor(x, levels=c(0,1,2),labels=c("Flight", "Ship", "Road"))
sm.density.compare(y, x, xlab="Loyal Customer")
colfill<-c(2:(2+length(levels(x.f))))
legend(locator(1),levels(x.f),fill=colfill)

# Third Density Compare Calculation 

library(sm)
library(plyr)
Amazon_Capstone_Project <- read.csv("~/Downloads/Train.csv")
Amazon_Capstone_Project$Mode_of_Shipment <- revalue(x = Amazon_Capstone_Project$Mode_of_Shipment, c("Flight" = 0, "Ship" = 1, "Road" = 2))
x <- Amazon_Capstone_Project$Mode_of_Shipment
y <- Amazon_Capstone_Project$Reached.on.Time_Y.N
x.f <- factor(x, levels=c(0,1,2),labels=c("Flight", "Ship", "Road"))
sm.density.compare(y, x, xlab="Arrival Time Based on Shipment Method")
colfill<-c(2:(2+length(levels(x.f))))
legend(locator(1),levels(x.f),fill=colfill)

#Pie Chart On Time Deliveries

Amazon_Capstone_Project$Reached.on.Time_Y.N <- revalue(as.character(Amazon_Capstone_Project$Reached.on.Time_Y.N), c("0" = "No", "1" = "Yes"))
slices <- c(1, 2)
lbls <- c("No", "Yes")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Deliveries Received on Time")

# Second Pie Chart Option Without %

slices <- c(1, 2)
lbls <- c("Flight", "Ship", "Road")
pie(slices, labels = lbls, main="Pie Chart of Shipment Method")
View(Amazon_Capstone_Project)

#Bar Chart Plot Function 
#Made Comparison between Flight and Road delivery 
Amazon_Capstone_Project$Mode_of_Shipment <- revalue(x = Amazon_Capstone_Project$Mode_of_Shipment, c("Flight" = 1, "Road" = 2))
#Amazon_Capstone_Project$Reached.on.Time_Y.N <- revalue(x = Amazon_Capstone_Project$Reached.on.Time_Y.N, c("No" = 0, "Yes" = 1))
H <- c(1, 2)
M <- c("No", "Yes")
barplot(H, names.arg = M, xlab = "Arrived on Time", ylab = "Mode of Shipment", col = "blue", 
        main = "Delivery Time Based on Shipment Method")



#Summary of Dataset

summary(Amazon_Capstone_Project)

#GGplot Calculations

library(ggplot2)

x <- Amazon_Capstone_Project$Customer_care_calls
y <- Amazon_Capstone_Project$Customer_rating

plot(y ~ x, data = Amazon_Capstone_Project)

#Test Functions & Package Installs Below This Point

summary(Amazon_Capstone_Project)
hist(Amazon_Capstone_Project$Customer_care_calls)
sd(Amazon_Capstone_Project$Customer_rating)
t.test(x,y, paired = TRUE)
hist(Amazon_Capstone_Project$Reached.on.Time_Y.N)

x <- Amazon_Capstone_Project$Reached.on.Time_Y.N
y <- Amazon_Capstone_Project$Customer_rating

plot(y ~ x, data = Amazon_Capstone_Project)


scatter.smooth(x, y)

install.packages("tidyverse")

str(Amazon_Capstone_Project)

View(Amazon_Capstone_Project)

filter(Amazon_Capstone_Project, Amazon_Capstone_Project$Mode_of_Shipment == "Ship")


library(plyr)
Amazon_Capstone_Project$Mode_of_Shipment <- revalue(x = Amazon_Capstone_Project$Mode_of_Shipment, c("Flight" = 0, "Ship" = 1, "Road" = 2))
Amazon_Capstone_Project
x <- Amazon_Capstone_Project$Mode_of_Shipment
y <- Amazon_Capstone_Project$Customer_rating
plot(y ~ x, data = Amazon_Capstone_Project)
scatter.smooth(x, y)
hist(Amazon_Capstone_Project$Mode_of_Shipment)
 plot.default(y, x, Amazon_Capstone_Project)
 
 plot(x, y, main = "This Sucks ASS",
      xlab = "Care Calls", ylab = "Customer Rating",
      pch = 19, frame = FALSE)
 abline(lm(y ~ x, data = Amazon_Capstone_Project), col = "blue")

 table(unlist(Amazon_Capstone_Project))
 
 install.packages("sm")
 
 library("sm")
 sm.density.compare(x, y, xlab="Mode of Shipment", ylab="Rating")