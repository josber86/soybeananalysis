#File for Hypothesis Testing on Soy Trade
install.packages('tidyverse')
install.packages('tidyr')
install.packages("TTR")
install.packages('moments')
install.packages("outliers")
library(tidyverse)
library(lubridate)
library(ggplot2)
library(TTR)
library(tsbox)
library(reshape2)
library(moments)
library(outliers)


#Collect data from the https://fgisonline.ams.usda.gov/ExportGrainReport/
gi.2018 <- read.csv("https://fgisonline.ams.usda.gov/ExportGrainReport/CY2018.csv")
gi.2018.china <-  subset(gi.2018, Destination =='CHINA MAIN')
gi.2018.china.soy <- subset(gi.2018.china, Grain =='SOYBEANS')

gi.2017 <- read.csv("https://fgisonline.ams.usda.gov/ExportGrainReport/CY2017.csv")
gi.2017.china <-  subset(gi.2017, Destination =='CHINA MAIN')
gi.2017.china.soy <- subset(gi.2017.china, Grain =='SOYBEANS')

gi.2016<- read.csv("https://fgisonline.ams.usda.gov/ExportGrainReport/CY2016.csv")
gi.2016.china <-  subset(gi.2016, Destination =='CHINA MAIN')
gi.2016.china.soy <- subset(gi.2016.china, Grain =='SOYBEANS')

gi.2015<- read.csv("https://fgisonline.ams.usda.gov/ExportGrainReport/CY2015.csv")
gi.2015.china <-  subset(gi.2015, Destination =='CHINA MAIN')
gi.2015.china.soy <- subset(gi.2015.china, Grain =='SOYBEANS')

gi.2014<- read.csv("https://fgisonline.ams.usda.gov/ExportGrainReport/CY2014.csv")
gi.2014.china <-  subset(gi.2014, Destination =='CHINA MAIN')
gi.2014.china.soy <- subset(gi.2014.china, Grain =='SOYBEANS')

gi.2013<- read.csv("https://fgisonline.ams.usda.gov/ExportGrainReport/CY2013.csv")
gi.2013.china <-  subset(gi.2013, Destination =='CHINA MAIN')
gi.2013.china.soy <- subset(gi.2013.china, Grain =='SOYBEANS')

gi.2012<- read.csv("https://fgisonline.ams.usda.gov/ExportGrainReport/CY2012.csv")
gi.2012.china <-  subset(gi.2012, Destination =='CHINA MAIN')
gi.2012.china.soy <- subset(gi.2012.china, Grain =='SOYBEANS')

gi.2011<- read.csv("https://fgisonline.ams.usda.gov/ExportGrainReport/CY2011.csv")
gi.2011.china <-  subset(gi.2011, Destination =='CHINA MAIN')
gi.2011.china.soy <- subset(gi.2011.china, Grain =='SOYBEANS')

gi.2010<- read.csv("https://fgisonline.ams.usda.gov/ExportGrainReport/CY2010.csv")
gi.2010.china <-  subset(gi.2010, Destination =='CHINA MAIN')
gi.2010.china.soy <- subset(gi.2010.china, Grain =='SOYBEANS')

gi.2009<- read.csv("https://fgisonline.ams.usda.gov/ExportGrainReport/CY2009.csv")
gi.2009.china <-  subset(gi.2009, Destination =='CHINA MAIN')
gi.2009.china.soy <- subset(gi.2009.china, Grain =='SOYBEANS')

gi.2008<- read.csv("https://fgisonline.ams.usda.gov/ExportGrainReport/CY2008.csv")
gi.2008.china <-  subset(gi.2008, Destination =='CHINA MAIN')
gi.2008.china.soy <- subset(gi.2008.china, Grain =='SOYBEANS')

gi.2007<- read.csv("https://fgisonline.ams.usda.gov/ExportGrainReport/CY2007.csv")
gi.2007.china <-  subset(gi.2007, Destination =='CHINA MAIN')
gi.2007.china.soy <- subset(gi.2008.china, Grain =='SOYBEANS')

gi.2006<- read.csv("https://fgisonline.ams.usda.gov/ExportGrainReport/CY2006.csv")
gi.2006.china <-  subset(gi.2006, Destination =='CHINA MAIN')
gi.2006.china.soy <- subset(gi.2006.china, Grain =='SOYBEANS')

gi.2005<- read.csv("https://fgisonline.ams.usda.gov/ExportGrainReport/CY2005.csv")
gi.2005.china <-  subset(gi.2005, Destination =='CHINA MAIN')
gi.2005.china.soy <- subset(gi.2005.china, Grain =='SOYBEANS')

gi.2004<- read.csv("https://fgisonline.ams.usda.gov/ExportGrainReport/CY2004.csv")
gi.2004.china <-  subset(gi.2004, Destination =='CHINA MAIN')
gi.2004.china.soy <- subset(gi.2004.china, Grain =='SOYBEANS')

gi.2003<- read.csv("https://fgisonline.ams.usda.gov/ExportGrainReport/CY2003.csv")
gi.2003.china <-  subset(gi.2003, Destination =='CHINA MAIN')
gi.2003.china.soy <- subset(gi.2003.china, Grain =='SOYBEANS')

gi.2002<- read.csv("https://fgisonline.ams.usda.gov/ExportGrainReport/CY2002.csv")
gi.2002.china <-  subset(gi.2002, Destination =='CHINA MAIN')
gi.2002.china.soy <- subset(gi.2002.china, Grain =='SOYBEANS')

gi.2001<- read.csv("https://fgisonline.ams.usda.gov/ExportGrainReport/CY2001.csv")
gi.2001.china <-  subset(gi.2001, Destination =='CHINA MAIN')
gi.2001.china.soy <- subset(gi.2001.china, Grain =='SOYBEANS')

gi.2000<- read.csv("https://fgisonline.ams.usda.gov/ExportGrainReport/CY2000.csv")
gi.2000.china <-  subset(gi.2000, Destination =='CHINA MAIN')
gi.2000.china.soy <- subset(gi.2000.china, Grain =='SOYBEANS')


# Transform data and get trend for 2000-2018 in order to do Statistical Analysis
gi.china.soy.weekly.2000.to.2018 <- rbind(gi.2018.china.soy, gi.2017.china.soy, gi.2016.china.soy, gi.2015.china.soy, gi.2014.china.soy, gi.2013.china.soy, gi.2012.china.soy, gi.2011.china.soy, gi.2010.china.soy, gi.2009.china.soy, gi.2008.china.soy, gi.2007.china.soy, gi.2006.china.soy, gi.2005.china.soy, gi.2004.china.soy, gi.2003.china.soy, gi.2002.china.soy, gi.2001.china.soy, gi.2000.china.soy)
gi.clean.china.weekly.2000.to.2018 <- gi.china.soy.weekly.2000.to.2018[,1:15]    # Choose only the collumns we are interested in
gi.clean.china.weekly.2000.to.2018 <- cbind(gi.clean.china.weekly.2000.to.2018[1],gi.clean.china.weekly.2000.to.2018[14] ) # Pick only reporting week day and Pounds
summary.weekly.2000.to.2018<- aggregate(Pounds ~ Thursday, gi.clean.china.weekly.2000.to.2018 , sum)  # Aggregate weekly data
series.gi.clean.china.weekly.2000.to.2018 <- ts(summary.weekly.2000.to.2018$Pounds, freq=365.25/7, start=decimal_date(ymd("2000-01-06"))) # convert into time series so we can convert
summary.weekly.2000.to.2018.components <-decompose(series.gi.clean.china.weekly.2000.to.2018)
plot(summary.weekly.2000.to.2018.components,xaxt='n',yaxt='n' )


# Calculate Weekly Change in soy Bean Shipment 2018
gi.china.soy.2000.to.2018 <- rbind(gi.2018.china.soy, gi.2017.china.soy, gi.2016.china.soy, gi.2015.china.soy, gi.2014.china.soy, gi.2013.china.soy, gi.2012.china.soy, gi.2011.china.soy, gi.2010.china.soy, gi.2009.china.soy, gi.2008.china.soy, gi.2007.china.soy, gi.2006.china.soy, gi.2005.china.soy, gi.2004.china.soy, gi.2003.china.soy, gi.2002.china.soy, gi.2001.china.soy, gi.2000.china.soy)
gi.clean.china.2000.to.2018 <- gi.china.soy.2000.to.2018[,1:15]    # Choose only the collumns we are interested in
gi.clean.china.2000.to.2018$Thursday = substr(gi.clean.china.2000.to.2018$Thursday ,1,nchar(gi.clean.china.2000.to.2018$Thursday)-2) # Remove day of the week
gi.clean.china.monthly.2000.to.2018 <- cbind(gi.clean.china.2000.to.2018[1],gi.clean.china.2000.to.2018[14] ) # Pick only reporting week day and Pounds
summary.monthly.2000.to.2018 <- aggregate(Pounds ~ Thursday, gi.clean.china.monthly.2000.to.2018 , sum)  # Aggregate weekly data
monthly.change.2000.2018 <- cbind(summary.monthly.2000.to.2018, Change = round(summary.monthly.2000.to.2018$Pounds/lag(summary.monthly.2000.to.2018$Pounds, 1) -1, 4))

# Find outliers
monthly.change.2000.2018[order(monthly.change.2000.2018$Change),]
# Remove Outliers
monthly.change.2000.2018.remove.outliers <- monthly.change.2000.2018[-c(1, 45,148,160,137,100, 71,27,80, 169,161, 17, 138), ]
monthly.change.2000.2017.remove.outliers <- monthly.change.2000.2018.remove.outliers[1:174,]
monthly.change.2018.remove.outliers <- monthly.change.2000.2018.remove.outliers[177:185,]
monthly.change.2018.remove.outliers
#Hypothesis Testing
mean.2000.to.2017 <- mean(monthly.change.2000.2017.remove.outliers$Change)
mean.2000.to.2017

# Get Statistics for Analysis
sd.2000.to.2017 <- sd(monthly.change.2000.2017.remove.outliers$Change)
sd.2000.to.2017
mean.2018 <- mean(monthly.change.2018.remove.outliers$Change)
mean.2018
sd.2018 <- sd(monthly.change.2018.remove.outliers$Change)
sd.2018

# Main Charts
plot(monthly.change.2000.2017.remove.outliers$Change, breaks = 50, col = "blue", main = "Monthly Change Soy Shipments 2000 - 2017", xlab = paste("Month (Starting Month Jan 2000)", "Mean: " ,round(mean.2000.to.2017, 4)*100, "%", "SD: ", round(sd.2000.to.2017,4) *100, "%"), ylab = "Monthly Change in Percentage Decimals", text(mean.2000.to.2017))
abline(0,0, col = "red")
hist(monthly.change.2000.2017.remove.outliers$Change, breaks = 50, col = "blue", main = "Monthly change Soy Shipments 2000 - 2017", xlab = "Month (Starting Month Jan 2000)", ylab = "Monthly Change in Percentage Decimals")
plot(monthly.change.2018.remove.outliers$Change, breaks = 50, col = "blue", main = "Monthly change Soy Shipments 2018")


       