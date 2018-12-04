install.packages('tidyverse')
install.packages('tidyr')
install.packages("TTR")
library(tidyverse)
library(lubridate)
library(ggplot2)
library(TTR)

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

# Merge all data into one data frame to 
gi.china.soy.2008.to.2018 <- rbind(gi.2018.china.soy, gi.2017.china.soy, gi.2016.china.soy, gi.2015.china.soy, gi.2014.china.soy, gi.2013.china.soy, gi.2012.china.soy, gi.2011.china.soy, gi.2011.china.soy)
gi.clean.china <- gi.china.soy.2008.to.2018[,1:15]
write.csv(gi.clean.china, 'ChinaSoy.csv')



# Need to free space on R
rm(list=ls())
# Load Data from Compputer
gi.clean.china <- read.csv('ChinaSoy.csv')
gi.clean.china



# Tidyverse command
rownames(gi.clean.china) <- NULL
weeklyTotals <- gi.clean.china %>% select(Thursday, Pounds) %>% group_by(Thursday)
weeklyTotals

# Weekly Data upload since Tidyverse is not loading had to run pivot table 
gi.clean.china <- read.csv('ChinaSoyWeekly.csv')
series.gi.clean.china <- ts(gi.clean.china$Pounds_Soybeans, freq=365.25/7, start=decimal_date(ymd("2011-01-06")))
plot.ts(series.gi.clean.china, col= 'blue', main = "Weekly Soybean Grain Inspections China", yaxt ="n") 
axis(side=2,at=c(0,5000000000/2,5000000000), labels = c("0", "2.5 Trillion", "5 Trillion"))

# Explore the Monthly Average
gi.china.soySMA4 <- SMA(series.gi.clean.china, n=4)
plot.ts(gi.china.soySMA4)

# Explore the Quarterly Average
gi.china.soySMA53 <- SMA(series.gi.clean.china, n=53)
plot.ts(gi.china.soySMA53, main= "Average Grain Exports 52 Week Interval", col= 'red', ylab='Soybean pounds')

# Seasonal and Unseasonal components
china.soy.components <- decompose(series.gi.clean.china)
plot(china.soy.components)

#Filter China Value
grain.inspection.200.to.2018.china <- grain.inspection.200.to.2018.all[grain.inspection.200.to.2018.all$Destination == "CHINA MAIN"]

# Remove dupicate rows
grain.inspection.200.to.2018.all <- unique(grain.inspection.200.to.2018.duplicates.all)