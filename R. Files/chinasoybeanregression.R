install.packages("lme4")
install.packages("jtools ")
install.packages("gridExtra")
install.packages("lmtest")
install.packages("arm")
library(ggplot2)
library(lme4)
library(jtools)
library(gridExtra)
library(lmtest)
library(arm)

# Need to change this to where the regression Analysis file is
setwd("C:/Users/josbe/Documents")

# regresion of soybean exports
regression.data <- read.csv("chinasoybeanregression.csv")

# Vsualization of the variables
Dependent1 <- ggplot(data= regression.data, aes(y = regression.data$EXPORTTOTALUS, x = regression.data$EXPORTSUSTOCHINA)) + geom_point() + geom_smooth(method = 'lm', se = F) + xlab("Export Soy US to China") + ylab("Total Exports Soy US to World") +  theme(axis.text.x=element_blank(), axis.text.y=element_blank())
Dependent2 <- ggplot(data= regression.data, aes(y = regression.data$GAS, x = regression.data$EXPORTSUSTOCHINA)) + geom_point() + geom_smooth(method = 'lm', se = F) + xlab("Export Soy US to China") + ylab("Gas Price") +  theme(axis.text.x=element_blank(), axis.text.y=element_blank())
Dependent2 <- ggplot(data= regression.data, aes(y = regression.data$USTOTALPROD, x = regression.data$EXPORTSUSTOCHINA)) + geom_point() + geom_smooth(method = 'lm', se = F) + xlab("Export Soy US to China") + ylab("US Total Soy Production") +  theme(axis.text.x=element_blank(), axis.text.y=element_blank())
Dependent3 <- ggplot(data= regression.data, aes(y = regression.data$PHDI, x = regression.data$EXPORTSUSTOCHINA)) + geom_point() + geom_smooth(method = 'lm', se = F) + xlab("Export Soy US to China")  + ylab("PHDI Weather Index") +  theme(axis.text.x=element_blank(), axis.text.y=element_blank())
Dependent4 <- ggplot(data= regression.data, aes(y = regression.data$FOREX, x = regression.data$EXPORTSUSTOCHINA)) + geom_point() + geom_smooth(method = 'lm', se = F) + xlab("Export Soy US to China")  + ylab("Yuan-USD Exchange Rate") +  theme(axis.text.x=element_blank(), axis.text.y=element_blank())
Dependent5 <- ggplot(data= regression.data, aes(y = regression.data$CHINASOYEXPORTOWORLD, x = regression.data$EXPORTSUSTOCHINA)) + geom_point() + geom_smooth(method = 'lm', se = F) + xlab("Export Soy US to China") + ylab("China total Soy Export") +  theme(axis.text.x=element_blank(), axis.text.y=element_blank())
grid.arrange(Dependent1, Dependent2, Dependent3, Dependent4, Dependent5,nrow = 2)

# Correlation to exclude Total Exports
cor(regression.data$EXPORTTOTALUS, regression.data$USTOTALPROD,  method = c("pearson", "kendall", "spearman"))
cor(regression.data$FOREX, regression.data$USTOTALPROD,  method = c("pearson", "kendall", "spearman"))

# Regression version 2 removed EXPORTSUSTOCHINA
soy.exports.from.us.to.china.regression.v2 <- lm(formula = EXPORTSUSTOCHINA ~ GAS + USTOTALPROD + PHDI + FOREX + CHINASOYEXPORTOWORLD, data = regression.data)
summary(soy.exports.from.us.to.china.regression.v2)
coefplot(soy.exports.from.us.to.china.regression.v2, cex.pts = 1.5, col.pts = 2, h.axis = F)
plot(soy.exports.from.us.to.china.regression.v2$residuals, col="blue", pch = 25, cex=1.5, bg="blue")


