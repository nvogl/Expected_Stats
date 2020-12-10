#pull in data
setwd("C:/Users/Nick/Documents/MLB/Expected_Stats")
expected_stats <- read.csv("expected_stats_2017.csv")
shift_stats <- read.csv("shift_stats_2017.csv")
postdata <- read.csv("expected_stats_2018.csv")



#add libraries
#install.packages("dplyr")
library("dplyr")
library("MLmetrics")



#organize data
expected_stats$batter <- paste(expected_stats$last_name, expected_stats$first_name, sep = ", ")
data <- inner_join(expected_stats, shift_stats, by = "batter")
postdata$batter <- paste(postdata$last_name, postdata$first_name, sep = ", ")
postdata$postba <- postdata$batting_avg
postdata$postslg <- postdata$slg_percent
postdata$postwoba <- postdata$woba
postdata <- postdata[,c(16:19)]
data <- left_join(data, postdata, by = "batter")



#initial scatter plots
#avg
plot(data$shift_percent, data$xbadiff, xlab = "Shift %", ylab = "xBA - BA", 
     main = "Shift % vs xBA - BA")
abline(lsfit(data$shift_percent,data$xbadiff))
#slg
plot(data$shift_percent, data$xslgdiff, xlab = "Shift %", ylab = "xSLG - SLG", 
     main = "Shift % vs xSLG - SLG")
abline(lsfit(data$shift_percent,data$xslgdiff))
#woba
plot(data$shift_percent, data$wobadif, xlab = "Shift %", ylab = "xwOBA - wOBA", 
     main = "Shift % vs xwOBA - wOBA")
abline(lsfit(data$shift_percent,data$wobadif))



#building model for avg
model1.1 <- lm(xbadiff ~ shift_percent, data = data)
summary(model1.1)
plot(predict(model1.1), data$xbadiff, xlab = "predicted", ylab = "actual", 
     main = "predicted vs actual")
plot(model1.1)

#building model for slg
model2.1 <- lm(xslgdiff ~ shift_percent, data = data)
summary(model2.1)
plot(predict(model2.1), data$xslgdiff, xlab = "predicted", ylab = "actual", 
     main = "predicted vs actual")
plot(model2.1)

#building model for woba
model3.1 <- lm(wobadif ~ shift_percent, data = data)
summary(model3.1)
plot(predict(model3.1), data$wobadif, xlab = "predicted", ylab = "actual", 
     main = "predicted vs actual")
plot(model3.1)



#add new variables to the dataset
data$xsba <- data$xba + predict(model1.1)
data$xsslg <- data$xslg + predict(model2.1)
data$xswoba <- data$xwoba + predict(model3.1)



#remove batters that did not qualify both years
data <- na.omit(data)



#make plots of prior year predictions vs next year results
#avg vs avg
plot(data$batting_avg, data$postba, xlab = "Prior AVG", ylab = "AVG",
     main = "Prior Year AVG vs Current Year AVG")
MSE(data$batting_avg, data$postba)
#xba vs avg
plot(data$xba, data$postba, xlab = "Prior xBA", ylab = "AVG",
     main = "Prior Year AVG vs Current Year AVG")
MSE(data$xba, data$postba)
#xsba vs avg
plot(data$xsba, data$postba, xlab = "Prior xsBA", ylab = "AVG",
     main = "Prior Year AVG vs Current Year AVG")
MSE(data$xsba, data$postba)

#slg vs slg
plot(data$slg_percent, data$postslg, xlab = "Prior SLG", ylab = "SLG",
     main = "Prior Year SLG vs Current Year SLG")
MSE(data$slg_percent, data$postslg)
#xslg vs slg
plot(data$xslg, data$postslg, xlab = "Prior xSLG", ylab = "SLG",
     main = "Prior Year xSLG vs Current Year SLG")
MSE(data$xslg, data$postslg)
#xsslg vs slg
plot(data$xsslg, data$postslg, xlab = "Prior xsSLG", ylab = "SLG",
     main = "Prior Year xsSLG vs Current Year SLG")
MSE(data$xsslg, data$postslg)

#woba vs woba
plot(data$woba, data$postwoba, xlab = "Prior wOBA", ylab = "wOBA",
     main = "Prior Year wOBA vs Current Year wOBA")
MSE(data$woba, data$postwoba)
#xwoba vs woba
plot(data$xwoba, data$postwoba, xlab = "Prior xwOBA", ylab = "wOBA",
     main = "Prior Year xwOBA vs Current Year wOBA")
MSE(data$xwoba, data$postwoba)
#xswoba vs woba
plot(data$xswoba, data$postwoba, xlab = "Prior xswOBA", ylab = "wOBA",
     main = "Prior Year xswOBA vs Current Year wOBA")
MSE(data$xswoba, data$postwoba)