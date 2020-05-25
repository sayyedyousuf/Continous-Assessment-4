
Crime_Ireland <- read.csv("FinalCrimeDataset.csv", header = TRUE)

crime_multiple <- data.frame(Crime_Ireland$Year, Crime_Ireland$TotalCrime, Crime_Ireland$Unemployment.Rate...., Crime_Ireland$Alcohol.Litres..per.capita.over.15)

library(plyr)

crime_multiple <- rename(crime_multiple, c("Crime_Ireland.Year"="Year"))

crime_multiple <- rename(crime_multiple, c("Crime_Ireland.TotalCrime"="TotalCrime"))

crime_multiple <- rename(crime_multiple, c("Crime_Ireland.Unemployment.Rate...."="UnemploymentRate"))

crime_multiple <- rename(crime_multiple, c("Crime_Ireland.Alcohol.Litres..per.capita.over.15"="AlcoholPerCapitaOver"))

rownames(crime_multiple) <- crime_multiple$Year


crime_multiple <- subset(crime_multiple, select = c(2,3,4,1))

set.seed(1)
no_rows_data <- nrow(crime_multiple)
sample <- sample(1:no_rows_data, size = round(0.7 * no_rows_data), replace = FALSE)

training_data <- crime_multiple[sample, ]
testing_data <-  crime_multiple[-sample,]


fit <- lm( TotalCrime ~ UnemploymentRate + AlcoholPerCapitaOver, data=training_data)

summary(fit)

confint(fit)

library(car)
qqPlot(fit, labels=row.names(Crime_multiple), id.method="identify", simulate=TRUE, main="Q-Q Plot")

training_data["2009",]
training_data["2016",]

fitted(fit)["2009"]
fitted(fit)["2016"]

student_fit <- rstudent(fit)
hist(student_fit, 
     breaks=10, 
     freq=FALSE, 
     xlab="Studentized Residual", 
     main="Distribution of Errors")

rug(jitter(student_fit), col="brown")

curve(dnorm(x, mean=mean(student_fit), sd=sd(student_fit)), add=TRUE, col="blue", lwd=2)

lines(density(student_fit)$x, density(student_fit)$y, col="red", lwd=2, lty=2)

legend("topright", legend = c( "Normal Curve", "Kernel Density Curve"), lty=1:2, col=c("blue","red"), cex=.3)

library(car)
outlierTest(fit)

# Remove year 2016 record
# from crime_multiple dataset
crime_multiple <- subset(crime_multiple, crime_multiple$Year != "2016")

set.seed(1)
no_rows_data <- nrow(crime_multiple)
sample <- sample(1:no_rows_data, size = round(0.7 * no_rows_data), replace = FALSE)

training_data <- crime_multiple[sample, ]
testing_data <- crime_multiple[-sample, ]


fit <- lm( TotalCrime ~ UnemploymentRate + AlcoholPerCapitaOver, data=training_data)

outlierTest(fit)



crime_multiple <- subset(crime_multiple, crime_multiple$Year != "2009")

set.seed(1)
no_rows_data <- nrow(crime_multiple)
sample <- sample(1:no_rows_data, size = round(0.7 * no_rows_data), replace = FALSE)

training_data <- crime_multiple[sample, ]
testing_data <- crime_multiple[-sample, ]


fit <- lm( TotalCrime ~ UnemploymentRate + AlcoholPerCapitaOver, data=training_data)

outlierTest(fit)


student_fit <- rstudent(fit)
hist(student_fit, 
     breaks=10, 
     freq=FALSE, 
     xlab="Studentized Residual", 
     main="Distribution of Errors")

rug(jitter(student_fit), col="brown")

curve(dnorm(x, mean=mean(student_fit), sd=sd(student_fit)), add=TRUE, col="blue", lwd=2)

lines(density(student_fit)$x, density(student_fit)$y, col="red", lwd=2, lty=2)

legend("topright", legend = c( "Normal Curve", "Kernel Density Curve"), lty=1:2, col=c("blue","red"), cex=.3)


crPlots(fit)

cutoff <- 4/(nrow(training_data) - length(fit$coefficients) - 2)
plot(fit, which = 4, cook.levels = cutoff)
abline(h = cutoff, lty = 2, col = "red")

avPlots(fit, ask=FALSE)


library(car)
influencePlot(fit, main="Influence Plot",
              sub="Circle size is proportional to Cook's distance")


ncvTest(fit)


spreadLevelPlot(fit)

library(gvlma)
gvmodel <- gvlma(fit)
summary(gvmodel)

library(car)
vif(fit)

sqrt(vif(fit)) > 2


#summary(powerTransform(training_data$TotalCrime))

#sqrt_transform_crime <- sqrt(training_data$TotalCrime)

#training_data$TotalCrime_sqrt <- sqrt_transform_crime


#fit <- lm( TotalCrime ~ UnemploymentRate + AlcoholPerCapitaOver, data=training_data)
#fit_model2 <- lm(TotalCrime_sqrt ~ UnemploymentRate + AlcoholPerCapitaOver, data=training_data)
#AIC(fit_model1,fit_model2)

#spreadLevelPlot(fit_model2)

library(MASS)

fit_test <- lm( TotalCrime ~ UnemploymentRate + AlcoholPerCapitaOver, data=training_data)

stepAIC(fit_test, direction="backward")



library(leaps)
leaps <-regsubsets(TotalCrime ~ UnemploymentRate + AlcoholPerCapitaOver, data=training_data, nbest=4)
plot(leaps, scale="adjr2")



#library(MASS)
#fit_test <- lm(TotalCrime ~ UnemploymentRate + AlcoholPerCapitaOver, data=training_data)
#stepAIC(fit_test, direction="backward")

#library(leaps)
#leaps <-regsubsets(TotalCrime ~ UnemploymentRate + AlcoholPerCapitaOver, data=training_data, nbest=4)
#plot(leaps, scale="adjr2")


#fit_model <- lm(TotalCrime ~ UnemploymentRate + AlcoholPerCapitaOver, data=training_data)
#fit_model_sqrt <- lm(TotalCrime ~ UnemploymentRate + AlcoholPerCapitaOver, data=training_data)


predicted_crime <- predict(fit_model, testing_data)
predicted_crime_sqrt <- predict(fit_model_sqrt, testing_data)
converted_crime_sqrt <- predicted_murder_sqrt ^2

actuals_predictions <- data.frame(cbind(actuals = testing_data$TotalCrime, predicted = predicted_crime))
head(actuals_predictions)

actuals_predictions_sqrt <-  data.frame(cbind(actuals = testing_data$TotalCrime, predicted = converted_crime_sqrt))
head(actuals_predictions_sqrt)


correlation_accuracy <- cor(actuals_predictions)
correlation_accuracy


correlation_accuracy <- cor(actuals_predictions_sqrt)
correlation_accuracy

min_max_accuracy <- mean(apply(actuals_predictions, 1, min) / apply(actuals_predictions, 1, max))
min_max_accuracy

min_max_accuracy <- mean(apply(actuals_predictions_sqrt, 1, min) / apply(actuals_predictions_sqrt, 1, max))
min_max_accuracy

sigma(fit_model)/ mean(testing_data$TotalCrime)

sigma(fit_model_sqrt)/ mean(testing_data$TotalCrime)

summary(crime_multiple)

df <- data.frame(UnemploymentRate = c(10), AlcoholPerCapitaOver=c(25))
predicted_crime <- predict(fit_model, df)
predicted_crime

df <- data.frame(UnemploymentRate = c(100), AlcoholPerCapitaOver=c(250))
predicted_crime <- predict(fit_model, df)
predicted_crime





