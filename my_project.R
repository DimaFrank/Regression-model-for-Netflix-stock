################################# My Project ###################################

library(ggplot2)
library(e1071)

prices = read.csv("C:/Users/97252/Desktop/archive/prices.csv")

#display top 10 rows
head(prices,10)

#num of rows
nrow(prices)

tail(prices,10)

securities = read.csv("C:/Users/97252/Desktop/archive/securities.csv")

#data for Netflix securities
data = prices[which(prices$symbol == "NFLX"),] 

nrow(data)


#Removing outliers

par(mfrow = c(1,2))

boxplot(data$open, main = "Open price", sub= paste("" ,boxplot.stats(data$open)$out))# box plot for open price variable predictor.
boxplot(data$low, main = "Low price", sub= paste("outlier rows" ,boxplot.stats(data$low)$out))# box plot for Low price variable response.
boxplot(data$high, main = "High price", sub= paste("outlier rows" ,boxplot.stats(data$high)$out))# box plot for High price variable response.
boxplot(data$volume, main = "Volume", sub= paste("outlier rows" ,boxplot.stats(data$volume)$out))# box plot for Volume variable response.


outliers_open = boxplot(data$open, plot = FALSE)$out
outliers_low = boxplot(data$low, plot = FALSE)$out
outliers_high = boxplot(data$high, plot = FALSE)$out
outliers_volume = boxplot(data$volume, plot = FALSE)$out


'%!in%' <- function(x,y)!('%in%'(x,y))


nrow(data)

clean_data = data[which(data$open %!in% outliers_open & data$low %!in% outliers_low & data$high %!in% outliers_high & data$volume %!in% outliers_volume),]


nrow(clean_data)



boxplot(clean_data$open, main = "Open price", sub= paste("Open price clean data" ,boxplot.stats(clean_data$open)$out))
boxplot(clean_data$low, main = "Low price", sub= paste("Low price clean data" ,boxplot.stats(clean_data$low)$out))
boxplot(clean_data$high, main = "High price", sub= paste("High price clean data" ,boxplot.stats(clean_data$high)$out))
boxplot(clean_data$volume, main = "Volume", sub= paste("Volume clean data" ,boxplot.stats(clean_data$volume)$out))


summary(clean_data)


scatter.smooth(x= clean_data$open, y = clean_data$close, main= "open~close", col = "red" )




par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(clean_data$open), main="Density Plot: Open Price", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(clean_data$open), 2)))  # density plot for 'Open price'
polygon(density(clean_data$open), col="red")

plot(density(clean_data$close), main="Density Plot: Close Price", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(clean_data$close), 2)))  # density plot for 'Close price'
polygon(density(clean_data$close), col="green")



cor(x = clean_data$open, y = clean_data$close)

cor(x = clean_data$high, y = clean_data$close)

cor(x = clean_data$low,  y = clean_data$close)

cor(x = clean_data$volume, y = clean_data$close)




#Regression model 

train.size = floor(0.7*nrow(clean_data))
train.index = sample(1:nrow(clean_data), train.size)
train.set = clean_data[train.index,]
test.set = clean_data[-train.index,]



reg1 = lm(close ~ open + volume + low + high , data = train.set)
summary(reg1)



#Log transformation


Translinearmodel <- lm(log(close)~log(open)+log(volume)+log(low)+log(high), data = train.set)

summary(Translinearmodel)



# prediction the test based on training data.

y_pred  <- predict(reg1, test.set)
summary(y_pred)
y_pred




#Calculate prediction accuracy and error rates.


actuals_preds <- data.frame(cbind(actuals=test.set$close, predicteds=y_pred))
actuals_preds



#The Correlation accuracy.
correlation_accuracy <- cor(actuals_preds) 
correlation_accuracy


predict(reg1, list(open = 53.62, volume = 6783700,low = 52.70, high = 53.93), interval = "confidence")








