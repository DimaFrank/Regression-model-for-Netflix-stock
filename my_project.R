################################# My Project ###################################

library(ggplot2)
library(e1071)
library(lmtest)
library(sm)
library(nortest)

prices = read.csv("C:/Users/97252/Desktop/myProject/archive/prices.csv")

#display top 10 rows
head(prices,10)

#num of rows
nrow(prices)

tail(prices,10)

securities = read.csv("C:/Users/97252/Desktop/myProject/archive/securities.csv")

#data for Netflix securities only
data = prices[which(prices$symbol == "NFLX"),] 

nrow(data)

for(i in 1:nrow(data)){
  
  row.names(data)[i] =  i

}



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


par(mfrow = c(1,1))
scatter.smooth(x= clean_data$open, y = clean_data$close, main= "open~close", col = "red" )


for(i in 1:nrow(clean_data)){
  
  row.names(clean_data)[i] =  i
  
}


##############################Linear model######################################


LM  = lm(close ~ open, data = clean_data)
summary(LM)

AIC(reg1)
BIC(reg1)


LM$residuals

par(mfrow = c(1,1))
plot(LM$residuals,LM$fitted.values)
M = mean(LM$residuals)
SD = sd(LM$residuals, na.rm = FALSE)
N = length(clean_data$close)

t_m = -M / SD * sqrt(N)

p_val = 0.95

t_tab = qt(p_val, N -1)

# Hypothesis: 
# H0 - there are correlation of residuals 
# H1 - there is no correlation of residuals

if(abs(t_m) < t_tab){
  
  print("accepting the null hypothesis")
}else{
  print("accepting alternative hypothesis")
}


cor(LM$fitted.values, LM$residuals)



bptest(LM) # There is no heteroskedasticity


bgtest(LM)

qqnorm(reg1$residuals)



sm.density(LM$residuals, model = "normal", xlab = "Reciduals" , ylab = "Frequency")


lillie.test(LM$residuals)
shapiro.test(LM$residuals[1:5000])



RMSE1 = sqrt(sum((test.set$сlose - predict(LM, test.set))^2)/nrow(test.set))


RMSE2 = sqrt(sum((test.set$close - predict(Translinearmodel, test.set))^2)/nrow(test.set))



###########################Log transformation###################################


Translinearmodel  = lm(log(close) ~ log(open), data = clean_data)
summary(Translinearmodel)

AIC(Translinearmodel)
BIC(Translinearmodel)


Translinearmodel$residuals

par(mfrow = c(1,1))
plot(Translinearmodel$residuals,Translinearmodel$fitted.values)
M = mean(Translinearmodel$residuals)
SD = sd(Translinearmodel$residuals, na.rm = FALSE)
N = length(clean_data$close)

t_m = -M / SD * sqrt(N)

p_val = 0.95

t_tab = qt(p_val, N -1)

# Hypothesis: 
# H0 - there are correlation of residuals 
# H1 - there is no correlation of residuals

if(abs(t_m) < t_tab){
  
  print("accepting the null hypothesis")
}else{
  print("accepting alternative hypothesis")
}


cor(Translinearmodel$fitted.values, Translinearmodel$residuals)



bptest(Translinearmodel) # There is no heteroskedasticity


bgtest(Translinearmodel)

qqnorm(Translinearmodel$residuals)



sm.density(Translinearmodel$residuals, model = "normal", xlab = "Reciduals" , ylab = "Frequency")


lillie.test(Translinearmodel$residuals)
shapiro.test(Translinearmodel$residuals[1:5000])


RMSE2 = sqrt(sum((test.set$close - predict(Translinearmodel, test.set))^2)/nrow(test.set))





################################################################################
#Choosing random observations for fitting the model

set.seed(1)
set = sample(1:nrow(clean_data), 100, replace=FALSE)

small_set = clean_data[which(row.names(clean_data) %in% set ),]
plot(small_set$close ~ small_set$open)

nrow(small_set)
summary(small_set)

cor(small_set$close,small_set$open)

scatter.smooth(x= small_set$open, y = small_set$close, main= "open~close", col = "red" )




par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(small_set$open), main="Density Plot: Open Price", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(small_set$open), 2)))  # density plot for 'Open price'
polygon(density(small_set$open), col="red")

plot(density(small_set$close), main="Density Plot: Close Price", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(small_set$close), 2)))  # density plot for 'Close price'
polygon(density(small_set$close), col="green")



#Regression model 

train.size = floor(0.8*nrow(small_set))
train.index = sample(1:nrow(small_set), train.size)
train.set = small_set[train.index,]
test.set = small_set[-train.index,]

nrow(train.set)
nrow(test.set)

# prediction the test based on training 

reg1 = lm(close ~ open, data = train.set)
summary(reg1)

y_pred  <- predict(reg1, test.set)
summary(y_pred)
y_pred


#Calculate prediction accuracy and error rates.


actuals_preds <- data.frame(cbind(actuals=test.set$close, predicteds=y_pred))
actuals_preds


#The Correlation accuracy.
correlation_accuracy <- cor(actuals_preds) 
correlation_accuracy


min_max_accuracy = mean(apply(actuals_preds,1,min) / apply(actuals_preds, 1 , max))


mape = mean(  abs(actuals_preds$predicteds - actuals_preds$actuals)  /actuals_preds$actuals)







predict(reg1, list(open = 32.43), interval = "confidence")


p <- ggplot(test.set, aes(close, open)) +
  geom_point() +
  stat_smooth(method = lm)






