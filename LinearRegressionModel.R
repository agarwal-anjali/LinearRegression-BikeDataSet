# Set the working directory below as the driectory where the data set is stored.
setwd("")
data = read.csv("day.csv")
attach(data)

#PART 1:

# RESPONSE VARIABLE: cnt
# REGRESSORS: season, workingday, weathersit, temp, hum and windspeed
weather = ifelse(weathersit == 1, "Good", "Bad")
seasons = ifelse(season == 1 | season == 2, "Summer", "Winter")
workday = ifelse(workingday == 1, "Yes", "No")

#Q1
summary(cnt)
hist(cnt, col=4) #symmetric distribution
boxplot(cnt, ylab= "Frequency", xlab= "Count of daily total rental bike", col= 2, main = "Boxplot of cnt")
abline(h=median(cnt), col= 5)


#Q2
cor(cnt,temp) #positive and weak association
plot(cnt~temp) #linear, constant variance
hist(temp) #multimodal

cor(cnt,hum) #negative and weak association
plot(cnt~hum) #linear, constant variance violated
hist(hum) #left skewed

cor(cnt, windspeed) #negative and weak association
plot(cnt~windspeed) #linear, variance might be constant
hist(windspeed) #right skewed

#IQR is almost same, median of winter> summer season
boxplot(cnt~seasons,ylab="Season", xlab="Total daily rental",, main="Boxplot: season vs cnt", col=2) 

#IQR range of 0>1, spread of data 0>1, median alomost same, 1 had median slightly higher than 0
boxplot(cnt~workday, col=2, ylab="Working day", xlab="Total daily rental",, main="Boxplot: workingday vs cnt") 

#IQR is almost same, median of good weather>bad weather
boxplot(cnt~weather, ylab="Weather", xlab="Total daily rental",, main="Boxplot: weathersit vs cnt", col=2)

#PART 2:

#Q3
#MODEL 1
M1= lm(cnt~seasons+workday+weather+temp+hum+windspeed, data=data)
summary(M1) # adj r sqr = 0.4732, workday is insignificant

raw.res = M1$res
SR = rstandard(M1)

which(SR>3 | SR<(-3)) #one outlier at 69
c = cooks.distance(M1) 
which(c>1) #no influential points

hist(SR, prob=TRUE)#slightly left skewed, normality violated

qqnorm(SR, datax = TRUE) #normality violated, tails are slightly shorter than normal
qqline(SR, datax = TRUE, col="red")

#PLOTTING PREDICTED Y^ VS SR TO CHECK FOR CONSTANT VARIANCE AND NORMALITY
plot(M1$fitted.values, SR) #slightly funnel shaped, equal variance violated
abline(h=0, col="red") 

#PLOTTING EACH X AGAINST SR TO CHECK FOR X'S LINEARITY ASSUMPTION
plot(data$temp, SR) #linearity might be violated
abline(h=3, col="red")
abline(h=(-3), col="red")

plot(data$hum, SR) #linearity check of hum
abline(h=3, col="red")#might be linear
abline(h=(-3), col="red")

plot(data$windspeed, SR) #linearity check of windspeed
abline(h=3, col="red") #might be linear
abline(h=(-3), col="red")

#Workingday is insignificant
#There is one outlier which is not influential
#The model is not adequate due to the violations of the assumptions
#There might interaction terms among the regressors


#MODEL 2
#Since we added interaction terms of workday with other regressors, workday is still a regressor
#Added interaction terms
M2 = lm(cnt~seasons+weather+hum+temp+windspeed+workday+
            seasons*hum+seasons*temp+seasons*windspeed+
            weather*windspeed+weather*temp+weather*hum+
            hum*workday+hum*temp+hum*windspeed+
            temp*windspeed+temp*workday+
            windspeed*workday+
            seasons*weather+seasons*workday+weather*workday, data=data)
summary(M2)
# Adjusted R^2 = 0.5454
# Interation terms that are insignificant:
# seasonsWinter*windspeed, weatherGood*temp, seasonsWinter*hum, hum*workdayYes,hum*temp,hum*windspeed,
# temp*windspeed, workdayYes*windspeed, seasonsWinter*weatherGood and seasonsWinter*workdayYes

#MODEL 3
#Removed all insignificant regressors
M3 = lm(cnt~seasons+weather+hum+temp+windspeed+workday+
            seasons*temp+
            weather*windspeed+weather*hum+
            temp*workday+
            weather*workday, data=data)
summary(M3)
#Adjusted R^2 = 0.5485
#hum and windspeed are insignificant but since they are involved in interaction terms, did not remove them
#temp*workingday is insignificant
#Modifying model 3 itself and removing the insignificant term

M3 = lm(cnt~seasons+weather+hum+temp+windspeed+workday+
            seasons*temp+
            weather*windspeed+weather*hum+
            weather*workday, data=data)
summary(M3)

raw.res = M3$res
SR3 = rstandard(M3)

which(SR3>3 | SR3<(-3)) #no outliers
c3 = cooks.distance(M3) 
which(c3>1) #no influential points

hist(SR3, prob=TRUE) #slighlt left skewed

qqnorm(SR3, datax = TRUE) #normality violated, tails are slightly shorter than normal
qqline(SR3, datax = TRUE, col="red")

plot(M3$fitted.values, SR3) #slightly funnel shaped, equal variance might be violated
abline(h=0, col="red")

#PLOTTING EACH X AGAINST SR 
plot(data$temp, SR3) #constant variance might be violated
abline(h=3, col="red")
abline(h=(-3), col="red")

plot(data$hum, SR3) 
abline(h=3, col="red")
abline(h=(-3), col="red")

plot(data$windspeed, SR3) 
abline(h=3, col="red") 
abline(h=(-3), col="red")

#Adjusted R^2 = 0.5524
#Model is not adequate due to the violations
#Doing transformations to the response and regressors might help in eliminating the violations


#MODEL 4
n_hum = hum^3 #for higher powers the scatterplot might be funnel shaped
n_wind = windspeed^2
n_cnt = log(cnt)
n_temp = log(temp)
M4 = lm(n_cnt~seasons+weather+workday+n_wind+n_hum+n_temp+weather*n_wind+seasons*n_temp+weather*n_hum+weather*workday, data=data)
summary(M4)

raw.res = M4$res
SR4 = rstandard(M4)

which(SR4>3 | SR4<(-3)) #outliers at 2, 27, 69, 668
c4 = cooks.distance(M4) 
which(c4>1) #no influential point

hist(SR4, prob=TRUE) #highly left skewed, might be due to outliers

qqnorm(SR4, datax = TRUE) #left tail is longer & right tail is almost normal, normality violated, 
qqline(SR4, datax = TRUE, col="red")

#PLOTTING PREDICTED Y^ VS SR TO CHECK FOR CONSTANT VARIANCE AND NORMALITY
plot(M4$fitted.values, SR4) #normality violated
abline(h=0, col="red")

#PLOTTING EACH X AGAINST SR TO CHECK FOR X'S LINEARITY ASSUMPTION
plot(n_temp, SR4) #linear
abline(h=3, col="red")
abline(h=(-3), col="red")

plot(n_hum, SR4) #linearity check of hum
abline(h=3, col="red")#linear
abline(h=(-3), col="red")

plot(n_wind, SR4) #linearity check of windspeed
abline(h=3, col="red") #linear
abline(h=(-3), col="red")

#Adjusted R^2 = 0.5976
#After the transformations the adjusted R^2 increased and it also elimininated the linearity violations of the regressors
#Still the model is not adequate
#Suspected reason is due to outliers


#MODEL 5
new_data = data[-c(1,239,2,27,69,668, 302,328, 341, 358, 359, 669, 726, 299, 325, 338, 355, 356, 665, 722 ),]
n_hum = (new_data$hum)^3 #for higher powers the scatterplot might be funnel shaped
n_wind = new_data$windspeed
n_cnt = log(new_data$cnt)
n_temp = log(new_data$temp)
n_season = seasons[-c(1,239,2,27,69,668, 302,328, 341, 358, 359, 669, 726, 299, 325, 338, 355, 356, 665, 722 )]
n_weathersit = weather[-c(1,239,2,27,69,668, 302,328, 341, 358, 359, 669, 726, 299, 325, 338, 355, 356, 665, 722 )]
n_workingday = workday[-c(1,239,2,27,69,668, 302,328, 341, 358, 359, 669, 726, 299, 325, 338, 355, 356, 665, 722 )]
M5 = lm(n_cnt~n_season+n_weathersit+n_workingday+n_wind+n_hum+n_temp+n_weathersit*n_wind+n_season*n_temp+n_season*n_hum+n_weathersit*n_hum+n_weathersit*n_workingday, data=new_data)
summary(M5)

raw.res = M5$res
SR5 = rstandard(M5)

which(SR5>3 | SR5<(-3)) #outliers at 2, 239, 358, 669, 668, 667, 662, 353 (removed this from new_data and modified M5)
c5 = cooks.distance(M5) 
which(c5>1) #no influential point

hist(SR5, prob=TRUE) #slighlty left skewed, might be due to outliers

qqnorm(SR5, datax = TRUE) #both tails are slighlty shorter than normal, normality slighlty violated
qqline(SR5, datax = TRUE, col="red")

#PLOTTING PREDICTED Y^ VS SR TO CHECK FOR CONSTANT VARIANCE AND NORMALITY
plot(M5$fitted.values, SR5) #constant variance but normality is minimally violated
abline(h=0, col="red")

#PLOTTING EACH X AGAINST SR TO CHECK FOR X'S LINEARITY ASSUMPTION
plot(n_temp, SR5) #linear, constant variance
abline(h=3, col="red")
abline(h=(-3), col="red")

plot(n_hum, SR5) #linearity check of hum
abline(h=3, col="red")#linear, constant variance
abline(h=(-3), col="red")

plot(n_wind, SR5) #linearity check of windspeed
abline(h=3, col="red") #linear, constant variance
abline(h=(-3), col="red")

#After removing outliers calculated from M4
#This model had an Adjusted R^2 = 0.6613 and almost all major violations eliminated
#Thus, removed more outliers, found in M5, from new_data and modified M5 itself
#There are some insignificant regressors in the model but did not remove them for better estimation of Y^

#At last, the model has no outliers and almost all violations are eliminated from the model
#So, M5 is the final model that has:
#F-statistic = 127 and a null distribution of F(11,699)
#Residual standard error = 0.2981
#P-value of F-test < 0.05
#Adjusted R^2 = 0.6613, suggests that the model is good
#Thus, the model is almost adequate and significant 







