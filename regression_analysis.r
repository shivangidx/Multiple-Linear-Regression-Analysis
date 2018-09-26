
#Load data

#check working directory
getwd()
#set working directory
setwd("E:/")
#load file in csv format
data<-read.csv("Life Expectancy Data.csv")

#Explore data

#display first 6 rows
head(data)
#check structure of data
str(data)

#rename variable names
names(data)[c(4,5,6,8,9,12,14,16,19,20,21)]<-c("Life_expectancy","Adult_Mortality","Infant_deaths",
                           "Percentage_expenditure","Hepatitis_B","Under_5_deaths",
                           "Total_expenditure","HIV_AIDS","Thinness_1_to_19_yrs","Thinness_5_to_9_yrs",
                           "Income_composition_of_resources")

#check summary of data
summary(data)

#Clean data

#keeping original data safe
data1<-data

#display no. of missing values column wise in decreasing order
sort(colSums(is.na(data1)),decreasing = TRUE)

#replace missing values with mean

data1$Life_expectancy[is.na(data1$Life_expectancy)]<-mean(data1$Life_expectancy,na.rm=T)
data1$Adult_Mortality[is.na(data1$Adult_Mortality)]<-mean(data1$Adult_Mortality,na.rm=T)
data1$Alcohol[is.na(data1$Alcohol)]<-mean(data1$Alcohol,na.rm=T)
data1$Hepatitis_B[is.na(data1$Hepatitis_B)]<-mean(data1$Hepatitis_B,na.rm=T)
data1$BMI[is.na(data1$BMI)]<-mean(data1$BMI,na.rm=T)
data1$Polio[is.na(data1$Polio)]<-mean(data1$Polio,na.rm=T)
data1$Total_expenditure[is.na(data1$Total_expenditure)]<-mean(data1$Total_expenditure,na.rm=T)
data1$Diphtheria[is.na(data1$Diphtheria)]<-mean(data1$Diphtheria,na.rm=T)
data1$GDP[is.na(data1$GDP)]<-mean(data1$GDP,na.rm=T)
data1$Population[is.na(data1$Population)]<-mean(data1$Population,na.rm=T)
data1$Thinness_1_to_19_yrs[is.na(data1$Thinness_1_to_19_yrs)]<-mean(data1$Thinness_1_to_19_yrs,na.rm=T)
data1$Thinness_5_to_9_yrs[is.na(data1$Thinness_5_to_9_yrs)]<-mean(data1$Thinness_5_to_9_yrs,na.rm=T)
data1$Income_composition_of_resources[is.na(data1$Income_composition_of_resources)]<-mean(data1$Income_composition_of_resources,na.rm=T)
data1$Schooling[is.na(data1$Schooling)]<-mean(data1$Schooling,na.rm=T)

#check for any missing value column wise
colSums(is.na(data1))

#Analyze data

#display dimensions of data
dim(data1)
#list type of each variable
sapply(data1,class)

#checking distribution of variables
table(data1$Status)
table(data1$Year)
head(table(data1$Country))

#check correlation between different variables
cor(data1[,4:22])
library(corrplot)
corrplot(cor(data1[,4:22]))

#Visualize data

#check distribution of dependent variable i.e. Life.expectancy-histogram
hist(data1[,4], main="Histogram of Life expectancy")

#check distribution of independent variables-density plot
par(mfrow=c(3,6))
for(i in 5:22){
  hist(data1[,i],main=names(data1)[i],ylab = "Frequency",
       xlab=paste("",names(data1)[i]))
}

#Scatterplot of independent vriables against Life expectancy
par(mfrow=c(3,6))
for(i in 5:22){
  plot(data1[,i],data1$Life.expectancy,xlab=paste("",names(data1)[i]),
       ylab="Life expectancy")
}


#Remove Year variable
data2<-data1[,c(-2)]
dim(data2)
str(data2)

#Do one hot encoding for Status variable
for(i in unique(data2$Status)){
  data2[paste("Status",i)]<-ifelse(data2$Status==i,1,0)
}
dim(data2)

#Remove Status variable
data2<-data2[,c(-2)]
str(data2)

#find mean of all variables country wise
library(dplyr)

a<-data2 %>%
  group_by(Country) %>%
  summarise_all(funs(mean))

a
data3<-a

head(data3)

#Remove Country variable
data4<-data3[,c(-1)]
head(data4)

library(caret)

#normalizing the variables
preproc1 <- preProcess(data4, method = c("range"))

#Applying normalization to data
library(RANN)
data_processed1 <- predict(preproc1, data4)
head(data_processed1)

#Splitting data into train and test set
train<-data_processed1[1:150,]
test<-data_processed1[151:193,]

dim(train)
dim(test)

#Fit multiple linear regression model, excluding Country variable
model1<-lm(Life_expectancy~.,data=train)

#check summary of model
summary(model1)

#apply model on test data
testing<-predict(model1,test)

#comapre actual and predicted values of Life_expectancy
head(cbind(test$Life_expectancy,testing))

#data_processed1

#Fit model by removing Status variable because its not meaningful to add it

#Removing Status variables
data5<-data4[,c(-20,-21)]
head(data5)

#normalizing variables
preproc2 <- preProcess(data5, method = c("range"))

#Applying normalization to data
library(RANN)
data_processed2 <- predict(preproc2, data5)
head(data_processed2)
str(data_processed2)

#Splitting data into train and test set
train1<-data_processed2[1:150,]
test1<-data_processed2[151:193,]

#check dimensions of train and test set
dim(train1)
dim(test1)

#Fit multiple linear regression model, excluding Country variable
model2<-lm(Life_expectancy~.,data=train1)

#check summary of model
summary(model2)

#apply model on test data
testing1<-predict(model2,test1)

#comapre actual and predicted values of Life_expectancy
head(cbind(test$Life.expectancy,testing1))

#combine Counttry name, actual and predicted values of life expectancy
cbind(data3[151:193,"Country"],test$Life_expectancy,testing1)

#denormalized<-(data_processed1$Life.expectancy)*(max(data4$Life.expectancy)-min(data4$Life.expectancy))+min(data4$Life.expectancy)

#Denormalizing the life expectancy values so that we can compare predicted values with original values
normalize<-test$Life_expectancy

#denormalization formula for original values
denormalize<-function(x){
  fun<- normalize*(max(x)-min(x))+min(x)
  return (fun)
}

normalize1<-testing1

#denormalization formula for predicted values
denormalize1<-function(x){
  fun1<- normalize1*(max(x)-min(x))+min(x)
  return (fun1)
}

#Denormalized values of Actual and predicted life expectancy
denormalize_original<-denormalize(data4$Life_expectancy)
denormalize_original
denormalize_predicted<-denormalize1(data4$Life_expectancy)
denormalize_predicted

#Combine denormalized values of actual and predicted life expectancy
cbind(data3[151:193,"Country"],denormalize_original,denormalize_predicted)

#summary of model2
summary(model2)

#predicted values of Life expectancy by model
head(fitted(model2))

#difference between actual and predicted values of life expectancy by model
head(residuals(model2))

#The multiple R-squared (0.9336) indicates that the model accounts for 93.3 percent of the variance in
#expectancy. The multiple R-squared is also the correlation between the actual and predicted value.

#The residual standard error (0.06) can be thought of as the average error in predicting life 
#expectancy from independent variables using this model.

#F-statistic tests whether the predictor variables taken together, predict the response variable.

#summary() function provides no information that whether we've satisfied the statistical assumptions
#underlying the model.

#checking confidence interval of model
confint(model2)

#The results suggest that we can be 95 percent confident that the interval [0.15,0.54] contains the 
#true change in life expectancy for a 1 unit change in Diphtheria.

#checking whether our model satisfies statistical assumptions:

#Terms meaning:

#Normality -For fixed values of the independent variables, the dependent variable is normally 
#distributed.
#Independence(Autocorrelation) -The Yi values are independent of each other.
#Linearity -The dependent variable is linearly related to the independent variables.
#Homoscedasticity -The variance of the dependent variable doesn't vary with the levels of the 
#independent variables. We could call this constant variance, but saying homoscedasticity makes me
#feel smarter.

#combine 4 plots
par(mfrow=c(2,2))
plot(model2)

#Normal Q-Q plot (upper right) is a probability plot of the standardized residuals against the values 
#that would be expected under normality.Since we met the normality assumption, the points on this graph 
#fall on the straight 45-degree line.

#Since points in the Scale-Location graph (bottom left) has a random band around a horizontal line,
#homoscedasticity assumption is met.

#We can see that in graph 4, residuals vs. leverage, there are few outliers at 124,134 and 95 in 
#predictors value.

#To properly interpret the coefficients of the OLS model, we must satisfy a number of statistical 
#assumptions:
                                          
#The car package provides a number of functions that significantly enhance your ability to fit and
#evaluate regression models.
                                        
#gvlma package provides a global test for linear model assumptions.
                                          
library(car)
                                          
#NORMALITY
                                      
qqPlot(model2, labels=row.names(train), id.method="identify",simulate=TRUE, main="Q-Q Plot")
            
#With the exception of points 58 and 71(these are outliers), all the points fall close to the line and are within the confidence
#envelope, suggesting that we've met the normality assumption fairly well.                          

#INDEPENDENCE OF ERRORS(AUTOCORRELATION)
durbinWatsonTest(model2)

#The nonsignificant p-value (p=0.404) suggests a lack of autocorrelation, and conversely
#an independence of errors. The lag value (1 in this case) indicates that each observation
#is being compared with the one next to it in the dataset.

#Unless we add the option simulate=FALSE, we'll get a slightly different value each time we run
#the test.

#LINEARITY of predicted values (component plus residual plots or partial residual plots)
crPlots(model2)

#Nonlinearity in any of these plots suggests that we may not have adequately modeled the functional 
#form of that predictor in the regression. If so, we may need to add curvilinear components such as
#polynomial terms, transform one or more variables (for example, use log(X) instead of X), or
#abandon linear regression in favor of some other regression variant.

#The component plus residual plots confirm that you've met the linearity assumption.
#The form of the linear model seems to be appropriate for this dataset.

#Homoscedasticity
ncvTest(model2)
spreadLevelPlot(model2)

#The ncvTest() function produces a score test of the hypothesis of constant
#error variance against the alternative that the error variance changes with the level of
#the fitted values. A significant result suggests heteroscedasticity (nonconstant error variance).

#The spreadLevelPlot() function creates a scatter plot of the absolute standardized
#residuals versus the fitted values.

#In the spread-level plot,the points form a random horizontal band around a horizontal line of best fit. 
#If we'd violated the assumption, we'd expect to see a nonhorizontal line.

#suggested power p is that would stabilize the nonconstant error variance. For example, if the plot showed 
#a nonhorizontal trend and the suggested power transformation was 0.5, then using sq root Y rather than Y in the
#regression equation might lead to a model that satisfies homoscedasticity. If the suggested power was 0, 
#we'd use a log transformation. In the current example, there's no evidence of heteroscedasticity and the
#suggested power is close to 1 (no transformation required).

library(gvlma)
gvmodel <- gvlma(model2)
summary(gvmodel)

#we can see from the printout (the Global Stat line) that the data meet all the statistical
#assumptions that go with the OLS regression model (p = 0.573)

#gvlma() function performs a global validation of linear model assumptions as well as separate evaluations
#of skewness, kurtosis, and heteroscedasticity. In other words, it provides a single omnibus (go/no go) test of
#model assumptions.

#Multicollinearity

vif(model2)

sqrt(vif(model2))>2

#Multicollinearity can be detected using a statistic called the variance inflation factor (VIF). For any
#predictor variable, the square root of the VIF indicates the degree to which the confidence interval for 
#that variable's regression parameter is expanded relative to a model with uncorrelated predictors.
# sq. root vif >2 indicates a multicollinearity problem.

# We see that multicollinearity is there in our data.
#Infant deaths, under 5 deaths, polio, thinness 1 to 19 yrs and thinness 5 to 9 yrs are highly correlated. 

#outlier test
outlierTest(model2)

#Outliers are observations that aren't predicted well by the model. They have either unusually large 
#positive or negative residuals. Positive residuals indicate that the model is underestimating the response
#value, while negative residuals indicate an overestimation.

#One way to identify outliers is points in the Q-Q plot that lie outside the confidence band are considered 
#outliers. A rough rule of thumb is that standardized residuals that are larger than 2 or less than -2 are 
#worth attention.

#The car package also provides a statistical test for outliers. The outlierTest() function reports the
#Bonferroni adjusted p-value for the largest absolute studentized residual:

#this function tests the single largest (positive or negative) residual for significance as an outlier. 
#If it isn't significant, there are no outliers in the dataset. If it is significant, we must delete
#it and rerun the test to see if others are present.

#Comparing models: one including status variable and other without including status variable

#Comparing nested models using the anova() function
anova(model2,model1)

#Because the test is nonsignificant (p = .504), we conclude that status variable don't
#add to the linear prediction and we're justified in dropping them from our model.
                                 
#Comparing models with the AIC-Akaike Information Criterion

AIC(model1,model2)

#the model with the lowest AIC score is preferred. The absolute values of the AIC scores do not matter. 
#These scores can be negative or positive.The AIC values suggest that the model2 is the better model.

#Variable selection-STEPWISE REGRESSION
library(MASS)

#Backward stepwise selection (model1 with status variable)
stepAIC(model1, direction="backward")

#Improving model accuracy

#by taking into account, multicollinearity and variable section method-stepwise regression, we are deleting
#few variabes such as: Infant deaths, under 5 deaths,polio, thinness_1to19_yrs and thinness_5_to9_yrs

data_processed3<-data_processed2[,c(-3,-9,-10,-16,-17)]

#Splitting data into train and test set
train2<-data_processed3[1:150,]
test2<-data_processed3[151:193,]

#check dimensions of train and test set
dim(train2)
dim(test2)

#Fit multiple linear regression model, excluding Country variable
model3<-lm(Life_expectancy~.,data=train2)

#check summary of model
summary(model3)

#apply model on test data
testing2<-predict(model3,test2)

#comapre actual and predicted values of Life_expectancy
head(cbind(test$Life_expectancy,testing2))

#combine Counttry name, actual and predicted values of life expectancy
cbind(data3[151:193,"Country"],test$Life_expectancy,testing2)

#Comparing nested models using the anova() function
anova(model3,model2)

#Since p-value is non signifacnt, therefore we can say our model3 is better model.

#Comparing models with the AIC-Akaike Information Criterion

AIC(model2,model3)

#Also using AIC value, we see that our model3 is better model

#lets check our model by removing GDP and Population variable

str(data_processed3)
data_processed4<-data_processed3[,c(-11,-12)]
str(data_processed4)

#Splitting data into train and test set
train3<-data_processed4[1:150,]
test3<-data_processed4[151:193,]

#check dimensions of train and test set
dim(train3)
dim(test3)

#Fit multiple linear regression model, excluding Country variable
model4<-lm(Life_expectancy~.,data=train3)

#check summary of model
summary(model4)

#apply model on test data
testing3<-predict(model4,test3)

#comapre actual and predicted values of Life_expectancy
head(cbind(test$Life_expectancy,testing3))

#combine Counttry name, actual and predicted values of life expectancy
cbind(data3[151:193,"Country"],test$Life_expectancy,testing2)

#Comparing nested models using the anova() function
anova(model4,model3)

#Comparing models with the AIC-Akaike Information Criterion

AIC(model3,model4)                                          

#We see that GDP and Population is also not a good predictor.

#So, we got the final accuracy 93.03%







                                          