# Multiple-Linear-Regression-Analysis

Data description:

The Global Health Observatory (GHO) data repository under World Health Organization (WHO) keeps track of the health status as well as many
other related factors for all countries. The data-sets are made available to public for the purpose of health data analysis. The data-set
related to life expectancy, health factors for 193 countries has been collected from the same WHO data repository website and its 
corresponding economic data was collected from United Nation website.

It has been observed that in the past 15 years , there has been a huge development in health sector resulting in improvement of human mortality rates especially in the developing nations in comparison to the past 30 years.

The final merged file(final dataset) consists of 22 Columns and 2938 rows which meant 20 predicting variables. 

Source: https://www.kaggle.com/kumarajarshi/life-expectancy-who

Language used: R

Technical details:

I have applied multiple linear regression to predict life expectancy. Firstly, I fitted model using all the variables and checked whether
our model satisfies following statistical assumptions:

1. Normality of dependent variable
2. Independence of errors or Autocorrelation
3. Linearity of predicted values.
4. Homoscedasticity
5. Multicollinearity

I also used variable selection method i.e. backward stepwise selection and checked which variables are good predictors.

Out first model has multicollinearity and also using the results of variable selectin method, I improved the model by removing variables
such as Infant deaths,under 5 deaths,polio,thinness_1to19_yrs,GDP, Population and thinness_5_to9_yrs.

I compared models using Anova test and AIC(Akaike Information Criterion).

I got the final accuracy of 93.03% on my model.
