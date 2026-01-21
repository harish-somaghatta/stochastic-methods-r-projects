#Task_4

#The cherry.csv dataset contains measurements of circumference (Girth), height (Height), and
#volume (Volume) of n = 30 of all black cherry trees. Circumference and height are given in feet
#and volume in cubic feet. The circumference was determined at a height of 4.5 feet above the
#ground.

#a) We want to investigate whether the wood volume of black cherry trees behaves like the
#volume of a cylinder.

#load the dataset
cherry_data  <- read.csv("cherry.csv")

#fit the linear model to the data
linear_model <- lm(Volume ~ Height * I(Girth^2), data = cherry_data)

# Summarize the model
summary(linear_model)

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)  
#(Intercept)       15.24612   18.85073   0.809   0.4260  
#Height            -0.19481    0.24883  -0.783   0.4408  
#I(Girth^2)        -1.77436    1.79015  -0.991   0.3307  
#Height:I(Girth^2)  0.05338    0.02300   2.321   0.0284 *


#(i)  Are there any anomalies in the residual analysis?

#plotting the graphs for residuals
par ( mfrow = c(2, 2))
plot (linear_model)

#From the plotted graph, we can conclude that,
#There is no homoscedasticity i.e, non-constant variance
#The residuals are not scattered homogenously
#There are outliers(large residuals)
#The 18. data point is clearly recognizable as influential(according to cook's distance)

#(ii) Does the model fit the data (goodness-of-fit, R**2)?
#goodness-of-fit
#As we see the randomly scattered around zero without any systematic patterns, we
#could consider that goodness-of-fit test is passed
#F-test for model comparison - we will test our model with small model and check
#Assumption: The models are nested, i.e., smaller model are also part of larger model
#null hypothesis (H0): small model is sufficient
#alternative hypothesis(HA) : small model is not sufficient

# Fit the small model (only intercept)
small_model = lm(Volume ~ 1, data = cherry_data)

#F-test
anova(linear_model, small_model)

#Model 1: Volume ~ Height * I(Girth^2)
#Model 2: Volume ~ 1
#Res.Df    RSS Df Sum of Sq     F    Pr(>F)    
#1     26  263.8                                 
#2     29 5758.5 -3   -5494.7 180.5 < 2.2e-16

#Observation: The p-value of the small model(< 2.2e-16) is << 0.001 which indicates 
#that there is strong evidence to reject the null hypothesis.
#The previous full model(linear_model) is significantly better fit than the small_model.


#Multiple R-squared:  0.9542,	Adjusted R-squared:  0.9489 (from summary)
#As the R-squared and Adjusted R-squared values are high and close to the 1.
#The higher R**2 the better the fit of the regression function to the data. 



#(iii)Is θ0 significantly different from zero?
#Coefficients: (from summary)
#  Estimate Std. Error t value Pr(>|t|)  
#(Intercept)       15.24612   18.85073   0.809   0.4260  

#Null Hypothesis (H0): θ0 = 0 (Intercept is zero).
#Alternative Hypothesis (HA): θ0 ≠ 0 (Intercept is not zero).

#since the p-value of the intercept θ0(0.4260) is greater than the significance level(0.05)
#we can not reject the null hypothesis and 
#conclude that the θ0 is not significantly different from zero.


#(iv) Is θ1 significantly different from 1/4π?
# If so, is the wood volume of black cherry trees significantly larger or smaller 
#than that of a cylinder of the same height and circumference?

#Assumptions:
#null hypothesis(H0): θ1 = 1/4π
#Alternative Hypothesis (Ha): θ1 ≠ 1/(4π) (two-tailed test)

# check value for 1/(4π)
compare_value <- 1 / (4 * pi)

# collecting estimate
estimate_θ1 <- coef(summary(linear_model))["Height:I(Girth^2)", "Estimate"]

# collecting standard error
std_error_θ1 <- coef(summary(linear_model))["Height:I(Girth^2)", "Std. Error"]

# Calculate the t-statistic
t_statistic <- (estimate_θ1 - compare_value) / std_error_θ1

# Calculate the p-value for two-tailed
p <- 2 * pt(abs(t_statistic), df = summary(linear_model)$df[3])

#p = 1.68183

#since the p value is greater than the significance level(0.05), we can reject 
#the null hypothesis. so θ1 is significantly different from 1/4π.

#From summary
##Height:I(Girth^2)  0.05338    0.02300   2.321   0.0284 

#As we could see that the θ1 is positive, which mean that the wood volume is significantly 
#larger than that of a cylinder with the same height and circumference.




# b) We want to fit a quadratic function as an alternative to a):
#volume = θ0 + θ1Height + θ2Girth + θ3Height · Girth + θ4Height2 + θ5Girth2

#(i) First fit only a linear function
#volume = θ0 + θ1Height + θ2Girth

#fit the alternative linear model to the data
linear_model_alter <- lm(Volume ~ Height + Girth, data = cherry_data)

# Summarize the alternative linear model
summary(linear_model_alter)

#plotting the graphs for residuals
par ( mfrow = c(2, 2))
plot (linear_model_alter)


#From the plotted graph, we can conclude that,
#we can observe trends in red trend line  
#There are outliers(large residuals)


#goodness-of-fit
# Fit the small model (only intercept)
small_model_alter = lm(Volume ~ 1, data = cherry_data)


#F-test
anova(linear_model_alter, small_model_alter)

#Analysis of Variance Table

#Model 1: Volume ~ Height + Girth
#Model 2: Volume ~ 1
#Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
#1     27  466.3                                  
#2     29 5758.5 -2   -5292.2 153.21 1.832e-15 ***

#Observation: The p-value of the small model(1.832e-15) is << 0.001 which indicates 
#that there is strong evidence to reject the null hypothesis.
#The previous full model(linear_model_alter) is significantly better fit than the small_model_alter


#(ii) Now, step by step, add the three remaining basis functions to the quadratic function.


# Create a data frame
quad_data <- cherry_data

# Add the quadratic term for Height
quad_data$Height_2 <- quad_data$Height^2

# Fit the model with the quadratic term for Height
quad_model_height <- lm(Volume ~ Height + Girth + Height_2, data = quad_data)

# Summary of the quad model
summary(quad_model_height)

# Add the quadratic term for Girth
quad_data$Girth_2 <- quad_data$Girth^2

# Fit the model with the quadratic term for Girth
quad_model_only_girth <- lm(Volume ~ Height + Girth + Girth_2, data = quad_data)
summary(quad_model_only_girth)

# Fit the model with both quadratic terms for Height and Girth
quad_model_girth <- lm(Volume ~ Height + Girth + Height_2 + Girth_2, data = quad_data)

# Summary of the updated model
summary(quad_model_girth)

# Add the quadratic term Height * Girth
quad_data$Height_Girth <- quad_data$Height * quad_data$Girth

# Fit the model with both quadratic terms only for Height and Girth together
quad_model_hg <- lm(Volume ~ Height + Girth + Height_Girth, data = quad_data)
summary(quad_model_hg)

# Fit the final quadratic model 
quad_model <- lm(Volume ~ Height + Girth + Height_2 + Girth_2 + Height_Girth, data = quad_data)

# Summary of the final model
summary(quad_model)


#(iii)Always check the residuals of the larger model again for anomalies, the goodness of
#fit of the model and whether the goodness of fit of the model has indeed improved
#significantly by adding the new function.

#plotting the graphs for residuals
par ( mfrow = c(2, 2))
plot (quad_model)

#residuals vary randomly around 0
#residuals are normally distributed

#anomalies
#The residuals are not scattered homogenously initially but later they are homogenously scattered.

# goodness of fit 
anova(quad_model, linear_model_alter)


#Analysis of Variance Table
#Model 1: Volume ~ Height + Girth + Height_2 + Girth_2 + Height_Girth
#Model 2: Volume ~ Height + Girth
#Res.Df    RSS Df Sum of Sq      F   Pr(>F)   
#1     24 256.95                                
#2     27 466.31 -3   -209.36 6.5183 0.002212 **

anova(quad_model, small_model_alter)

#Analysis of Variance Table
#Model 1: Volume ~ Height + Girth + Height_2 + Girth_2 + Height_Girth
#Model 2: Volume ~ 1
#Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
#1     24  257.0                                  
#2     29 5758.5 -5   -5501.6 102.77 2.121e-15

##Observation: The p-value of the linear_model_alterl(0.002212) is > 0.001 which indicates 
#that there is strong evidence to reject the null hypothesis.
#The quadratic model is significantly better fit than the linear_model_alter

#(iv) If the larger model is not significantly better, then stay with the smaller model and try
#the next of the three estimators (this is a forward model selection).

#From the last observation, we can conclude that the larger model is significantly better than linear_model_alter
#but not better than small_model_alter. we are going to try next three estimators

anova(quad_model_height, small_model_alter)

#Model 1: Volume ~ Height + Girth + Height_2
#Model 2: Volume ~ 1
#Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
#1     26  460.1                                  
#2     29 5758.5 -3   -5298.4 99.798 2.179e-14

anova(quad_model_only_girth, small_model_alter)

#Model 1: Volume ~ Height + Girth + Girth_2
#Model 2: Volume ~ 1
#Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
#1     26  267.5                                  
#2     29 5758.5 -3     -5491 177.89 < 2.2e-16

anova(quad_model_hg, small_model_alter)

#Model 1: Volume ~ Height + Girth + Height_Girth
#Model 2: Volume ~ 1
#Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
#1     26  296.4                                  
#2     29 5758.5 -3   -5462.1 159.69 < 2.2e-16


#checking for R^2:


#quad_model_height
#Volume ~ Height + Girth + Height_2, data = quad_data
#Multiple R-squared:  0.9201,	Adjusted R-squared:  0.9109

#quad_model_only_girth
#Volume ~ Height + Girth + Girth_2, data = quad_data
#Multiple R-squared:  0.9535,	Adjusted R-squared:  0.9482 

#quad_model_hg
#Volume ~ Height + Girth + Height_Girth, data = quad_data
#Multiple R-squared:  0.9485,	Adjusted R-squared:  0.9426

#quad_model
#Volume ~ Height + Girth + Height_2 + Girth_2 + Height_Girth, data = quad_data
#Multiple R-squared:  0.9554,	Adjusted R-squared:  0.9461

#Which submodel of the quadratic function above is the final one you choose?

#From all the above observation, we could choose quadratic model with better p-value and maximum R^2 value
#(Volume ~ Height + Girth + Height*Girth) has better p-value(2.2e-16) and 
#maximum R^2 value (0.9485), which indicates that the mentioned submodel is to be chosen.  



#c) Now choose either the adjusted model from a) or the final model from b). Give reasons for
#your decision.

#linear_model_alter
#Volume ~ Height + Girth, data = cherry_data
#p-value: 1.832e-15
#Multiple R-squared:  0.919,	Adjusted R-squared:  0.913 

#quad_model
#p-value: 2.121e-15
#Volume ~ Height + Girth + Height_2 + Girth_2 + Height_Girth, data = quad_data
#Multiple R-squared:  0.9554,	Adjusted R-squared:  0.9461

#quadratic model is to be chosen to the adjusted linear model, as both the models are statistically
#significant and the p-values are almost close to eachother but quadratic model has 
#higher Adjusted R-squared value 
#quadratic model can be used for non-linear relationships
#quadratic model has more variance


