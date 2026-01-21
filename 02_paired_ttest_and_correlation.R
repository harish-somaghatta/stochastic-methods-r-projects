#Task_2

#The measured temperatures (in C) before and 3 hours after taking a drug can be found for ten
#different patients

#Given sample data
before <- c(38.4, 39.6, 39.4, 40.1, 39.2, 38.5, 39.3, 39.1, 38.4, 39.5)
after <- c(37.6, 37.9, 39.1, 39.4, 38.6, 38.9, 38.7, 38.7, 38.9, 38.7)

#a) Describe which sample situation (one, two, paired, multiple, ...) we have here

#As we could see that the each patient is measured with two temperatures before and 
#after taking the drug. Here, the variable(Temperature) is same and measured twice.
#From the above, we can conclude that the given sample is paired.

#b) Generate a scatter plot of the data. What do you observe?
#Creating a scatter plot
plot(before, after, main = "Patient's temperatures before and after taking drug",
     xlab = "Temp before taking drug in C", ylab = "Temp after taking drug in C",
     pch = 20, col = "blue")

#Observations:
#Most of the points move from lower-left to upper-right, but due to outliers at starting,
#it can be considered as weaker positive correlation
#Points are closely clustered around a linear pattern
#Outliers - Three datapoints are deviated from the overall pattern of the scatter plot
#Relationship - Except outliers, the datapoints follow straight line with linear relationship



#c) Estimate the correlation between the temperature values before and after taking the drug.

#Calcuting the correlation coefficient
correlation <- cor(before, after)

#Correlation Coefficient: 0.3485987 
#As the Correlation Coefficient(0.3485987) is greater than 0, we consider the correlation
#as positive correlation. However the correlation value is closer to 0 indicate a 
#weaker positive correlation.


#d) Test for level α = 0.05 if the correlation is significant (you can decide in one- or two-sided
#alternative). To this end, research (in literature or online) which test is suitable for this
#task and verify that its assumptions are satisfied for the data at hand.

#The most common test to check whether correlation is significant or not is Pearson correlation test.

#Null Hypothesis: H0: The correlation coefficient (ρ) is equal to zero.
#Alternative Hypothesis (HA): 
#The correlation coefficient (ρ) is not equal to zero(two-sided alternative)
#HA (Positive Correlation): ρ > 0 or HA (Negative Correlation): ρ < 0 (one-sided alternative)

#Pearson correlation test
cor.test(before, after, alternative = "greater")

#As the correlation coefficient is positive, one-sided alternative is considered and used "greater" 
# alternative in the correlation test.

#p-value = 0.1618

#As the p-value is greater than the alpha(0.05) value, we can not reject the null hypothesis
#the correlation is not significant i.e The correlation coefficient (ρ) is equal to zero.


#Before using paired t-test, the test assumes that the difference between the observations follows a
#normal distribution. The Shapiro-Wilk test is used to check the normality.

difference <- before - after
#checking whether the difference is normally distributed

#Null hypothesis: The difference follows normal distribution
#Alternative hypothesis:The difference does not follows normal distribution

shapiro.test(difference)

#p-value = 0.3265 (from the test)
#since the p-value is greater than the given significance level(alpha = 0.05),
#we don't reject the null hypothesis and the difference follows normal distribution.

#As the assumption is fulfilled, paired t-test is carried

#Null hypothesis: No significant difference between the temperatures of different patients
#Alternative hypothesis: Significant difference between the temperatures of different patients

t.test(before, after, paired = TRUE, alternative = "greater")

#p-value = 0.01635 (from the test)
#As the p-value is smaller than the alpha(0.05) value, we can reject the null hypothesis
#and can conclude that there is significant difference between the temperatures of different patients.


