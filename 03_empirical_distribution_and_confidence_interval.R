#Task_3
#In the text file coal data.txt you find the time intervals in days between disasters in British coal
#mines between 1850 and 1965.

#a)Load the data into R and visualize the empirical distribution via a histogram. Also draw a
#kernel density estimte for the underlying distribution. What do you observe regarding the
#(shape of the) distribution?

#Loading the data from coal_data.txt
coal_data = scan("coal_data.txt")

#Empirical distribution of data via histogram
hist(coal_data, 
     main = "Disasters in British coal mines between 1850 and 1965",
     xlab = "Time intervals in days",
     ylab = "Frequency",
     freq = FALSE,
     col = "blue",
     border = "black",
     breaks = 16)

#kernel density estimate
lines(density(coal_data), main = "Kernel Density Estimate", xlab = "Time intervals in days", ylab = "Density", col = "red", lwd = 3)

#Observations:
#The shape of the KDE looks like log normal distribution.
#From the overall data distribution and shape of the KDE, we can identify the distribution as unimodal(one peak).
#The distribution is asymmetric KDE.



#b) Compute a confidence interval for the mean value of days between two disasters for 
#confidence level of 95%.

#No. of items in the file coal_data.txt
n = 190

#Calculate the sample mean 
sample_mean <- mean(coal_data)

#Calculate the standard deviation
sample_sd <- sd(coal_data)

#Set the confidence level
confidence_level <- 0.95

#significance level (1 - confidence_level)
alpha = 0.05

#calculate confidence interval
confidence_interval = c(sample_mean - (sample_sd/sqrt(n)) * qt(1-(alpha/2), n-1),
                        sample_mean + (sample_sd/sqrt(n)) * qt(1-(alpha/2), n-1))

confidence_interval
#95% confidence interval ranges between 168.5473 and 258.2948.

