#Task_1

#In an investigation of the Cd contamination of trout in a river, ten trout were caught at each of
#two locations and their Cd content (in mg/g fresh weight) was determined.

#Location A and Location B:
Loc_A <- c(76.8, 72.3, 74.0, 73.2, 46.1, 76.5, 61.9, 62.4, 65.9, 62.4)
Loc_B <- c(64.4, 60.0, 59.4, 61.2, 52.0, 58.1, 62.0, 57.8, 57.2)


#a) Draw parallel box plots. What do you observe?

observations <- boxplot(Loc_A, Loc_B, names = c("Location_A", "Location_B"), main = "Cd contamination of trout in a river",
                       xlab = "Cd content (in mg/g fresh weight)", ylab = "Locations", col = 'steelblue',
                       horizontal = TRUE)

rownames(observations$stats)<-c("Minimum","First Quartile","Median","Third Quartile","Maximum")
colnames(observations$stats) <- c("Location_A","Location_B")
observations$stats

#From the boxplot, 

#we can visualize the five-number summary for Location A and Location B as below: 
#                 Location_A Location_B
#Minimum              46.1       57.2
#First Quartile       62.4       57.8
#Median               69.1       59.4 (Line within the box - Center of distribution)
#Third Quartile       74.0       61.2
#Maximum              76.8       64.4

#IQR(Interquartile Range): The length of the box which ranges between the first quartile and the third quartile
#ranges: (62.4 - 74.0) in Location A and (57.8 - 61.2) in Location B

#Whiskers : The two lines outside the box, 
#Location A: Lines go from 46.1(minimum) to 64.4(lower quartile) and 74.0(Upper quartile) to 76.8(Maximum)
#Location B: Lines go from 57.2(minimum) to 57.8(lower quartile) and 61.2(Upper quartile) to 64.4(Maximum)

#Skewness and symmetry:
#Location A is said to be Negatively skewed as:
#the left whisker is longer than the right whisker,
#the median is closer to the right of the box
#Most of the data is concentrated at the right end of the distribution
#Location B is said to be relatively symmetric as:
#the median is close to the center of the box

#Outliers:
#At Location B,  the data point 52.0 is said to be outlier that lies outside the whiskers 
#since the value of the point is lesser than lower outlier bound(A_u) value
#A_u = (first quartile - 1.5 * IQR) = (Q1 - 1.5 * (Q3 - Q1)) = 52.7 (< 52.0)

#Comparitive Analysis:
#As we could observe that there is no overlapping between the boxes, 
#which suggests significant differences in the distribution groups. 



#b) Test for the level α = 0.05 whether the Cd contents measured at both locations can be
#regarded as realizations of a normally distributed random variable.

#Null Hypothesis: Loc_A and Loc_B are normally distributed
#Alternate Hypothesis: Loc_A and Loc_B are not normally distributed

#Two independent samples Loc_A and Loc_B(Not paired)
#We need to conduct 2 SHAPIRO-WILK tests for Loc_A and Loc_B
#Normally distributed: If the P-value of Shapiro Wilk test is larger than 0.05(alpha)

shapiro.test(Loc_A)

#p-value = 0.09526

shapiro.test(Loc_B)

#p-value = 0.7679

#From the shapiro test, we can clearly see that the p-values at both the Location A and Location B
#are larger than α/2(since we are checking both the samples are normally distributed) = 0.05/2 = 0.025
#so that we can conclude that the cd contents at both the location variables are normally distributed



#c) Test for the level α = 0.05 whether the variances of the Cd contents are equal or significantly
#different from each other.

#Null Hypothesis: Sample Variances are equal
#Alternate Hypothesis: Sample Variances are not equal

var.test(Loc_A, Loc_B)

#p-value = 0.01022

#From the test, we could clearly see that the p-value is smaller than alpha(0.05) value,
#which shows that the the variances of the Cd contents are significantly different from eachother.


#d) Test for the level α = 0.05 whether the expected Cd content at location A is significantly
#greater than that at location B.

#Null Hypothesis: means at Loc_A and Loc_B are equal
#Alternate Hypothesis: mean at Loc_A > mean at Loc_B

#sample situation - Two independent samples Loc_A and Loc_B
#To be tested - Location, particlarly, the mean
#Available tests - Two sample t-test, Welch t-test, Wilcoxon rank sum test
#Need to validate before choosing the test:
#i) Normal distribution and (ii) equal variances for Location A and Location B

#i) From b) we can say that at the Loc_A and Loc_B, Cd contents are normally distribiuted
#ii) From c) we can conclude that the variances of the Cd contents are not equal

t.test(Loc_A, Loc_B, var.equal = FALSE, alternative = "greater")


#p-value = 0.01433


#From the test, the p-value( 0.01433) is smaller than α = 0.05, need to reject the null hypothesis.
#we can conclude that the mean at Location A significantly greater than the mean at Location B.


