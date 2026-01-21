#Task_5

#The degradation of a (very high) concentration of pollutants was measured each 30 minutes over
#the course of 10 hours. The corresponding data can be found in the file saet.csv. To model the
#time decay of the concentration, a Rodbard function
#c(t) = a + (b/(1 + (t/c)^d))
#can be fitted to the data. Here a describes the final concentration for t → ∞, a + b describes the
#initial concentration at t = 0, and c and d describe the rate of degradation.


#a) Fit the above model to the data. Plot the data (black circles) and the fitted Rodbard
#function (red line) in a graph

#import the data 
data_con = read.csv("saet.csv")

#plot the data
plot(data_con$t,data_con$conc,lwd=2,xlab="time(t)",ylab="concentration(conc)", main = "Time decay of concentration - Rodbard function")

#Number of measured data
n = dim(data_con)[1]

# set initial values
init = list(a=2, b = 2, c = 1, d = 1)

# nonlinear regression
model_nl = nls( conc ~ a + (b/(1 + (t/c)^d)), data = data_con, start=init, trace = TRUE)

#summary
summary(model_nl)

# plot the predicted values
y_hat = predict(model_nl)
lines(data_con$t, y_hat, lwd=2, col="red")




#b) We are now interested in the time t0.5 at which the pollutant concentration has fallen exactly
#to the mean of the initial and final concentrations. In particular, we want to estimate an
#upper bound for this time. Give a corresponding upper bound ”for t0.5 at 95% confidence.

#Final concentration for t → ∞ is a
#Initial concentration at t = 0 is a+b
#time at t(0.5) is mean of the initial and final concentrations
#t(0.5) = ((a+b)+a) / 2 = a+(b/2) 
#=> a + (b/(1 + (t(0.5)/c)^d)) = a+(b/2)
#=> 1 + ((t(0.5)/c)^d) = 2
#=> t(0.5)/c = 1
# t(0.5) = c

#time t0.5 can be calculated by parameter estimate c in this Rodbard function

t_0.5 = coef(model_nl)["c"]

#Find the confidence interval
conf_interval = confint(model_nl)["c"]

#As we need to find the upper bound for t(0.5), we will use only one parameter(c)
#since t(0.5) = c

#Find asymptotic confidence band:

#LS estimate for theta = c
theta_hat = as.numeric(coef(model_nl)["c"])


# Estimation of error variance
sigma2_hat = deviance(model_nl) / (n - 2)

# Significance level
alpha = 0.05

#Calculate the width of the asymptotic confidence band
Cband = function(x) {
#  NOTE: F distribution and number of parameters = 1
    w = sqrt(sigma2_hat * qt(1 - alpha, 2, n - 2))
  return(w)
}

x = seq(0,10,0.01)
y_hat = predict(model_nl, data.frame("t"=x))

#Calculate the upper bound 
Cwidth = rep(0,length(x))
for(j in 1:length(x)){
  # width of the asymptotic confidence band 
  # and the prediction intervals at all points 
  # evaluate from x
  Cwidth[j] = Cband(x[j]);
}
#Estimate the upper bound
upper_bound = y_hat + Cwidth

