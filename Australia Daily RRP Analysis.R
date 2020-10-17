#Clear all existing global environments for a clean execution of code
rm(list=ls())

#Loading required libraries
library("rjags")
library("coda")

########Loading the data

#Path to the pre-covid and post-covid files
pre_covid_data = paste0(getwd(),"\\Australia_Data_PreCov.csv")
post_covid_data = paste0(getwd(),"\\Australia_Data_PostCov.csv")

#Loading the files into code environment
dat1<-read.csv(pre_covid_data,header=TRUE)
dat2<-read.csv(post_covid_data,header=TRUE)



#Removing highly correlated columns like Minimum Temperature which is highly correlate to Maximum Temperature
dat1 = dat1[,c(1,2,3,5,6)]
dat2 = dat2[,c(1,2,3,5,6)]

#Removing rows with missing values
dat1 = na.omit(dat1)
dat2 = na.omit(dat2)

#Performing t-test to check significance of means of RRP between two data tables
t.test(dat1[,2], dat2[,2], alternative = "two.sided", var.equal = FALSE)



########Modeling the data

#Defining model as a string to be compiled by RJAGS package
mod_string = " model {
    for (i in 1:length(RRP)) {
		RRP[i] ~ dnorm( z[i], prec)
		z[i] = b0 + b[1]*Total_Demand[i] + b[2]*Rainfall[i] + b[3]*Maximum_Temp[i] + b[4]*Solar_Exposure[i]
	 }
    
    b0 ~ dnorm(0, 1/1e6)
	for (j in 1:4) {
		b[j] ~ dnorm(0, 1/1e6)
	}
	prec ~ dgamma(4,0.5)
	sig = (1.0/prec)
} "


########PRE-COVID DATA


#Defining data to be fed to the model, which here is pre-covid data
data_jags = as.list(dat1)

#Defining the parameters to track when simulating MCMC for the data
params = c("b0", "b", "sig")

#Compiling model 
mod1 = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)

#Running a burn-in of 10^4 iterations
update(mod1, 1e4)

#Simulating from 3 MCMC for 10^5 iterations
mod_sim1 = coda.samples(model=mod1,
                        variable.names=params,
                        n.iter=1e5)


#Checking convergence statistics
gelman.diag(mod_sim1)
autocorr.diag(mod_sim1)
effectiveSize(mod_sim1)
raftery.diag(mod_sim1)

#Plotting traceplot of the 3 MCMC chains
traceplot(mod_sim1)


#Binding all 3 chains together
mod_csim1 = as.mcmc(do.call(rbind, mod_sim1))

#Getting the point estimates of parameters through mean of simulation data
params_coef1 = colMeans(mod_csim1)
params_coef1




########POST-COVID DATA


#Defining the data for the model, which here is post-covid data
data_jags = as.list(dat2)

#Defining the parameters to track in MCMC simulations
params = c("b0", "b", "sig")

#Compiling the model
mod2 = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)

#Running burn-in period for 10^4 iterations
update(mod2, 1e4)

#Simulating 3 MCMC for 10^5 iterations
mod_sim2 = coda.samples(model=mod2,
                        variable.names=params,
                        n.iter=1e5)


#Checking convergence statistics
gelman.diag(mod_sim2)
autocorr.diag(mod_sim2)
effectiveSize(mod_sim2)
raftery.diag(mod_sim2)

#Plotting the traceplot of the 3 chains
traceplot(mod_sim2)

#Binding 3 chains into 1
mod_csim2 = as.mcmc(do.call(rbind, mod_sim2))

#Getting point estimates of parameters through mean of simulated data
params_coef2 = colMeans(mod_csim2)
params_coef2



#Plotting errors for predictions through model 1 on pre-covid data
y_pred1 = params_coef1[c(1,2,3,4)]%*%t(dat1[,c(1,3,4,5)])+params_coef1[c(5)]
resid1 = dat1[,c(2)] - y_pred1[1,]
plot(resid1~y_pred1[1,])

#Plotting errors for predictions through model 2 on post-covid data
y_pred2 = params_coef2[c(1,2,3,4)]%*%t(dat2[,c(1,3,4,5)])+params_coef2[c(5)]
resid2 = dat2[,c(2)] - y_pred2[1,]
plot(resid2~y_pred2[1,])

#Calculating RMSE for both models
rmse1 = sqrt(mean((dat1[,c(2)] - y_pred1[1,])^2))
rmse2 = sqrt(mean((dat2[,c(2)] - y_pred2[1,])^2))

rmse1
rmse2

########POSTERIOR DISTRIBUTION COMPARISONS FOR PARAMETERS



#Printing point estimates for parameters in both chains
params_coef1
params_coef2


#b1 parameter is coefficient associated with Total_Demand predictor

#Checking probability that b1 parameter in model 1 is less than 0
mean(mod_csim1[,1]<0)
#Checking probability that b1 parameter in model 2 is greater than 0
mean(mod_csim2[,1]>0)
#Checking probability that b1 parameter in model 2 is greater than b1 parameter in model 1
mean(mod_csim2[,1]>mod_csim1[,1])



#b3 parameter is the coefficient associated Maximum_Temp predictor

#Checking probability that b3 parameter in model 1 is greater than 0
mean(mod_csim1[,3]>0)
#Checking probability that b3 parameter in model 2 is less than 0
mean(mod_csim2[,3]<0)
#Checking probability that b3 parameter in model 1 is greater than b3 parameter in model 2
mean(mod_csim1[,3]>mod_csim2[,3])



########PLOTTING PARAMETER DISTRIBUTIONS



#Plotting distributions for b1 parameter in model 1 and model 2
hgA <- hist(mod_csim1[,1],freq = FALSE, plot = FALSE) # Save first histogram data
hgB <- hist(mod_csim2[,1],freq = FALSE, plot = FALSE) # Save 2nd histogram data


plot(hgA, main="b1 Parameter Distribution",xlab = "b1", col = 2, xlim = c(-0.0001, 0.0001),ylim = c(0,70000),freq = FALSE) # Plot 1st histogram using a transparent color
plot(hgB, col = 3, freq = FALSE,add = TRUE) # Add 2nd histogram using different color
legend("topright", 
       legend = c("Post-Covid", "Pre-Covid"), 
       fill = c("green", "red"), pt.cex = 1, cex=0.45)


#Plotting distributions for b3 parameter in model 1 and model 2
hgA <- hist(mod_csim1[,3],freq = FALSE, plot = FALSE) # Save first histogram data
hgB <- hist(mod_csim2[,3],freq = FALSE, plot = FALSE) # Save 2nd histogram data


plot(hgA, xlab = "b3", main = "b3 Parameter Distribution", col = 2, xlim = c(-2, 1),ylim = c(0,2.5),freq = FALSE) # Plot 1st histogram using a transparent color
plot(hgB, col = 3, freq = FALSE,add = TRUE) # Add 2nd histogram using different color
legend("topright", 
       legend = c("Post-Covid", "Pre-Covid"), 
       fill = c("green", "red"), pt.cex = 1, cex=0.45)
