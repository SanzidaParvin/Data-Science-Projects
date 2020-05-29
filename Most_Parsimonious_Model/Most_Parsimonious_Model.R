# Name: Sanzida Parvin
insurance1 = read.csv(file="C:/Users/...../insurance.csv", header=TRUE, sep=",")
summary(insurance1) # no missing values

head(insurance1)
# check the datatype of the variables
sapply(insurance1,class) 

# check the skewness of variables 'bmi' and 'charges' by histogram, if log transformation needed
hist(insurance1$bmi)
hist(insurance1$charges, main = "Histogram of Charges", xlab = "charges") # havily right skewed

insurance = data.frame(insurance1)
# variable charges is right skewed, it needs the log transformation
insurance$charges = log(insurance$charges) 

install.packages("psych")
library(psych)
?pairs

# Check for coliniarity. 
pairs(insurance) # no coliniarity exists between variables.
attach(insurance)

# fit linear model
fit_insurance = lm(charges ~., data = insurance) 
summary(fit_insurance)

par(mfrow = c(2, 2))
# residuals plot
plot(fit_insurance) 
par(mfrow = c(1, 1))

library(leaps)
# best subset selection using regsubsets function
regfit_insurance = regsubsets(charges ~ age + sex + bmi + children + smoker + region, data = insurance, nvmax = 6)

plot(regfit_insurance)
plot(regfit_insurance, scale = "adjr2")

reg_summary = summary(regfit_insurance)
# BIC values of the regfit
reg_summary$bic 

# plot BIC and adjr2 values to find out the best subsets
plot(reg_summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l", lwd = 2)
plot(reg_summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted R Square", type = "l", lwd = 2)

which.min(reg_summary$bic) # min of BIC
which.min(reg_summary$cp) # min of CP
which.max(reg_summary$adjr2) # max of adjr2
# model with 6 variables, means full model is the best model

# find the coefficient 
coef(regfit_insurance, 6)

# Define a predict() function for regsubsets objects
predict.regsubsets <- function(object, newdata, id, ...){
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id=id)
  xvars = names(coefi)
  mat[ , xvars] %*% coefi
}   # end fuction predict.regsubsets

# 10-fold cross-validation
n = dim(insurance)[1]
k = 10
groups = c(rep(1:k,floor(n/k)),1:(n-floor(n/k)*k))

set.seed(14)
cvgroups = sample(groups,n)
group.error = matrix(,nr=6, nc = k) # row = number of variables, column = which fold

for (i in 1:k) {
  groupi = (cvgroups == i)
  cv.fit = regsubsets(charges ~ age + sex + bmi + children + smoker + region, data = insurance[!groupi,], nvmax = 6)
  
  for (j in 1:6)  {
    y.pred = predict(cv.fit, newdata = insurance[groupi,], id = j)
    group.error[j,i] = mean((insurance$charges[groupi] - y.pred)^2)
  } # end iter over model size
} # end iter over folds

# calculate the MSE of the models
MSE = apply(group.error, 1, mean) 
plot(MSE)
which.min(MSE) 
# with 6 predictors give the minimum MSE

# calculate the standard error
std_error = apply(group.error, 1, sd)/sqrt(k) 
std_error[6]
which(MSE <= MSE[6]+std_error[6])

# With 3 variables gives the most parsimonious(simplest) model with a CV error within 1 standard error of the lowest
coef(regfit_insurance, 3)

# simplified model with 3 variables. 
# simplest model
# y_hat = 7.28772342 + 0.03528491*age + 0.10163109*children + 1.54427238*smokeryes



######################################################################################


# 2nd technique, Artificial neural network
library(nnet)
head(insurance1)

# fit the model
set.seed(14)
fit_nnet = nnet(charges ~ age + sex + bmi + children + smoker + region, data = insurance1, size = 1, maxit = 200)
summary(fit_nnet)

library(NeuralNetTools)
plotnet(fit_nnet) # neural network plot

# Cross Validation to tune the number of hidden nodes
n2 = dim(insurance1)[1];n2
k2 = 10 #using 10-fold cross-validation
sizes = 1:8 # number of hidden nodes
groups2 = c(rep(1:k2,floor(n2/k2)),1:(n2-floor(n2/k2)*k2)) # n is not perfectly divisable by k

set.seed(100)
cvgroups2 = sample(groups2,n2) 
squaredError = matrix( , nr = n2, nc = length(sizes) ) 

for(i in 1:k2){
  groupi = (cvgroups2 == i)
  for(j in 1:length(sizes)){
    fit = nnet(charges ~ age + sex + bmi + children + smoker + region, data = insurance1[!groupi,], 
               size = sizes[j], maxit = 1000, linout = T, trace = F)
    
    squaredError[groupi, j] = (insurance1$charges[groupi] - predict(fit, insurance1[groupi,]) )^2
  } # end iteration over j
} # end iteration over i

MSE_nnet = apply(squaredError, 2, mean); MSE_nnet
plot(sizes, MSE_nnet, type="l", lwd=2, las=1)
# hidden node is 3/4 to get the min error(most persimonious model)

min(MSE_nnet)
which(MSE_nnet == min(MSE_nnet)) # find out the best hidden nodes

head(insurance1)
# standerdized the numeric variables to reduce the influences of any variable
insurance1$charges = scale(insurance1$charges)
insurance1$bmi = scale(insurance1$bmi)
insurance1$age = scale(insurance1$age)

set.seed(100)
# fit the model
fit_nnet2 = nnet(charges ~ age + sex + bmi + children + smoker + region, data = insurance1, 
                 size = 4, maxit = 1000, linout = T)
summary(fit_nnet2)

fit_nnet2$convergence # check the convergence
plotnet(fit_nnet2)
# best model with minimum error is with 4 hidden nodes.

# most important variables of the model
garson(fit_nnet2) 












