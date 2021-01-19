library(knockoff)
library('tidyverse')
library("matrixStats")


load("...")
load("...")


n = 1747
p = 141

#X<-charging_stations.df[-1]
#X_2015 <- charging_stations.df_2015[-1]
no_pubs <- X[-6]
no_pubs_2015 <- X_2015[-6]
no_banks <- X[-1]
no_banks_2015 <- X_2015[-1]
neither <- no_pubs[-1]
neither_2015 <- no_pubs_2015[-1]

#y<- as.matrix(charging_stations.df[,1])
#y_2015 <- as.matrix(charging_stations.df_2015[,1]) 
  
#X= matrix(as.numeric(unlist(X)),nrow=nrow(X))
X_no_pubs <- matrix(as.numeric(unlist(no_pubs)),nrow=nrow(no_pubs))
X_no_banks <- matrix(as.numeric(unlist(no_banks)),nrow=nrow(no_banks))
X_neither <- matrix(as.numeric(unlist(neither)),nrow=nrow(neither))

#X_2015= matrix(as.numeric(unlist(X_2015)),nrow=nrow(X_2015))
X_no_pubs_2015 <- matrix(as.numeric(unlist(no_pubs_2015)),nrow=nrow(no_pubs_2015))
X_no_banks_2015 <- matrix(as.numeric(unlist(no_banks_2015)),nrow=nrow(no_banks_2015))
X_neither_2015 <- matrix(as.numeric(unlist(neither_2015)),nrow=nrow(neither_2015))


### Might need to do some sort of transforming of Y variable as the 
### method relies on a gaussian distribution. 
#hist(log(Y), breaks='FD')
#hist(Y, breaks='FD')

### standard knockoffs, By default, the knockoff filter creates model-X second-order 
###Gaussian knockoffs. This construction estimates from the data the 
##mean \(\mu\) and the covariance \(\Sigma\) of the rows of \(X\), 
##instead of using the true parameters (\(\mu, \Sigma\)) from which
##the variables were sampled
result = knockoff.filter(X, y, fdr = 0.4)
print(result)

fdp = function(selected) sum(beta[selected] == 0) / max(1, length(selected))
fdp(result$selected)

## 2015 results
result_2015 =  knockoff.filter(X_2015, y_2015, fdr = 0.3)
print(result_2015)

fdp = function(selected) sum(beta[selected] == 0) / max(1, length(selected))
fdp(result_2015$selected)

#result = knockoff.filter(X_no_banks, y, fdr = 0.4)
#print(result)

#result = knockoff.filter(X_no_pubs, y, fdr = 0.6)
#print(result)

#result = knockoff.filter(X_neither, y, fdr = 0.5)
#print(result)



#### generate knockoffs using the gaussian distribution model parameters
#### this may not relevant to the energy dataset.

# Generate the variables from a multivariate normal distribution
#mu = rep(0,p)
mu =colMeans(X)
#mu_2015 = colMeans(X_2015)
mu_no_pubs = colMeans(X_no_pubs)
mu_no_pubs_2015 = colMeans(X_no_pubs_2015)
#mu_neither = colMeans(X_neither)
#rho = 0.25
#Sigma = toeplitz(rho^(0:(p-1))) ## covariance matrix
Sigma = cov(X) ## covariance matrix
#Sigma_2015 = cov(X_2015)
Sigma_no_pubs = cov(X_no_pubs)
Sigma_no_pubs_2015 = cov(X_no_pubs_2015)
#Sigma_neither = cov(X_neither)

gaussian_knockoffs = function(X_no_pubs_2015) create.gaussian(X_no_pubs_2015, mu_no_pubs_2015, Sigma_no_pubs_2015)
result = knockoff.filter(X_no_pubs_2015, y_2015, knockoffs=gaussian_knockoffs, fdr=0.8)

print(result)

#gaussian_knockoffs = function(X_neither) create.gaussian(X_neither, mu_neither, Sigma_neither)
#result = knockoff.filter(X_neither, y, knockoffs=gaussian_knockoffs, fdr=0.5)

#print(result)

#fdp = function(selected) 
  
fdp = sum(beta[result$selected] == 0) / max(1, length(result$selected))
fdp

### Usign random forests
result = knockoff.filter(X_no_pubs_2015, y_2015, knockoffs = gaussian_knockoffs, statistic = stat.random_forest, fdr=0.9)
print(result)

fdp = sum(beta[result$selected] == 0) / max(1, length(result$selected))


### Using a user defined t-test statistic
gaussian_knockoffs = function(X_neither) create.gaussian(X_neither, mu_neither, Sigma_neither)

my_knockoff_stat = function(X, X_k, y) {
  abs(t(X) %*% y) - abs(t(X_k) %*% y)
}
result = knockoff.filter(X_neither, y, knockoffs = gaussian_knockoffs, statistic = my_knockoff_stat, fdr=0.5)
print(result)

fdp = sum(beta[result$selected] == 0) / max(1, length(result$selected))
fdp

##User-defined knockoff generation functions
create_knockoffs = function(X) {
  create.second_order(X, shrink=T) #https://www.rdocumentation.org/packages/knockoff/versions/0.3.2/topics/create.second_order
}
result = knockoff.filter(X_neither, y, knockoffs=create_knockoffs, fdr=0.3)
print(result)

###Equi-correlated knockoffs
##Equicorrelated knockoffs offer a computationally cheaper alternative to SDP knockoffs,
##at the cost of lower statistical power. In this example we generate second-order Gaussian
##knockoffs using the estimated model parameters and the equicorrelated construction. 
##Then we run the knockoff filter.

y<- log(y_2015)

gaussian_knockoffs = function(X) create.second_order(X, method='equi', shrink=T)
result = knockoff.filter(X, y, knockoffs = gaussian_knockoffs, fdr = 0.3)
print(result)

result = knockoff.filter(X, y, knockoffs = create.second_order, fdr = 0.4)
print(result)


###LOOKING INSIDE THE KNOCKOFF FILTER

#The first step is to generate the knockoff variables for the true Gaussian 
#distribution of the variables.

X<- X_no_pubs_2015
mu <- colMeans(X_no_pubs_2015)
Sigma <- cov(X_no_pubs_2015)
knockoffs = function(X) create.gaussian(X, mu, Sigma)

#Then, we compute the knockoff statistics using 10-fold cross-validated lasso
#W = stat.glmnet_coefdiff(X_no_pubs, X_k, y, nfolds=10)#,nlambda=200)

result = knockoff.filter(X, y, knockoffs=knockoffs,
                         statistic=stat.forward_selection)

#Now we can compute the rejection threshold
thres = knockoff.threshold(W, fdr=0.4, offset=1)

#The final step is to select the variables
selected = which(W >= thres)
print(selected)

fdp = function(selected) sum(beta[selected] == 0) / max(1, length(selected))
fdp(selected)


# We show how to manually run the knockoff filter multiple times and 
# compute average quantities. This is particularly useful to estimate the FDR 
# (or the power) for a particular configuration of the knockoff filter on artificial problems.

# Optimize the parameters needed for generating Gaussian knockoffs, 
# by solving an SDP to minimize correlations with the original variables.
# This calculation requires only the model parameters mu and Sigma, 
# not the observed variables X. Therefore, there is no reason to perform it
# more than once for our simulation.

diag_s = create.solve_asdp(Sigma_no_pubs)
fdp = function(selected) sum(beta[selected] == 0) / max(1, length(selected))

# Compute the fdp over 20 iterations
nIterations = 20
fdp_list = sapply(1:nIterations, function(it) {
  # Run the knockoff filter manually, using the pre-computed value of diag_s
  X_k = create.gaussian(X_no_pubs, mu_no_pubs, Sigma_no_pubs, diag_s=diag_s)
  W = stat.glmnet_lambdasmax(X_no_pubs, X_k, y)
  t = knockoff.threshold(W, fdr=0.2, offset=1)
  selected = which(W >= t)
  # Compute and store the fdp
  fdp(selected)
})
# Estimate the FDR
mean(fdp_list) 

### SAME TESTS FOR 2015
diag_s = create.solve_asdp(Sigma_no_pubs_2015)
fdp = function(selected) sum(beta[selected] == 0) / max(1, length(selected))

# Compute the fdp over 20 iterations
#### Look at the plot, could be useful for seeing how many times the knockoff filter
### leaves any variables at all.
nIterations = 20
fdp_list = sapply(1:nIterations, function(it) {
  # Run the knockoff filter manually, using the pre-computed value of diag_s
  X_k = create.gaussian(X_no_pubs_2015, mu_no_pubs_2015, Sigma_no_pubs_2015, diag_s=diag_s)
  W = stat.glmnet_lambdasmax(X_no_pubs_2015, X_k, y_2015)
  t = knockoff.threshold(W, fdr=0.2, offset=1)
  selected = which(W >= t)
  # Compute and store the fdp
  fdp(selected)
})
# Estimate the FDR
mean(fdp_list)
plot(fdp_list)


#### Split original matrix into subset groups of 5
## Shuffle row indices: rows
rows<- sample(nrow(charging_stations.df))

## Randomly order data
shuffled_stations <- charging_stations.df[rows,]

## Determine rows to split on
split<- round(nrow(charging_stations.df)*.2)

## Create subsets
subset1<- charging_stations.df[1:split,]
X1 <- subset1[-1]
X1<-matrix(as.numeric(unlist(X1)),nrow=nrow(X1))
y1<- as.matrix(subset1[1])
subset2<- charging_stations.df[(split+1):(2*split),]
X2 <- subset2[-1]
X2<-matrix(as.numeric(unlist(X2)),nrow=nrow(X2))
y2<- as.matrix(subset2[1])
subset3<- charging_stations.df[(2*split+1):(3*split),]
X3 <- subset3[-1]
X3<-matrix(as.numeric(unlist(X3)),nrow=nrow(X3))
y3<- as.matrix(subset3[1])
subset4<- charging_stations.df[(3*split+1):(4*split),]
X4 <- subset4[-1]
X4<-matrix(as.numeric(unlist(X4)),nrow=nrow(X4))
y4 <- as.matrix(subset4[1])
subset5<- charging_stations.df[(4*split+1):nrow(charging_stations.df),]
X5 <- subset5[-1]
X5<-matrix(as.numeric(unlist(X5)),nrow=nrow(X5))
y5 <- as.matrix(subset5[1])


result=knockoff.filter(X1, y1, fdr = 0.4)
print(result)
result=knockoff.filter(X2, y2, fdr = 0.4)
print(result)
result=knockoff.filter(X3, y3, fdr = 0.4)
print(result)
result=knockoff.filter(X4, y4, fdr = 0.4)
print(result)
result=knockoff.filter(X5, y5, fdr = 0.4)
print(result)



