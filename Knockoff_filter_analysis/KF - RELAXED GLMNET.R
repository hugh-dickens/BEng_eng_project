library(knockoff)
load("...")
load("...")


X<- X_2015
y <- y_2015

#par(mfrow=c(1,3))
#plot(fit) #### gamma = 1 is the traditional glmnet fit
#plot(fit,gamma=0.5) ### mixture of the two
#plot(fit,gamma=0) ## gamma=0 is the unpenalized fit


#print(cfit)

stat.glmnet_lambdasmax_rel <- function(X, X_k, y, family='gaussian', ...) {
  # Randomly swap columns of X and Xk
  swap = rbinom(ncol(X),1,0.5)
  swap.M = matrix(swap,nrow=nrow(X),ncol=length(swap),byrow=TRUE)
  X.swap  = X * (1-swap.M) + X_k * swap.M
  Xk.swap = X * swap.M + X_k * (1-swap.M)
  
  # Compute statistics
  ### THIS IS WHERE YOU INPUT THE NEW LASSO MAX LAMBDA
  ### IE RIDGE AND ELASTIC NET FUNCTION.
  Z = lasso_max_lambda_glmnet(cbind(X.swap, Xk.swap), y)
  p = ncol(X)
  orig = 1:p
  W = pmax(Z[orig], Z[orig+p]) * sign(Z[orig] - Z[orig+p])
  
  # Correct for swapping of columns of X and Xk
  W = W * (1-2*swap)
}


lasso_max_lambda_glmnet <- function(x, y, nlambda=500, intercept=T, standardize=T, ...) {
  if (!requireNamespace('glmnet', quietly=T))
    stop('glmnet is not installed', call.=F)
  
  # Standardize the variables
  if( standardize ){
    x = scale(x)
  }
  
  n = nrow(x); p = ncol(x)
  if (!methods::hasArg(family) ) family = "gaussian"
  else family = list(...)$family
  
  if (!methods::hasArg(lambda) ) {
    if( identical(family, "gaussian") ) {
      if(!is.numeric(y)) {
        stop('Input y must be numeric.')
      }
      # Unless a lambda sequence is provided by the user, generate it
      lambda_max = max(abs(t(x) %*% y)) / n
      lambda_min = lambda_max / 2e3
      k = (0:(nlambda-1)) / nlambda
      lambda = lambda_max * (lambda_min/lambda_max)^k
    }
    else {
      lambda = NULL
    }
  }
  
  fit <- glmnet::glmnet(x, y, lambda=lambda, intercept=T, 
                        standardize=F, standardize.response=F, relax = T, alpha=1, gamma = 0.5)#,relax = T,...)#exclude = integer(0)
  #fit <- relax.glmnet(fit, x = X, y = y, check.args = F)
  
  first_nonzero <- function(x) match(T, abs(x) > 0) # NA if all(x==0)
  indices <- apply(fit$beta, 1, first_nonzero)
  names(indices) <- NULL
  ifelse(is.na(indices), 0, fit$lambda[indices] * n)
}

result = knockoff.filter(X, y, statistic = stat.glmnet_lambdasmax_rel, fdr = 0.4)
#result

percent_var_select4 <- matrix(NA, nrow=21, ncol = 1)
#percent_var_select[,2] <- FDR
var_select4 <- matrix(NA, nrow=21, ncol = 1)
#var_select[,2] <- FDR
tot_per_relaxed <-  matrix(NA, nrow=21, ncol = 20)
tot_no_relaxed<- matrix(NA, nrow=21, ncol = 20)
array_factors_relaxed <- array(0, dim=c(141,21,20))

for (w in 1:20){
  factors_relaxed<- matrix(NA,nrow = 141)
  for (i in 1:21){
    result = knockoff.filter(X, y, statistic=stat.glmnet_lambdasmax_rel,fdr = (i*0.05-0.05))
    r <- as.numeric(result$selected)
    n <- 141
    length(r) <- n                      
    factors_relaxed <- cbind(factors_relaxed,r)
    no.var4 <- length(result$selected)
    var_select4[i,1] <- c(no.var4)
    percent_var4 <- no.var4 / 141
    percent_var_select4[i,1] <- c(percent_var4) 
    
  }
  factors_relaxed<-factors_relaxed[,-1]
  array_factors_relaxed[,,w] <- factors_relaxed
  tot_no_relaxed[,w]<- c(var_select4)
  tot_per_relaxed[,w]<- c(percent_var_select4)
}


tot_no_relaxed_ave <- as.matrix(rowMeans(tot_no_relaxed))
tot_per_relaxed_ave <- as.matrix(rowMeans(tot_per_relaxed))

save(array_factors_relaxed, file = "array_factors_relaxed.rdata")
save(tot_no_relaxed, file = "tot_no_relaxed.rdata")
save(tot_per_relaxed, file = "tot_per_relaxed.rdata")
save(tot_no_relaxed_ave, file = "tot_no_relaxed_ave.rdata")
save(tot_per_relaxed_ave, file = "tot_per_relaxed_ave.rdata")

