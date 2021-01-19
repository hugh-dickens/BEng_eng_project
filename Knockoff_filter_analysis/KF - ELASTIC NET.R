library(knockoff)
load("...")
load("...")


X<- X_2015
y <- y_2015


stat.glmnet_lambdasmax_elas <- function(X, X_k, y, family='gaussian', ...) {
  # Randomly swap columns of X and Xk
  swap = rbinom(ncol(X),1,0.5)
  swap.M = matrix(swap,nrow=nrow(X),ncol=length(swap),byrow=TRUE)
  X.swap  = X * (1-swap.M) + X_k * swap.M
  Xk.swap = X * swap.M + X_k * (1-swap.M)
  
  # Compute statistics
  ################# THIS IS WHERE YOU INPUT THE NEW LASSO MAX LAMBDA
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
  
  fit <- glmnet::glmnet(x, y, lambda=lambda,alpha = 0.5, intercept=T, 
                        standardize=F, standardize.response=F, ...)
  
  first_nonzero <- function(x) match(T, abs(x) > 0) # NA if all(x==0)
  indices <- apply(fit$beta, 1, first_nonzero)
  names(indices) <- NULL
  ifelse(is.na(indices), 0, fit$lambda[indices] * n)
}


percent_var_select <- matrix(NA, nrow=21, ncol = 1)
#percent_var_select[,2] <- FDR
var_select <- matrix(NA, nrow=21, ncol = 1)
#var_select[,2] <- FDR
tot_per_elastic_0.5 <-  matrix(NA, nrow=21, ncol = 20)
tot_no_elastic_0.5<- matrix(NA, nrow=21, ncol = 20)
array_factors_elastic_0.5 <- array(0, dim=c(141,21,20))


for (w in 1:20){
  factors_elastic_0.5 <- matrix(NA,nrow = 141)
  for (i in 1:21){
    FDR = (i*0.05-0.05)
    result = knockoff.filter(X, y, statistic=stat.glmnet_lambdasmax_elas,fdr = FDR)
    r <- as.numeric(result$selected)
    n <- 141
    length(r) <- n                      
    factors_elastic_0.5 <- cbind(factors_elastic_0.5,r)
    no.var <- length(result$selected)
    var_select[i,1] <- c(no.var)
    percent_var <- no.var / 141
    percent_var_select[i,1] <- c(percent_var) 
    
  }
  factors_elastic_0.5<-factors_elastic_0.5[,-1]
  array_factors_elastic_0.5[,,w] <- factors_elastic_0.5
  tot_no_elastic_0.5[,w]<- c(var_select)
  tot_per_elastic_0.5[,w]<- c(percent_var_select)
}

tot_no_elastic_ave_0.5 <- as.matrix(rowMeans(tot_no_elastic_0.5))
tot_per_elastic_ave_0.5 <- as.matrix(rowMeans(tot_per_elastic_0.5))

save(array_factors_elastic_0.5, file = "array_factors_elastic_0.5.rdata")
save(tot_no_elastic_0.5, file = "tot_no_elastic_0.5.rdata")
save(tot_per_elastic_0.5, file = "tot_per_elastic_0.5.rdata")
save(tot_no_elastic_ave_0.5, file = "tot_no_elastic_0.5_ave.rdata")
save(tot_per_elastic_ave_0.5, file = "tot_per_elastic_0.5_ave.rdata")
