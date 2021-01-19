library('knockoff')
library('tidyverse')

load("C:/Users/dicke/OneDrive/Documents/3rd Year/R&D Project/DATA/Analysis/Data/X_2015.rdata")
load("C:/Users/dicke/OneDrive/Documents/3rd Year/R&D Project/DATA/Analysis/Data/y_2015.rdata")

Y <- y_2015
X<- X_no_pubs_2015

n = 1747
p = 140

hist(y[,1], breaks='FD')
hist(log(y[,1]), breaks='FD')

#### FIXED-X tutorial
library(knockoff)

y <- y_2015
X<- X_no_pubs_2015

knockoff_and_bhq <- function (X, y, q) {
  # Log-transform the drug resistance measurements.
  Y <- y_2015
  y = log(Y)
  
  # Run the knockoff filter.
  knock.gen = function(x) create.fixed(x, method='equi')
  result = knockoff.filter(X, y, fdr=fdr, knockoffs=knock.gen, statistic=stat.glmnet_lambdasmax)
  print(result)
  knockoff_selected = names(result$selected)
  
  # Run BHq.
  p = ncol(X)
  lm.fit = lm(y ~ X - 1) # no intercept
  p.values = coef(summary(lm.fit))[,4]
  cutoff = max(c(0, which(sort(p.values) <= fdr * (1:p) / p)))
  bhq_selected = names(which(p.values <= fdr * cutoff / p))
  
  list(Knockoff = knockoff_selected, BHq = bhq_selected)
}

fdr = 0.4
results = lapply(Y, function(y) knockoff_and_bhq(X, y, fdr))

print(results[1])

##### FIXED-X knockoffs

y <- y_2015
X<- X_no_pubs_2015

result = knockoff.filter(X, y, knockoffs = create.fixed, statistic = stat.glmnet_lambdasmax, fdr = 0.5)
print(result)
fdp = function(selected) sum(beta[selected] == 0) / max(1, length(selected))
fdp(result$selected)

#### X has to be normalised
mu <- colMeans(X_no_pubs_2015)
Sigma <- cov(X_no_pubs_2015)


#
# Fit the data.
#
y <- log(z)
mu.hat <- mean(y)
sigma.hat <- sd(y)
#
# Plot a histogram and superimpose the fitted PDF.
#
hist(z, freq=FALSE, breaks=25)
phi <- function(x, mu, sigma) exp(-0.5 * ((x-mu)/sigma)^2) / (sigma * sqrt(2*pi))
curve(phi(log(x), mu.hat, sigma.hat) / x, add=TRUE, col="Red", lwd=2)








