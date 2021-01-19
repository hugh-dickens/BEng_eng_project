library(ggplot2)

load("C:/Users/dicke/OneDrive/Documents/3rd Year/R&D Project/DATA/Analysis/Data/X_2015.rdata")
load("C:/Users/dicke/OneDrive/Documents/3rd Year/R&D Project/DATA/Analysis/Data/y_2015.rdata")

FDR <- seq(0, 1, 0.05)
percent_var_select <- matrix(NA, nrow=21, ncol = 2)
percent_var_select[,2] <- FDR
var_select <- matrix(NA, nrow=21, ncol = 2)
var_select[,2] <- FDR


for (i in 1:21){

result_2015 <-  knockoff.filter(X_2015, y_2015, fdr = (i*0.05-0.05))
no.var <- length(result_2015$selected)
var_select[i,1] <- c(no.var)
percent_var <- no.var / 141
percent_var_select[i,1] <- c(percent_var) 

}

percent_var_select.df <- as.data.frame(percent_var_select)
percent_var_select.df<- setNames(percent_var_select.df, c("% Variables Selected", "FDR"))
var_select.df <- as.data.frame(var_select)
var_select.df<- setNames(var_select.df, c("No. Variables Selected", "FDR"))

ggplot(percent_var_select.df, aes(y = percent_var_select.df$`% Variables Selected`, x= percent_var_select.df$FDR)) +
  geom_point() +
  geom_smooth(span = 0.5, se = FALSE) +
  ylab("% Variables Selected")+
  xlab("FDR")+
  ylim(0, 1)

ggplot(var_select.df, aes(y = var_select.df$`No. Variables Selected`, x= var_select.df$FDR)) +
  geom_point() +
  geom_smooth(span = 0.5, se = FALSE) +
  ylab("No. Variables Selected")+
  xlab("FDR")



