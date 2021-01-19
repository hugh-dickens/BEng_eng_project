library(sp)
library('raster')
library('rgeos')



#Cluster load
load(...)
load(...)


length_stat <- 30 #nrow(station_circles)

# Create arrays for the neighbourhoods matrix (i.e. output of this programme)
# and for a difference in area vector between the areas counted and area of the circle
# radius to check results
shape_pop_grid_mat1_30 = list()
diff_area1_30<- c()

for (i in 1:30){

stat_1<- station_circles[i,]
int_circles <- intersect(stat_1, shape_pop_conv)

# For loop sums up the area of each intersection and puts these in a vector "areas"
areas <- 0
length_intdf <- nrow(int_circles)

for (w in 1:length_intdf){
  temp <- (int_circles@polygons[[w]]@area)
  areas<- areas + temp
}

# Sums the total of areas and compares this with the expected value i.e. area of circle
diff_area1_30 <- c(diff_area1_30, areas - (300*300*pi))


# Create area ratios matrix
area_ratios <- c()
for (w in 1:length_intdf){
  temp2<- int_circles@polygons[[w]]@area / 10000
  area_ratios <- c(area_ratios, temp2)
}

# See additional document for meaning of each column
# Selects the matrix for all columns that can be analysed continuously with the most recent years
# being used throughout


pop_2014 <- int_circles@data$INW2014
increase_pop_2000_10<- int_circles@data$TINW_00_10
pop_u_15<- int_circles@data$I2014_014
pop_15_25 <- int_circles@data$I2014_1524
pop_25_45 <- int_circles@data$I2014_2544
pop_45_65 <- int_circles@data$I2014_4564
pop_65 <- int_circles@data$I2014_65PL
men <- int_circles@data$MAN_2014
women <- int_circles@data$VROUW_2014

natives <- int_circles@data$P_AUTO2014
western <- int_circles@data$P_WAL2014
nonwestern<- int_circles@data$P_NWAL2014

# Deal with percents for origin grouping vars turning them into continuous vals
levels(natives) <- c(levels(natives), "0.5","0.2", "0.675", "0.825", "0.95", "0")
natives[natives == 'geen autochtoon'] <- '0'
natives[natives =='geheim'] <- '0'
natives[natives =='nihil'] <- '0'
natives[natives == 'minder dan 40%'] <- '0.2'
natives[natives == '40% tot 60%'] <- '0.5'
natives[natives == '60% tot 75%'] <- '0.675'
natives[natives == '75% tot 90%'] <- '0.825'
natives[natives == '90% of meer'] <- '0.95'
natives_num<- as.numeric(levels(natives))[natives]
natives_pop <- natives_num*pop_2014

levels(western) <- c(levels(western), "0.04","0.115", "0.35", "0.675", "0","0.2")
western[western == 'nihil'] <- '0'
western[western =='geheim'] <- '0'
western[western == 'geen w. allochtoon'] <- '0'
western[western == 'minder dan 8%'] <- '0.04'
western[western =='8% tot 15%'] <- '0.115'
western[western == '15% tot 25%'] <- '0.2'
western[western == '25% tot 45%'] <- '0.35'
western[western == '45% of meer'] <- '0.675'
western_num<- as.numeric(levels(western))[western]
western_pop <- western_num*pop_2014

levels(nonwestern) <- c(levels(nonwestern), "0.05","0.175", "0.35", "0.56", "0","0.835")
nonwestern[nonwestern == 'nihil'] <- '0'
nonwestern[nonwestern =='geheim'] <- '0'
nonwestern[nonwestern == 'geen nw. allochtoon'] <- '0'
nonwestern[nonwestern == 'minder dan 10%'] <- '0.05'
nonwestern[nonwestern == '10% tot 25%'] <- '0.175'
nonwestern[nonwestern == '25% tot 45%'] <- '0.35'
nonwestern[nonwestern == '45% tot 67%'] <- '0.56'
nonwestern[nonwestern == '67% of meer'] <- '0.835'
nonwestern_num<- as.numeric(levels(nonwestern))[nonwestern]
nonwestern_pop <- nonwestern_num*pop_2014

houses <- int_circles@data$WON2012
valuehouses<- int_circles@data$WOZWON2012

#data2<- int_circles[,61:75, drop = TRUE]
data_tot <- cbind(pop_2014,increase_pop_2000_10,pop_u_15,pop_15_25,pop_25_45,pop_45_65,pop_65,men,women,natives_pop,western_pop,nonwestern_pop,houses,valuehouses)

#Removes any data values less than 0
#Deals with any missing or secret data by using median imputation
data_tot[data_tot==-99999]<- NA   #DATA MISSING
data_tot[data_tot==-99998]<- 0   #NULL
data_tot[data_tot==-99997]<- NA   #SECRET
data_totdf <- as.data.frame(data_tot)

data_totdf$pop_2014[is.na(data_totdf$pop_2014)] <- median(data_totdf$pop_2014, na.rm=TRUE)
data_totdf$increase_pop_2000_10[is.na(data_totdf$increase_pop_2000_10)] <- median(data_totdf$increase_pop_2000_10, na.rm=TRUE)
data_totdf$pop_u_15[is.na(data_totdf$pop_u_15)] <- median(data_totdf$pop_u_15, na.rm=TRUE)
data_totdf$pop_15_25[is.na(data_totdf$pop_15_25)] <- median(data_totdf$pop_15_25, na.rm=TRUE)
data_totdf$pop_25_45[is.na(data_totdf$pop_25_45)] <- median(data_totdf$pop_25_45, na.rm=TRUE)
data_totdf$pop_45_65[is.na(data_totdf$pop_45_65)] <- median(data_totdf$pop_45_65, na.rm=TRUE)
data_totdf$pop_65[is.na(data_totdf$pop_65)] <- median(data_totdf$pop_65, na.rm=TRUE)
data_totdf$men[is.na(data_totdf$men)] <- median(data_totdf$men, na.rm=TRUE)
data_totdf$women[is.na(data_totdf$women)] <- median(data_totdf$women, na.rm=TRUE)
data_totdf$natives_pop[is.na(data_totdf$natives_pop)] <- median(data_totdf$natives_pop, na.rm=TRUE)
data_totdf$western_pop[is.na(data_totdf$western_pop)] <- median(data_totdf$western_pop, na.rm=TRUE)
data_totdf$houses[is.na(data_totdf$houses)] <- median(data_totdf$houses, na.rm=TRUE)
data_totdf$valuehouses[is.na(data_totdf$valuehouses)] <- median(data_totdf$valuehouses, na.rm=TRUE)

#Times the area ratios by the data
shape_pop_grid_density <-  area_ratios * data_totdf

#shape_pop_grid<- rapply( shape_pop_grid_density, f=function(x) ifelse(is.na(x),0,x), how="replace" )
# Sums the columns as each row value currently contains that value for a particular partition/neighbourhood
shape_pop_grid_sum<- colSums(shape_pop_grid_density)

#shape_pop_grid_mat[1,]<- c(shape_pop_grid_sum)

shape_pop_grid_mat1_30[[i]] <- shape_pop_grid_sum # add it to your list
}



#cluster save
save(shape_pop_grid_mat1_30, file = "...")
save(diff_area1_30, file = "...")




