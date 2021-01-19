library(dplyr)
library(raster)
library(tidyverse)
library(sf)
library(sp)
library(rgdal)

#Home loading data
load("...")
load("...")


# Sums up the total energy for all of the chargepoints instead of it being per
# transaction
temporary_data <- Transactions
temporary_data %>% select(ChargePoint, TotalEnergy, lat, lon)
tot_energy.agg <- aggregate(cbind(TotalEnergy) ~ ChargePoint, data = temporary_data, FUN = sum);

# Remove duplicated rows from my data set, then bind the latitude and longitude to the 
# aggregated energy data frame
my_data <- distinct(temporary_data, ChargePoint, .keep_all = TRUE)
tot_energy <- cbind.data.frame(my_data$lat, my_data$lon, tot_energy.agg)
tot_energy <- tot_energy[order(my_data$lat),]
colnames(tot_energy) <- c("lat", "long", "ChargePoint", "TotalEnergy")

# Convert the coordinates to the correct CRS
tot_energy_CRS <- st_as_sf(tot_energy, coords = c("long", "lat"), crs = 4326)%>%
  #st_transform(28992)

save(tot_energy_CRS, file = "tot_energy_CRS.rdata")


# Creates a circle of radius 300m around the charging stations
stat_circ_buurt <- st_buffer(tot_energy_CRS, dist = 300)

save(stat_circ_buurt, file="stat_circ_buurt.rdata")

# Has been tested to check that all of the spatial polygons are distinct 
#Convert to shape population data frame to sf format
buurt_2015_sf <- buurt_2015%>% sf::st_as_sf()%>%st_transform(28992)

save(buurt_2015_sf, file = "buurt_2015_sf.rdata")

#plot(buurt_2015@polygons[[4]]@Polygons[[1]]@coords)
length_stat <- 1 #nrow(stat_circ_buurt)

# Create arrays for the neighbourhoods matrix (i.e. output of this programme)
# and for a difference in area vector between the areas counted and area of the circle
# radius to check results
buurt_2015_mat<- matrix(NA, nrow=length_stat, ncol = 119 )
diff_area_buurt <- c()

# For loop to carry out analysis over the entire dataset of charging stations
for (i in 1:length_stat){

#choose first station
station_1 <- stat_circ_buurt[(1352),]

# Intersect the circle with the shapes
int_shapes <- st_intersection(station_1, st_buffer(buurt_2015_sf, 0))
#int_shapes <- st_intersection(station_1, buurt_2015_sf)

plot(int_shapes$geometry)

area_buurt <- c()
length_int_shapes <- nrow(int_shapes)
for (w in 1:length_int_shapes){
  temp <- st_area(int_shapes$geometry[w])
  area_buurt<- c(area_buurt, temp)
}


# Sums the total of areas and compares this with the expected value i.e. area of circle
sum_area_buurt <- sum(area_buurt)
diff_area_buurt <- c(diff_area_buurt, (sum(area_buurt) - (300*300*pi)))

area_ratio <- c()
for (z in 1:length_int_shapes){
  val<- as.numeric(int_shapes$BU_CODE[z])
  tot_area <- st_area(buurt_2015_sf[val,])
  temp2<- area_buurt[z] / tot_area
  area_ratio<-c(area_ratio, temp2)
}


# -99999999: the figure is unknown, insufficiently reliable or secret
#All variables currently being used

# 12th column
postcode <- int_shapes$DEK_PERC
levels(postcode) <- c(levels(postcode), "0.95","0.85", "0.75", "0.65", "0.55","0.25","0")
postcode[postcode == '1'] <- '0.95'
postcode[postcode == '2'] <- '0.85'
postcode[postcode == '3'] <- '0.75'
postcode[postcode == '4'] <- '0.65'
postcode[postcode == '5'] <- '0.55'
postcode[postcode == '6'] <- '0.25'
postcode[postcode == '-99999999'] <- '0'
col12<- as.numeric(levels(postcode))[postcode]

#14th column
urbanness <- int_shapes$STED
levels(urbanness) <- c(levels(urbanness), "3000", "2000", "1250", "750", "250", "0")
urbanness[urbanness == '1'] <- '3000'
urbanness[urbanness == '2'] <- '2000'
urbanness[urbanness == '3'] <- '1250'
urbanness[urbanness == '4'] <- '750'
urbanness[urbanness == '5'] <- '250'
urbanness[urbanness == '-99999999'] <- '0'
col14 <- as.numeric(levels(urbanness))[urbanness]

#Turns factored data into numeric values, 13th column
col13 <- as.numeric(levels(int_shapes$OAD))[int_shapes$OAD]

# Turns the unfactored data into numeric values within for loop
# Create the matrices first
col15_35 <- matrix(NA, nrow = length_int_shapes, ncol = 21)
col37_62 <- matrix(NA, nrow = length_int_shapes, ncol = 26)
col64_70 <- matrix(NA, nrow = length_int_shapes, ncol = 7)
col71_127 <- matrix(NA, nrow = length_int_shapes, ncol = 57)
col128_130 <- matrix(NA, nrow = length_int_shapes, ncol = 3)
for (t in 1:length_int_shapes){
  #15th - 35th column
  temp2<- unlist(int_shapes[t, 14:34, drop =TRUE])
  # Skip GEM_HH_GR as it is a decimal value, 37th - 62nd column
  temp3 <- unlist(int_shapes[t, 36:61, drop =TRUE])
  #skip AUTO_HH same reasoning, 64th - 70th column
  temp4 <- unlist(int_shapes[t, 63:69, drop =TRUE])
  #71st - 127th column
  temp5 <- unlist(int_shapes[t ,70:126, drop = TRUE])
  # 128th - 130th column
  temp6 <- unlist(int_shapes[t ,127:129, drop = TRUE])
  # Turn to numeric values
  temp2n <- as.numeric(levels(temp2))[temp2]
  temp3n <- as.numeric(levels(temp3))[temp3]
  temp4n <- as.numeric(levels(temp4))[temp4]
  #temp5n <- as.numeric(levels(temp5))[temp5] #Already numeric
  temp6n <- as.numeric(levels(temp6))[temp6]
  col15_35[t,] <- c(temp2n)
  col37_62[t,] <- c(temp3n)
  col64_70[t,] <- c(temp4n)
  col71_127[t,] <- c(temp5)
  col128_130[t,] <- c(temp6n)
}

#Col 36 - GEM_HH_GR
col36 <-int_shapes$GEM_HH_GR

#Col 63 - AUTO_HH
col63 <- int_shapes$AUTO_HH

data_used <- cbind(col12, col13, col14, col15_35, col36, col37_62, col63, col64_70, col71_127,col128_130)

data_used[data_used==-99999]<- 0   #DATA MISSING
data_used[data_used==-99999999] <- 0

# Times the ratio by the data
buurt_2015_density <-  area_ratio * data_used

# Sums the columns as each row value currently contains that value for a particular neighbourhood
buurt_2015_sum<- colSums(buurt_2015_density)

#Add the value to the overall matrix for neighbourhoods
buurt_2015_mat[i,]<- c(buurt_2015_sum)

}


save(diff_area_buurt_1593_end, file = "diff_area_buurt_1593_end.rdata")
save(buurt_2015_mat_1593_end, file = "buurt_2015_mat_1593_end.rdata")


diff_area_buurt_1593_end  <- diff_area_buurt
summary(buurt_2015_mat)

buurt_2015_mat_1593_end<- buurt_2015_mat


####629 doesnt work
buurt_2015_mat[,1]


a<- buurt_2015_mat_1_847
b<- buurt_2015_mat_847_1593
c<-buurt_2015_mat_1593_end

buurt_2015_final <- rbind(a,b,c)

save(buurt_2015_final, file ="buurt_2015_final.rdata")


diff_area_buurt_missing<- c(diff_area_buurt_1_627, diff_area_buurt_847_1593, diff_area_buurt_1593_end)

save(diff_area_buurt_missing, file="diff_area_buurt_missing.rdata")

get_names1<- names(buurt_2015_sf)

get_names<- get_names1[9:127]

save(get_names, file = "get_names.rdata")

