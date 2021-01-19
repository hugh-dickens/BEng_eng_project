library(ggplot2)
library(ggmap)
library(tidyverse)
coord<- read.table("C:\\Users\\dicke\\OneDrive\\Documents\\3rd Year\\R&D Project\\DATA\\Coordinates.csv", header = T, sep = ",")
devtools::install_github("dkahle/ggmap")
#> Google Maps API Terms of Service: http://developers.google.com/maps/terms.
#> Please cite ggmap if you use it: see citation("ggmap") for details.

# save api key
register_google(key = "AIzaSyCGY6DuU1en_8kNbpo4M_DDSBzMTLy7AYw") 

#p<- ggplot(coord, aes( x = lat, y = lng))
 #p <- p + geom_point()
 #print(p)

# coord[1720,] and coord[2333,] not in netherlands
my_co <- coord[-c(1720, 2333),]
 
#library(rworldmap)
#newmap <- getMap(resolution = "low")
#plot(newmap)
 
#plot(newmap,
  #xlim = range(my_co$lng),
  #ylim = range(my_co$lat),
  #asp = 1)

register_google(GGMAP_GOOGLE_API_KEY = AIzaSyCGY6DuU1en_8kNbpo4M_DDSBzMTLy7AYw)
get_map("Netherlands", zoom = 7) %>% ggmap()+
geom_point(data = my_co, aes(x= my_co$lng, y = my_co$lat), col = "red", size = 0.3)





