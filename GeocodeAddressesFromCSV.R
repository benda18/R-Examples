#How to Geocode a CSV of Addresses
#http://www.storybench.org/geocode-csv-addresses-r/
#https://www.littlemissdata.com/blog/maps


#----LIBRARIES----
library(readxl)
library(lubridate)
library(ggplot2)
library(ggmap)
library(dplyr)
library(data.table)
library(ggrepel)
if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup")

#----API----
register_google(key = "AIzaSyB8d2V5M5_2EjjVDxYmiWX3zMLQRE8fR7c")
#https://developers.google.com/maps/documentation/maps-static/dev-guide

#----VARIABLES----
xl_addre <- read_excel("R Line Data Tidied.xlsx", sheet = "Addresses") #loads excel 
xl_addre <- filter(xl_addre, Latitude > 0)
clon <- -78.6406935 #longitude Google Static Map Center
clat <-  35.7782689 #latitude Google Static Map Center
czom <-  14      #zoom level Google Static Map Center

#----TIDY----

#----GGMAP PROCESSING----
print("Working...on the meat")
if(exists("p") != TRUE) {
  print("get_googlemap is already complete - process skipped")
  
  p <- ggmap(get_googlemap(center = c(lon = clon, lat = clat),
                           zoom = czom, scale = 2,
                           maptype ='roadmap',    #can be roadmap, satellite, hybrid, or terrain
                           color = 'bw'))     #can be color or bw
}
p2 <- p


print("plotting the points")
xl_addre <- read_excel("R Line Data Tidied.xlsx", sheet = "Addresses") #loads excel 
xl_addre <- filter(xl_addre, Latitude > 0 & of == "Dollars")
p + geom_point(aes(x = Longitude, y = Latitude, color = xl_addre$Subcategory), data = xl_addre, alpha = 0.55, size = xl_addre$Count/10000000) + 
  theme(legend.position="bottom")



print("plotting the heatmap")
xl_addre <- read_excel("R Line Data Tidied.xlsx", sheet = "Addresses") #loads excel 
xl_addre <- filter(xl_addre, Latitude > 0 & Category == "Residential")
p2 + stat_density2d(
  aes(x = xl_addre$Longitude, y = xl_addre$Latitude, color = "blue", fill = "blue", alpha = 0.999),
  size = 0.2, bins = 5, alpha = 0.2, data = xl_addre, color = "blue", fill = "blue",
  geom = "polygon"
) +
  labs(title = "Downtown Raleigh Residential Developments") +
  geom_density2d(data = xl_addre, aes(x = Longitude, y = Latitude), bins = 5, color = "blue") #+ facet_grid(.~Category)

print("plotting the heatmap")
xl_addre <- read_excel("R Line Data Tidied.xlsx", sheet = "Addresses") #loads excel 
xl_addre <- filter(xl_addre, Latitude > 0 & Category == "Commercial")
p2 + stat_density2d(
  aes(x = xl_addre$Longitude, y = xl_addre$Latitude, color = "blue", fill = "blue", alpha = 0.999),
  size = 0.2, bins = 5, alpha = 0.2, data = xl_addre, color = "blue", fill = "blue",
  geom = "polygon"
) +
  labs(title = "Downtown Raleigh Commercial Developments") +
  geom_density2d(data = xl_addre, aes(x = Longitude, y = Latitude), bins = 5, color = "blue") #+ facet_grid(.~Category)

print("plotting the heatmap")
xl_addre <- read_excel("R Line Data Tidied.xlsx", sheet = "Addresses") #loads excel 
xl_addre <- filter(xl_addre, Latitude > 0 & Category == "Hotels")
p2 + stat_density2d(
  aes(x = xl_addre$Longitude, y = xl_addre$Latitude, color = "blue", fill = "blue", alpha = 0.999),
  size = 0.2, bins = 5, alpha = 0.2, data = xl_addre, color = "blue", fill = "blue",
  geom = "polygon"
) +
  labs(title = "Downtown Raleigh Hotel Developments") +
  geom_density2d(data = xl_addre, aes(x = Longitude, y = Latitude), bins = 5, color = "blue") #+ facet_grid(.~Category)

print("plotting the heatmap")
xl_addre <- read_excel("R Line Data Tidied.xlsx", sheet = "Addresses") #loads excel 
xl_addre <- filter(xl_addre, Latitude > 0 & Category == "Mixed use")
p2 + stat_density2d(
  aes(x = xl_addre$Longitude, y = xl_addre$Latitude, color = "blue", fill = "blue", alpha = 0.999),
  size = 0.2, bins = 5, alpha = 0.2, data = xl_addre, color = "blue", fill = "blue",
  geom = "polygon"
) +
  labs(title = "Downtown Raleigh Mixed Use Developments") +
  geom_density2d(data = xl_addre, aes(x = Longitude, y = Latitude), bins = 5, color = "blue") #+ facet_grid(.~Category)

print("plotting the heatmap")
xl_addre <- read_excel("R Line Data Tidied.xlsx", sheet = "Address") #loads excel 
xl_addre <- filter(xl_addre, Latitude > 0 & Category == "Parking")
p2 + stat_density2d(
  aes(x = xl_addre$Longitude, y = xl_addre$Latitude, color = "blue", fill = "blue", alpha = 0.999),
  size = 0.2, bins = 5, alpha = 0.2, data = xl_addre, color = "blue", fill = "blue",
  geom = "polygon"
) +
  labs(title = "Downtown Raleigh Parking Facilities") +
  geom_density2d(data = xl_addre, aes(x = Longitude, y = Latitude), bins = 5, color = "blue") #+ facet_grid(.~Category)

print("plotting the heatmap")
xl_addre <- read_excel("R Line Data Tidied.xlsx", sheet = "Addresses") #loads excel 
xl_addre <- filter(xl_addre, Latitude > 0 & Category == "Office")
p2 + stat_density2d(
  aes(x = xl_addre$Longitude, y = xl_addre$Latitude, color = "blue", fill = "blue", alpha = 0.999),
  size = 0.2, bins = 5, alpha = 0.2, data = xl_addre, color = "blue", fill = "blue",
  geom = "polygon"
) +
  labs(title = "Downtown Raleigh Office Developments") +
  geom_density2d(data = xl_addre, aes(x = Longitude, y = Latitude), bins = 5, color = "blue") #+ facet_grid(.~Category)
