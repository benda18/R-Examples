library("weatherr")
library("ggmap")
library("ggplot2")
#library("stringr")
#library("dplyr")
library("readr")

#functions----

fun_f2c <- function(f){
  (f-32)*5/9
  }
fun_c2f <- function(c){
  (c*9/5)+32
}

#fun_map----
fun_map <- function(bbox, zoom, maptype = "toner-lite", 
                    padding = 0.004, darken.n = 0.1, darken.c = "black") {
  map.out <- ggmap(get_stamenmap(bbox = c(left = bbox$left - padding, 
                                          bottom = bbox$bottom - padding,
                                          right = bbox$right + padding, 
                                          top = bbox$top + padding),
                                 zoom = zoom, 
                                 maptype = maptype), 
                   darken = c(darken.n, darken.c)) +
    coord_quickmap() +
    theme(axis.text = element_blank(),
          axis.title = element_blank(), 
          axis.ticks = element_blank(),
          legend.position = "none", 
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "white", 
                                          color = "white")) 
  return(map.out)
}
#fun_bbox----
fun_bbox <- function(data, x = "stop_lon", y = "stop_lat", padding = 0) {
  df <- data.frame(left = min(data[x]) - padding,
                   bottom = min(data[y]) - padding,
                   right = max(data[x]) + padding,
                   top = max(data[y]) + padding)
  return(df)
}

#vars----

grid.box <- data.frame(name = NA, 
                       x    = c(-90,-90,-77,-77), 
                       y    = c(34,39,39,34), 
                       stringsAsFactors = FALSE)
grid.points <- data.frame(name = NA, 
                          x    = rep(min(grid.box$x):max(grid.box$x), 
                                     each = max(grid.box$x)-min(grid.box$x)) , 
                          y    = rep(min(grid.box$y):max(grid.box$y), 
                                     length.out =  max(grid.box$x)-min(grid.box$x)),
                          stringsAsFactors = FALSE)
#data----
center.nc <- state.center %>% 
  as.data.frame() %>% 
  .[state.abb == "NC",]

bb <- fun_bbox(data = center.nc, x = "x", y = "y", 
               padding = 4) %>%
  as.data.frame()
bb <- data.frame(left = min(grid.points$x), 
                 right = max(grid.points$x), 
                 bottom = min(grid.points$y), 
                 top = max(grid.points$y))



base.map <- fun_map(zoom = 5, darken.n = 0.5, darken.c = "white",
                    padding = 0.5,
                    bbox = bb)  +
  theme(axis.text = element_text(), 
        axis.ticks = element_line())
base.map + 
  geom_point(data = center.nc, color = "white", 
             fill = "blue", shape = 21, size = 4,
             aes(x = x, y = y)) +
  geom_point(data = grid.points, 
             shape = 21, color = "dark red", fill = "orange", 
             size = 3, 
             aes(x = x, y = y)) +
  geom_polygon(data = grid.box, 
               fill = NA, color = "blue", 
               aes(x = x, y = y)) + 
  scale_x_continuous(breaks = seq(-90,-70,by = 1)) +
  scale_y_continuous(breaks = seq(30,40,by = 1))

#pull weather data----
args(locationforecast)

if(!exists("wthr")) {
  wthr <- NULL
  for (i in 1:nrow(grid.points)) {
    temp.w     <- locationforecast(lon = grid.points$x[i], lat = grid.points$y[i])
    temp.w$lon <- grid.points$x[i]
    temp.w$lat <- grid.points$y[i]
    wthr <- rbind(wthr, 
                  temp.w)
  }
}
write_csv(wthr, "weather_grid.csv", append = FALSE)

#tidy----
wthr2 <- wthr[hour(wthr$time) %in% seq(0, 18, by = 6),]
wthr2$temperature <- fun_c2f(wthr2$temperature)


#vars----
hr <- 18
dt <- Sys.Date() %m+% days(1)

#plots----
base.map + 
  geom_point(data = wthr2[date(wthr2$time) == dt &  
                            hour(wthr2$time) == hr,], 
             size = 4,
             aes(x = lon, y = lat, color = temperature)) +
  geom_contour(data = wthr2[date(wthr2$time) == dt &  
                              hour(wthr2$time) == hr,], 
               aes(x = lon, y = lat, z = temperature)) +
  scale_colour_viridis_c(option = "C") +
  theme(legend.position = "right")


