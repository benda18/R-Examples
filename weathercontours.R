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
                       stringsAsFactors = FALSE) %>% 
  .[!duplicated(.),]

grid.points <- data.frame(name = NA, 
                          x    = rep(min(grid.box$x):max(grid.box$x), 
                                     each = max(diff(grid.box$y)+1)) , 
                          y    = rep(min(grid.box$y):max(grid.box$y), 
                                     length.out =  max(diff(grid.box$x)+1)),
                          stringsAsFactors = FALSE) %>% 
  .[!duplicated(.),]

ggplot() + 
  geom_histogram(data = grid.points, bins = 14,
                 aes(x = x, fill = factor(y)), color = "white" ) 
ggplot() +
  geom_histogram(data = grid.points, bins = 6, 
                 aes(x = y, fill = factor(x)), color = "white")
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



base.map <- fun_map(zoom = 5, darken.n = 0.5, darken.c = "black",
                    padding = 0.5,
                    bbox = bb)  +
  theme(axis.text = element_text(), 
        axis.ticks = element_line()) + 
  scale_x_continuous(breaks = seq(-90,-70,by = 1)) +
  scale_y_continuous(breaks = seq(30,40,by = 1))

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
               aes(x = x, y = y)) 

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
wthr2 <- wthr[hour(wthr$time) %in% seq(0, 18, by = 6),] %>% .[!duplicated(.),]
wthr2$temperature <- fun_c2f(wthr2$temperature)


#vars----
hr <- 18
dt <- Sys.Date() %m+% days(1)

#plots----
base.map + 
  geom_point(data = wthr2[date(wthr2$time) == dt &
                            hour(wthr2$time) == hr,],
             #size = 4,
             aes(x = lon, y = lat, color = temperature, size = humidity)) +
  # stat_contour(data = wthr2[date(wthr2$time) == dt &  
  #                             hour(wthr2$time) == hr,], 
  #              bins = 10, 
  #              geom = "contour", 
  #              aes(x = lon, y = lat, z = temperature, color = ..level..)) +
  scale_colour_viridis_c(option = "C") +
  theme(legend.position = "right")


tempXhumid <- wthr2
tempXhumid$hr <- hour(tempXhumid$time)
tempXhumid <- tempXhumid[tempXhumid$hr %in% c(0,6,12,18),]

names(tempXhumid)

ggplot(data = tempXhumid, bins = 5,
       aes(x = temperature, y = cloudiness)) + 
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth()+
  geom_density2d(size = 1.2, aes(color = ..level..)) +
  scale_color_viridis_c(option = "C")

wind <- wthr2
wind$hr <- hour(wind$time)
wind <- wind[wind$hr %in% c(0,6,12,18),]

wind2 <- wind[date(wind$time)== Sys.Date() %m+% days(1) & 
                wind$hr == 12,]

ggplot(data = wind2, aes(x = lon, y = lat, 
                         color = windDirection,
                        angle = windDirection, 
                        radius = I(windSpeed_mps)/8)) + 
  geom_point(size = 1) +
  geom_spoke(arrow = arrow(type = "closed", angle = 20, length = unit(0.075, "inches")))+
  scale_color_viridis_c(option = "C")

wthr3 <- wthr2
wthr3$hr <- hour(wthr3$time)
ggplot() + 
  #geom_point(data = wthr3, alpha = 0.5,  aes(x = time, y = temperature)) + 
  geom_violin(data = wthr3, bins = 12,
             aes(x = factor(date(time)), 
                 y = temperature, 
                 fill = hr)) +
  geom_hline(yintercept = 85, color = "red") +
  facet_grid(hr~., scales = "free_x", space = "free_x")+
  scale_fill_viridis_c(option = "C")
