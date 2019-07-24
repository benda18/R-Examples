library("ggplot2")
library("ggmap")
library("readr")
#library("tidyverse")
library("lubridate")
library("dplyr")
library("reshape2")
#library("ggrepel")
#library("tidycensus")
#library("dplyr")
#library("quantreg")
library("units")
#library("hexbin")
library("ggrepel")
library("stringr") #for str_wrap()

warning("to do 1: actually depart at the time or wait for departure\nto do 2: density based on one stop per station")
# #fun_angle----
# angle <- function(x,y){
#   dot.prod <- x%*%y 
#   norm.x <- norm(x,type="2")
#   norm.y <- norm(y,type="2")
#   theta <- acos(dot.prod / (norm.x * norm.y))
#   as.numeric(theta)
# }
# x <- as.matrix(c(0,0))
# y <- as.matrix(c(0,1))
# ggplot() + 
#   geom_segment(arrow = arrow(),
#     aes(x = x[1], xend = x[2], 
#         y = y[1], yend = y[2])) +
#   coord_fixed()
# angle(t(x),y) 
#fun_bbox----
fun_bbox <- function(data, x = "stop_lon", y = "stop_lat", padding = 0) {
  df <- data.frame(left = min(data[x]) - padding,
                   bottom = min(data[y]) - padding,
                   right = max(data[x]) + padding,
                   top = max(data[y]) + padding)
  return(df)
}
#fun_map----
fun_map <- function(bbox, zoom, maptype = "toner-lite", 
                    padding = 0.004, darken.n = 0.1, darken.c = "black", 
                    ...) {
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

#vars----
a.day <- ymd(20190513)
time.jesus.left <- hm("15:00")
jesus.trav.min.hrs <- 0
jesus.trav.max.hrs <- 4
map.zoom <- 6
map.pad  <- 1
bbins    <- 4

print(as.character(wday(a.day, label = TRUE, abbr = FALSE)))
mandatory.fields <- c("route_id", "service_id", "trip_id", "direction_id", 
                      "block_id", "shape_id", "stop_id", 
                      "stop_code", "stop_lon", "stop_lat", "stop_name", 
                      "route_short_name", "route_long_name", 
                      "service_id", "monday", "tuesday", "wednesday", "thursday", 
                      "friday", "saturday", "sunday", "start_date", "end_date",
                      "direction", "shape_pt_lat", "shape_pt_lon", 
                      "shape_dist_traveled", "shape_pt_sequence", 
                      "arrival_time", "departure_time", 
                      "stop_sequence", "trip_headsign", "trip_short_name", "timepoint",
                      "stop_timezone")
#data----
feed.url <- "http://github.com/transitland/gtfs-archives-not-hosted-elsewhere/raw/master/amtrak.zip"
if(!file.exists("20170401Amtrak.zip")) {
  download.file(feed.url, "20170401Amtrak.zip")
}
unzip("20170401Amtrak.zip")

gtfs_transfers       <- read_csv("transfers.txt",       col_types = c("ccdd"))  
gtfs_agency          <- read_csv("agency.txt",           col_types = c("dcccc")) 
gtfs_trips           <- read_csv("trips.txt",           col_types = c("dddcddc")) 
gtfs_calendar        <- read_csv("calendar.txt",        col_types = c("dddddddddd"))
gtfs_routes          <- read_csv("routes.txt",          col_types = c("ddccdccc")) 
#gtfs_directions      <- read_csv("directions.txt",      col_types = c("ddc")) 
gtfs_stops           <- read_csv("stops.txt",           col_types = c("ccddcc"))
#gtfs_stop_attributes <- read_csv("stop_attributes.txt", col_types = c("dc")) 
#gtfs_shapes          <- read_csv("shapes.txt",          col_types = c("cdddd"))
gtfs_stop_times      <- read_csv("stop_times.txt",      col_types = c("dcccddd"))

gtfs_calendar$start_date <- ymd(gtfs_calendar$start_date)
gtfs_calendar$end_date   <- ymd(gtfs_calendar$end_date)

# gtfs_stop_times$arrival_time <- force_tz(a.day + hms(gtfs_stop_times$arrival_time), 
#                                          tzone = "America/New_York") %>% as.POSIXct()
# gtfs_stop_times$departure_time <- force_tz(a.day + hms(gtfs_stop_times$departure_time), 
#                                          tzone = "America/New_York") %>% as.POSIXct()

#analysis----


service_ids <- NULL
for (i in unique(gtfs_calendar$service_id)) {
  if(between(a.day, 
          gtfs_calendar$start_date[gtfs_calendar$service_id == i], 
          gtfs_calendar$end_date[gtfs_calendar$service_id == i])) {
    service_ids <- c(service_ids, i)
  }
}
gtfs_calendar <- gtfs_calendar[gtfs_calendar$service_id %in% service_ids,] %>%
  melt(., measure.vars = c("monday", "tuesday", "wednesday", "thursday", 
                           "friday", "saturday", "sunday"), 
       variable.name = "dow.name", value.name = "dow.value") %>% 
  .[.$dow.value == 1,] %>%
  .[.$dow.name == tolower(wday(a.day, label = TRUE, abbr = FALSE)),] %>% 
  .[!colnames(.) %in% c("dow.value")]


dt.jesus.left <- force_tz((a.day + time.jesus.left), tzone = "America/Chicago")

#jesus just left chicago----
jesus.routes.taken <- inner_join(gtfs_stops, gtfs_stop_times) %>%
  .[.$stop_name == "Chicago Union Station Amtrak",] %>% 
  inner_join(., gtfs_trips[c("trip_id", "route_id", "service_id")]) %>%
  .[.$service_id %in% service_ids,] %>%
  inner_join(., gtfs_routes[c("route_id", "route_long_name")]) %>%
  .$route_long_name %>%
  unique()
jesus.map <- gtfs_routes[gtfs_routes$route_long_name %in% jesus.routes.taken,] %>%
  inner_join(., gtfs_trips) %>% 
  .[.$service_id %in% service_ids,] %>%
  inner_join(., gtfs_stop_times) %>% 
  inner_join(., gtfs_stops) %>% 
  .[colnames(.) %in% mandatory.fields] %>%
  .[order(.$stop_sequence),]

jesus.map$arrival_time   <- force_tzs(a.day + hms(jesus.map$arrival_time), 
                                      tzones = jesus.map$stop_timezone, 
                                      tzone_out = "America/New_York") %>%
  as.POSIXct()
jesus.map$departure_time <- force_tzs(a.day + hms(jesus.map$departure_time), 
                                      tzones = jesus.map$stop_timezone, 
                                      tzone_out = "America/New_York") %>%
  as.POSIXct()




jesus.map$time.from.chicago <- NA
for (t in unique(jesus.map$trip_id)) {
  #for (i in 1:nrow(jesus.map[jesus.map$trip_id == t,])) {
  trip.start <- min(jesus.map$departure_time[jesus.map$trip_id == t])
  jesus.map$time.from.chicago[jesus.map$trip_id == t] <- as_units(jesus.map$arrival_time[jesus.map$trip_id == t] - trip.start, "hour")
  #}
}

#remove all inbound trips
jesus.map <- jesus.map[jesus.map$trip_headsign != "Chicago Union Station Amtrak",]

ggplot() + 
  geom_point(data = jesus.map[jesus.map$stop_sequence == 1,], 
             aes(x = arrival_time, y = factor(trip_id), color = trip_headsign)) +
  facet_grid(route_long_name+trip_headsign~., scales = "free", space = "free") +
  scale_color_viridis_d(option = "C")+
  theme(strip.text.y = element_text(angle = 0), 
        legend.position = "none") +
  labs(title = "trips departing chicago after", 
       subtitle = format(dt.jesus.left, format = "%A, %m-%d-%Y at %I:%M%p"))

#filter out routes that wern't available at time jesus left
jesus.trips <- jesus.map$trip_id[jesus.map$departure_time >= dt.jesus.left]
jesus.map <- jesus.map[jesus.map$trip_id %in% jesus.trips,]

fun_map(zoom = 5, bbox = fun_bbox(jesus.map)) + 
  geom_path(data = jesus.map, 
            aes(x = stop_lon, y = stop_lat, color = time.from.chicago, 
                group = trip_id)) +
  scale_color_viridis_c(option = "C") 

jesus.went.this.far <- jesus.map[between(jesus.map$time.from.chicago, 
                                         left = jesus.trav.min.hrs,
                                         right = jesus.trav.max.hrs),]

map.arrows <- jesus.went.this.far %>% 
  group_by(route_long_name) %>% 
  summarise(max.time.fc = max(time.from.chicago)) %>%
  inner_join(., jesus.went.this.far,
             by = c("route_long_name", 
                    "max.time.fc" = "time.from.chicago")) %>%
  group_by(stop_id, stop_name, stop_lon, stop_lat, 
           #route_long_name, 
           max.time.fc) %>%
  summarise(n = n()) %>%
  .[order(.$n, decreasing = TRUE),]

stops.along.way <- jesus.went.this.far %>%
  group_by(stop_id, 
           #route_long_name, 
           stop_lon, stop_lat) %>% 
  summarise() %>%
  .[!.$stop_id %in% map.arrows$stop_id,]
 
fun_map(zoom = map.zoom, darken.n = 0.5, darken.c = "white", maptype = "toner-background", 
        bbox = fun_bbox(jesus.went.this.far), padding = map.pad) +
  stat_density2d(data = stops.along.way, 
                 geom = "polygon", color = "black", alpha = 0.2,
                 size = 0.1,
                 bins = bbins,
                 aes(x = stop_lon, y = stop_lat, fill = ..nlevel..)) +
  geom_point(data = stops.along.way, 
             size = 1.5, color = "black", fill = "dark grey",
             shape = 21, 
             #fontface = "bold", 
             aes(x = stop_lon, y = stop_lat)) +
  geom_point(data = map.arrows, shape = 21, color = "black", fill = "white",
             size = 5,
             aes(x = stop_lon, y = stop_lat)) +
  geom_segment(data = map.arrows, size = 1, arrow.fill = "white", 
               alpha = 0.5,
               arrow = arrow(type = "closed", angle = 20, 
                             length = unit(0.125, "inches")),
               aes(x = gtfs_stops$stop_lon[gtfs_stops$stop_name == "Chicago Union Station Amtrak"], 
                   y = gtfs_stops$stop_lat[gtfs_stops$stop_name == "Chicago Union Station Amtrak"], 
                   xend = stop_lon, yend = stop_lat)) +
  geom_point(data = gtfs_stops[gtfs_stops$stop_name == "Chicago Union Station Amtrak",], 
             size = 6, shape = 24, color = "black", fill = "orange", alpha = 0.8,
             aes(x = stop_lon, y = stop_lat)) +
  theme(panel.border = element_rect(color = "black", fill = NA)) + 
  labs(title = paste("Jesus just left Chicago"),
       subtitle = paste("On an Amtrak Train between", jesus.trav.min.hrs, "and", 
                                      jesus.trav.max.hrs, "hours ago")) +
  # geom_label_repel(data = map.arrows[c("stop_lon", "stop_lat", "stop_id", "stop_name")] %>% 
  #                    .[!duplicated(.),], 
  #                  size = 3, direction = "both",
  #                  force = 0.5, alpha = 0.75,
  #                  min.segment.length = 0, 
  #                  point.padding = unit(0.1, "inches"),
  #                  label.padding = unit(0.03, "inches"),
  #                  aes(x = stop_lon, y = stop_lat, label = str_wrap(stop_name, 20))) +
  scale_fill_viridis_c(option = "C") 

