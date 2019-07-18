#https://medium.com/@traffordDataLab/querying-apis-in-r-39029b73d5f1
library(tidyverse) 
library(httr) 
library(jsonlite)
library(lubridate)
library(ggrepel)
library(ggmap)

#vars
query.for.seconds <- 60*60
query.cycle.seconds <- 60

cn <- c("generated_on", "data.20.last_updated_on", 
        "data.20.speed", "data.20.vehicle_id", 
        "data.20.segment_id", "data.20.route_id", 
        "data.20.location.lat", "data.20.location.lng", 
        "data.20.heading")

url <- "https://transloc-api-1-2.p.rapidapi.com/vehicles.json?routes=4000032&agencies=20"

#functions----
fun_bbox <- function(data, x = "stop_lon", y = "stop_lat", padding = 0) {
  df <- data.frame(left = min(data[x]) - padding,
                   bottom = min(data[y]) - padding,
                   right = max(data[x]) + padding,
                   top = max(data[y]) + padding)
  return(df)
}

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

#vehicles----
now <- Sys.time()
fut <- Sys.time() %m+% seconds(query.for.seconds)
if (!exists("rline.test")) {
  rline.test <- NULL
  #}else{
  #  rline.test <- rline.test[colnames(rline.test) != "time"]
}
while (now < fut) {
  now <- Sys.time()
  request.v <- GET(url = url, 
                   add_headers("X-RapidAPI-Host" = "transloc-api-1-2.p.rapidapi.com",
                               "X-RapidAPI-Key" = "TNmlaSj4zumshYECd4UlIDGBNMldp1BvclGjsnRyOxyNJCx0s7"))
  
  # if(request.v$status_code != 200) {
  #   warning("non-200 response - TB")
  # }else{
  #   print("Success - TB")
  # }
  
  response.v <- content(request.v, as = "text", encoding = "UTF-8") %>%
    fromJSON(., flatten = TRUE) %>% 
    data.frame() %>%
    .[cn]
  response.v$time <- NA
  rline.test <- rbind(rline.test, 
                      response.v)
  rm(response.v)
  
  #time tidy----
  rline.test$time <- rline.test$data.20.last_updated_on %>% ymd_hms(., tz = "America/New_York") 
  
  

  write_csv(rline.map,"rline-map.csv", append = TRUE)
  Sys.sleep(query.cycle.seconds)
  print(Sys.time())
}

#bad.data tidy----
buses <- unique(rline.test$data.20.vehicle_id[!rline.test$data.20.vehicle_id %in% 4004920])
rline.map <- rline.test[rline.test$data.20.vehicle_id %in% buses,]
#plot----
fun_map(zoom = 15, bbox = fun_bbox(data = rline.map, padding = 0, 
                                   x = "data.20.location.lng", 
                                   y = "data.20.location.lat")) +
  geom_path(data = rline.map, size = 2,
            aes(x = data.20.location.lng, 
                y = data.20.location.lat, 
                color = data.20.vehicle_id)) +
  geom_point(data = rline.map, size = 3, shape = 21, 
             color = "black", 
             aes(x = data.20.location.lng, 
                 y = data.20.location.lat, 
                 fill = data.20.vehicle_id)) +
  geom_label_repel(data = rline.map, alpha = 0.7,
                   min.segment.length = 0, size = 3,
                   fontface = "bold", 
                   point.padding = unit(0.125, "inches"), 
                   arrow = arrow(type = "closed", angle = 20, 
                                 length = unit(0.1, "inches")),
                   aes(x = data.20.location.lng, 
                       y = data.20.location.lat, 
                       label = format(time, format = "%I:%M"))) +
  theme(legend.position = "right")
