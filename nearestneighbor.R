#-------------------------------
#Nearest Neighbor / Points within Poly
#-------------------------------
#https://cran.r-project.org/web/packages/gridExtra/vignettes/tableGrob.html

#LIBRARIES----
library("hexbin")
library("tidyverse") #A
library("sp") #A
library("dplyr") #A
library("ggplot2") #A
#library("ggpmisc") #for adding table inside of plot 
#library("forcats")
library("readxl") #A
library("ggmap") #A
#library("reshape2") #for melt
#library("raster") #for pointDistance()
#library("gtools") #for smartbind
#library("hexbin") #for geom_hex
#library("lubridate")
#library("ggrepel")
#library("scales")
#library("rgdal") #shapefiles
library("proj4") #A   coordinate transformation
#library("gridExtra") #annotate
#library("grid")
library("shapefiles") #A

#delete pngs----
idpngs <- as.vector(list.files())
dlpngs <- grep(".png$",idpngs, ignore.case = TRUE, value = TRUE)
unlink(dlpngs)

#coordinate transformations with proj4
#https://www.rforge.net/doc/packages/proj4/transform.html
#projections found here in wayback machine:
#http://web.archive.org/web/20160802172057/http://www.remotesensing.org/geotiff/proj_list/

#DATA import----
##make downtown plan boundary. ----
dtplan <- shapefiles::read.shapefile("DRA_Bounds_WGS")
dtplan_xy <- as.data.frame(dtplan[["shp"]][["shp"]][[1]][["points"]])

#others
(r_stops   <- read_excel("GrOnOffMayJul2018.xlsx", sheet = "APC by Route May-July")) #loads boarding data
every_stop <- r_stops
r_stops <- filter(r_stops, 
                  STOP_ID == 9913 | STOP_ID == 9914 | STOP_ID == 9916 | STOP_ID == 9917 |
                    STOP_ID == 9918 | STOP_ID == 9900 | STOP_ID == 9901 | STOP_ID == 9902 | 
                    STOP_ID == 9903 | STOP_ID == 9920 | STOP_ID == 9732 | STOP_ID == 9921 | 
                    STOP_ID == 9905 | STOP_ID == 9909 | STOP_ID == 9908 | STOP_ID == 8571 | 
                    STOP_ID == 9910 | STOP_ID == 9911 | STOP_ID == 9912 | STOP_ID == 8183 )
r_stops <- r_stops %>% group_by(STOP_ID) %>% summarise(Longitude = mean(AVG_LONGITUDE), 
                                                       Latitude = mean(AVG_LATITUDE))
r_stops_ids <- r_stops
(r_path  <- read_excel("rlineroute.xlsx", sheet = "rlineroute")) #loads rline path base data for plots
(hfa2    <- read_excel("DTparcelpoints2.xlsx", sheet = "parcelstabledowntown"))	 #heated floor area	
#filter out stops outside of downtown plan study area
hfa2_pointsinpoly <- point.in.polygon(hfa2$Longitude, hfa2$Latitude, dtplan_xy$X, dtplan_xy$Y)
hfa2 <- hfa2[hfa2_pointsinpoly == 1,]

#View(hfa2)
#create a list of xy coordinates from hfa2 to transform
trans_coords <- hfa2[,11:12]

#coordinate transformation via projection
pc <- ptransform(trans_coords/180*pi, 
                 '+proj=latlong +ellps=sphere',
                 '+proj=merc +ellps=sphere')
#save projected coords back to hfa2 as new fields for circle creation
hfa2$circx <- pc$x
hfa2$circy <- pc$y

#SET vars----

#pts <- sample(1:nrow(hfa2), 4) #4 random hfa2 points selected to make radius from
#hfa_pts <- hfa2[pts,]
#hfa_pts2 <- hfa2[pts,] %>% transmute(x1 = Longitude, y1 = Latitude)

dd_r   <- 402 #meters in radius == 1/4 mile 


#INPUT POINTS----
#input_pts <- transmute(r_stops, longitude = r_stops$AVG_LONGITUDE, latitude = r_stops$AVG_LATITUDE)
input_pts <- r_stops
ipc <- ptransform(input_pts[,2:3]/180*pi, 
                  '+proj=latlong +ellps=sphere',
                  '+proj=merc +ellps=sphere')
input_pts$circx <- ipc$x
input_pts$circy <- ipc$y

points_x <- array(input_pts$circx) #Longitude for queried points of radius dd_r
points_y <- array(input_pts$circy) #Latitude for queried points of radius dd_r
points_id <- array(input_pts$STOP_ID) #retain stop_ids
# points_x <- array(hfa_pts$circx) #Longitude for queried points of radius dd_r
# points_y <- array(hfa_pts$circy) #Latitude for queried points of radius dd_r
#View(input_pts)
#INPUT POINTS----


#(pxy <- as_tibble(t(rbind(points_x, points_y))))

#FUNCTIONS----
#plot circle of radius dd_r
circleFun <- function(center = c(0,0), diameter = dd_r, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

#RLINE BUS STOPS INPUT
# View(r_stops)
# r_stops <- filter(r_stops, 
#                 STOP_ID == 9913 | STOP_ID == 9914 | STOP_ID == 9916 | STOP_ID == 9917 |
#                   STOP_ID == 9918 | STOP_ID == 9900 | STOP_ID == 9901 | STOP_ID == 9902 | 
#                   STOP_ID == 9903 | STOP_ID == 9920 | STOP_ID == 9732 | STOP_ID == 9921 | 
#                   STOP_ID == 9905 | STOP_ID == 9909 | STOP_ID == 9908 | STOP_ID == 8571 | 
#                   STOP_ID == 9910 | STOP_ID == 9911 | STOP_ID == 9912 | STOP_ID == 8183 )
# r_stops <- r_stops %>% group_by(STOP_ID) %>% summarise(Longitude = mean(AVG_LONGITUDE), 
#                                                      Latitude = mean(AVG_LATITUDE))



#circle work 
circs1 <- NULL
for (i in 1:(sum(length(c(points_x, points_y)))/2)) {
  #print(i)
  #circs1$uid[i] <- NA
  circs1 <- rbind(circs1, circleFun(c(points_x[i], points_y[i])))
  
  #circs1 <- cbind(circs1, i)
}
#head(circs1)


n <- 0
for (i in 1:nrow(circs1)){
  circs1$row[i] <- n
  print(i)
  if(i/100 == as.integer(i/100)) n <- n + 1
}
(circs1)
#View(circs1)

#add uid
circs1$uid <- NA
for (i in circs1$row) circs1$uid[circs1$row == i] <- points_id[i+1]



## /180*pi
#now we transform those projected circs1 coords back to lon/lat
proj_circs1xy <- circs1[,1:2]
circs1_lonlat <- ptransform(proj_circs1xy,
                            '+proj=merc +ellps=sphere', 
                            '+proj=latlong +ellps=sphere') 
circs1_lonlat$x <- circs1_lonlat$x * 180/pi
circs1_lonlat$y <- circs1_lonlat$y * 180/pi
circs1_lonlat

circs1$x <- circs1_lonlat$x
circs1$y <- circs1_lonlat$y

pxy <- as_tibble(t(rbind(circs1$x, circs1$y)))

#somethign else
pxy
#View(pxy)
names(pxy)
colnames(pxy) <- c("points_x", "points_y")
## [1] "points_x" "points_y"
names(hfa2)
## [1] "PIN_NUM"    "TOTAL_VALU" "PROPDESC"   "HEATEDAREA"
## [5] "SITEADDRE"  "YEAR_BUILT" "LANDDECODE" "ACTIVITY"  
## [9] "TOTUNITS"   "RL_Market"  "Longitude"  "Latitude" 

##definitions
# r_x - x values of radius centroid
# r_y - y values of radius centroid
# hfa_x - all hfa points
# hfa_y - all hfa points


#### find points from hfa2 within circs1 using same coordinate systems.  

## in each circle
hfa2$pts_in_cir <- 0
hfa2$number_of_circles <- 0
for (i in unique(circs1$row)) {
  poly <- circs1[circs1$row == i,]
  polyx <- poly$x
  polyy <- poly$y
  
  for (j in 1:nrow(hfa2)) {
    #print(paste(i,j))
    ptx <- hfa2$Longitude[j]
    pty <- hfa2$Latitude[j]  
    #sdid <- c(sdid, )
    #pip <- point.in.polygon(ptx, pty, polyx, polyy)
    if (point.in.polygon(ptx, pty, polyx, polyy) > 0) {
      hfa2$pts_in_cir[j] <- 1
      hfa2$number_of_circles[j] <- hfa2$number_of_circles[j] + 1
    }
    #print(pip)
  }
}



##OUTPUT STATS----
output_stats <- hfa2 %>% group_by(pts_in_cir) %>% summarise(parcels = n(), heated_area = sum(HEATEDAREA))
#hfa_out <- output_stats$heated_area[output_stats$pts_in_cir == 0]
#hfa_in2 <- output_stats$heated_area[output_stats$pts_in_cir == 1]
hfa_tha <- sum(output_stats$heated_area)
hfa_tp <- sum(output_stats$parcels)
output_stats <- rbind(output_stats, c(sum(output_stats$pts_in_cir), 
                                      sum(output_stats$parcels),
                                      sum(output_stats$heated_area)))
output_stats <- cbind(output_stats, (output_stats$heated_area / hfa_tha),
                      deparse.level = 0)
output_stats <- cbind(output_stats, (output_stats$parcels / hfa_tp),
                      deparse.level = 0)
(colnames(output_stats) <- c("n","Parcels", "Heated Floor Area (sqft)", "Percent of HFA", "Percent of Parcels"))
rownames(output_stats) <- c("Greater 1/4 mile", "Less than 1/4 mile", "Total")
#format
output_stats$`Heated Floor Area (sqft)` <- formatC(output_stats$`Heated Floor Area (sqft)`,format="d", big.mark=",")
output_stats$Parcels <- formatC(output_stats$Parcels,format="d", big.mark=",")
View(output_stats)
##OUTPUT STATS----

##OUTPUT PLOT----
vbins    <- 7       #set number of bins
(alpha_c <- (0.5 + (10-vbins)/30))      #transparency of countour lines
(alpha_r <- (0.5 - (10-vbins)/30))    #transparency of raster bins
color_c <- "orange"  #color of contour lines
c_low   <- "light blue" #raster color for min value
c_high  <- "blue"    #raster color for max value


nn <- qmplot(x = Longitude, 
             y = Latitude, 
             data = hfa2,
             geom = "point",
             maptype = "toner-lite", 
             darken = c(0.7, "black"),
             main = paste("Heated Floor Area within\n 1/4 mile of Existing R-Line Stops"),
             size = I(0.25),
             color = I("light blue"), 
             alpha = I(0.4),
             margins = TRUE,
             force = FALSE,
             padding = 0.02,
             extent = "panel", #, #extent, device, normal, panel
             f = 0.4) + 
  #remove axis labels and ticks
  theme(axis.ticks = element_blank(), 
        axis.text.x = element_blank(), axis.text.y = element_blank(), 
        axis.title.y = element_blank(), axis.title.x = element_blank()
  ) +
  coord_map("bonne", lat0 = 35.8, 
            xlim = c(-78.6560,-78.630),
            #ylim = c( 35.7685, 35.793)
            ylim = c(35.76655, 35.79664)
  ) +
  geom_path(data = dtplan_xy, aes(x = X, y = Y, color = I("pink"))) +
  geom_path(data = r_path, aes(x = x, y = y, color = I("white"), size = I(1.75), alpha = I(0.8))) #+
#rline route
nn + geom_path(data = circs1, aes(x, y, alpha = I(0.95), color = I("orange"), group = row)) + 
  labs(caption = paste(scales::percent(output_stats$`Percent of HFA`[2]),"of total heated floor area\nis located within 1/4 mile radius\nof existing R-Line stops")) +
  #geom_polygon(data = circs1, aes(x, y, alpha = I(0.5), color = I("black"), group = row)) + 
  geom_point(data = input_pts, aes(x = circx, y = circy)) +
  
  geom_point(data = filter(hfa2, pts_in_cir == 1), aes(x = Longitude, y = Latitude, color = I("orange"), size = I(0.5))) #+ 
# scale_fill_gradient("legend title here",low = c_low, high = c_high) +
# stat_density2d(data = hfa2[rep(row.names(hfa2), round(hfa2$HEATEDAREA/360)), 1:15],
#                bins = 10,
#                n = 200,
#                aes(x = Longitude, y = Latitude, 
#                    #alpha = alpha_c, 
#                    alpha = I(0.9),
#                    color = I("orange")
#                    #color = as.factor(pts_in_cir+1)
#                    #color = ..level..
#                )) +
#annotate(geom = "table", x = median(d_all$lonx), y = min(d_all$laty), 
#label = list( s_dall[1:2]), vjust = 1.4, hjust = 0.5) 

#nn 

# ##make downtown plan boundary. ----
# dtplan <- shapefiles::read.shapefile("DRA_Bounds_WGS")
# dtplan_xy <- as.data.frame(dtplan[["shp"]][["shp"]][[1]][["points"]])


##make a layer of all stops as follows: 

stop.density <- t(data.frame(c(NA,NA,NA,NA,NA), 
                             row.names = c("stop_id", "ons_plus_offs", "hfa_quarter_mile", "x", "y")))
for (i in r_stops_ids$STOP_ID) {
  stop.density <- rbind(stop.density, c(i,NA,NA,NA,NA))
}

rownames(stop.density) <- NULL
stop.density <- as.data.frame(stop.density) %>% filter(is.na(stop_id) == FALSE)

for (i in 1:nrow(r_stops_ids)) {
  stop.density$x[i] <- r_stops_ids$Longitude[i]
  stop.density$y[i] <- r_stops_ids$Latitude[i]
}
View(stop.density)

#stop ons + offs
ess <- every_stop %>% group_by(STOP_ID) %>% summarise(onoff = DAILY_AVG_ON+DAILY_AVG_OFF)
for (i in stop.density$stop_id){
  stop.density$ons_plus_offs[stop.density$stop_id == i] <- ess$onoff[ess$STOP_ID == i]
}

#pull it all together
for (i in stop.density$stop_id){
  #print(i)
  id <- stop.density$stop_id
  #stop.density$hfa_quarter_mile[i] == sum(???)
  ptx <- hfa2$Longitude
  pty <- hfa2$Latitude
  plx <- circs1$x[circs1$uid == i]
  ply <- circs1$y[circs1$uid == i]
  pip <- point.in.polygon(ptx, pty, plx, ply)
  stop.density$hfa_quarter_mile[stop.density$stop_id == i] <- sum(hfa2$HEATEDAREA[pip > 0])
}


# ggplot(data = stop.density, aes(x = ons_plus_offs, y = hfa_quarter_mile)) + geom_point() +
#   geom_quantile() 
# ggplot() + geom_point(data = stop.density, 
#                       aes(x = x, y = y, size = (ons_plus_offs/hfa_quarter_mile*100000))) +
#   coord_map("bonne", lat0 = 35.8, 
#             xlim = c(-78.6560,-78.630),
#             #ylim = c( 35.7685, 35.793)
#             ylim = c(35.76655, 35.79664)
#   )

#more stats----
ggplot() + 
  geom_point(data = hfa2, aes(x = Longitude, y = Latitude, color = I("grey"),size = (number_of_circles)))+
  coord_map("bonne", lat0 = 35.8, 
            xlim = c(-78.6560,-78.630),
            #ylim = c( 35.7685, 35.793)
            ylim = c(35.76655, 35.79664)
  ) +
  scale_fill_gradient("legend title here",low = "blue", high = "red") +
  stat_density2d(data = hfa2[rep(row.names(hfa2), hfa2$number_of_circles), 1:16],
                 bins = 5,
                 geom = "polygon",
                 n = 200,
                 aes(x = Longitude, y = Latitude,
                     #alpha = alpha_c,
                     alpha = I(0.3),
                     #color = I("red"),
                     #color = as.factor(pts_in_cir+1)
                     fill = ..level..
                 ))
# 
# ggplot(data = hfa2, aes(x = hfa2$number_of_circles, y = HEATEDAREA)) + 
#   geom_point() 
# 
# ggplot() + geom_histogram(data = hfa2, aes(x = hfa2$number_of_circles))


proximity_summary <- hfa2 %>% group_by(LANDDECODE, number_of_circles) %>% summarise(n = n(), hfa = median(HEATEDAREA))

qmplot(x = Longitude, 
       y = Latitude, 
       data = hfa2,
       geom = "point",
       maptype = "toner-lite", 
       darken = c(0.7, "black"),
       main = paste("Heated Floor Area within\n 1/4 mile of Existing R-Line Stops"),
       size = I(0.25),
       color = I("light blue"), 
       alpha = I(0.0),
       margins = TRUE,
       force = FALSE,
       padding = 0.02,
       extent = "panel", #, #extent, device, normal, panel
       f = 0.4) + 
  #remove axis labels and ticks
  theme(axis.ticks = element_blank(), 
        axis.text.x = element_blank(), axis.text.y = element_blank(), 
        axis.title.y = element_blank(), axis.title.x = element_blank()
  ) +
  coord_map("bonne", lat0 = 35.8, 
            xlim = c(-78.6560,-78.630),
            #ylim = c( 35.7685, 35.793)
            ylim = c(35.76655, 35.79664)
  ) +
  #rline route
  geom_path(data = r_path, 
            aes(x = x, y = y, color = I("white"), size = I(1.75), alpha = I(0.2))) +
  geom_path(data = dtplan_xy, 
            aes(x = X, y = Y, size = I(1), color = I("pink"))) +
  geom_point(data = hfa2, 
             aes(x = Longitude, y = Latitude, size = I(0.5), color = I("white"))) +
  facet_grid(~number_of_circles)


##
nn + geom_point(data = hfa2, 
                aes(x = Longitude, 
                    y = Latitude, 
                    size = number_of_circles))
nn + geom_bin2d(data = hfa2, 
                aes(x = Longitude, 
                    y = Latitude,
                    weight = number_of_circles,
                    alpha = I(0.4))) +
  scale_fill_gradient(trans = "log10",
                      low = "yellow",
                      high = "red")
#plot
hfa2$min_stops_walkable <- NA
for (i in 1:nrow(hfa2)){
  #print(paste(hfa2$number_of_circles[i],"+"))
  if(hfa2$number_of_circles[i] == 1) hfa2$min_stops_walkable[i] <- as.character("One")
  if(hfa2$number_of_circles[i] >= 2) hfa2$min_stops_walkable[i] <- as.character("Multiple")
  if(hfa2$number_of_circles[i] == 0) hfa2$min_stops_walkable[i] <- as.character("None")
}  
#hfa2$min_stops_walkable <- as.factor(hfa2$min_stops_walkable)

nn +
  geom_point(data = hfa2, 
             aes(x = Longitude, 
                 y = Latitude, 
                 size = I(0.5))) +
  facet_grid(~min_stops_walkable)

####--------------------------------
#NEW ACTIVITY - FIND PRIME LOCATIONS
####--------------------------------
best <- 1
n <- 0

coln <- colnames(hfa2)
if(length(coln[coln == "qmw_hfa"]) == 0) hfa2$qmw_hfa <- NA
if(length(coln[coln == "best"]) == 0) hfa2$best <- 0
if(length(coln[coln == "excluded"]) == 0) hfa2$excluded <- NA


hfa2$qmw_hfa <- NA
hfa2$best <- 0
hfa2$excluded <- NA

for (k in 1:nrow(hfa2[is.na(hfa2$excluded) == TRUE,])){
  if(k > 200) stop("stop dammit")
  #hfa2$excluded[is.na(hfa2$best) == FALSE | is.na(hfa2$excluded) == FALSE] <- 1
  hfa2$rank <- 0
  for (i in 1:nrow(hfa2[is.na(hfa2$excluded) == TRUE,])){
    if(is.na(hfa2$qmw_hfa[is.na(hfa2$excluded) == TRUE][i] == TRUE)){
      parcel.in.hfa2 <- hfa2[is.na(hfa2$excluded) == TRUE,][i,]
      x <- parcel.in.hfa2$circx
      y <- parcel.in.hfa2$circy
      ptx <- hfa2$circx[hfa2$PIN_NUM != parcel.in.hfa2$PIN_NUM]
      pty <- hfa2$circy[hfa2$PIN_NUM != parcel.in.hfa2$PIN_NUM]
      cir <- circleFun(c(x,y))
      pox <- cir$x
      poy <- cir$y
      pic <- point.in.polygon(ptx, pty, pox, poy)
      pic.hfa <- cbind(hfa2[hfa2$PIN_NUM != parcel.in.hfa2$PIN_NUM,], pic)
      pic.hfa <- pic.hfa %>% filter(pic > 0)
      ha <- as.vector(pic.hfa$HEATEDAREA)
      ha <- sum(ha)
      hfa2$qmw_hfa[is.na(hfa2$excluded) == TRUE][i] <- ha
    }
  }
  hfa2$rank[is.na(hfa2$excluded) == TRUE] <- rank(1/hfa2$qmw_hfa[is.na(hfa2$excluded) == TRUE])
  hfa2$best[hfa2$rank == 1] <- k
  hfa2$excluded[hfa2$rank == 1] <- 1
  
  n <- n + 1
  if (n > 300) stop("too many iterations")
  
  # id those parcels within "best" and 
  # assign them hfa2$excluded = 1
  #1. make the circle
  ptx <- hfa2$circx[is.na(hfa2$excluded) == TRUE]
  pty <- hfa2$circy[is.na(hfa2$excluded) == TRUE]
  cirb <- circleFun(c(hfa2$circx[hfa2$rank == 1], hfa2$circy[hfa2$rank == 1]))
  pox <- array(cirb$x)
  poy <- array(cirb$y)
  pic2 <- point.in.polygon(ptx, pty, pox, poy)
  length(pic2)
  hfa2$excluded[is.na(hfa2$excluded) == TRUE][pic2 == 1] <- k #best
  best <- best + 1
  #2 
  
  print(nrow(hfa2[is.na(hfa2$excluded)==TRUE,]))
}

hfa2$excluded[hfa2$excluded == 1 & hfa2$best != 0] <- hfa2$best[hfa2$excluded == 1 & hfa2$best != 0]  


#endloop//

#stats
hfa2$cumhfabyrank <- NA

for (i in 1:nrow(hfa2)){
  if(hfa2$best[i] > 0){ 
    hfa2$cumhfabyrank[i] <- sum(hfa2$qmw_hfa[1:i])
  }
}

(SS <- hfa2 %>% filter(best != 0) %>% 
    group_by(best) %>% 
    summarise(lon = min(Longitude), lat = min(Latitude),
              best_id = min(best), hfa_cum = min(qmw_hfa)))
View(SS)
ggplot(data = SS, aes(x = best_id, y = hfa_cum)) + geom_point() +
  geom_smooth()




nstops <- 16

(pp <- qmplot(x = Longitude, 
              y = Latitude, 
              data = hfa2,
              geom = "point",
              maptype = "toner-lite", 
              darken = c(0.7, "black"),
              main = paste("Heated Floor Area within\n 1/4 mile of Existing R-Line Stops"),
              size = I(0.25),
              color = I("blue"), 
              alpha = I(0.0),
              margins = TRUE,
              force = FALSE,
              padding = 0.02,
              extent = "panel", #, #extent, device, normal, panel
              f = 0.4) + 
    #remove axis labels and ticks
    theme(axis.ticks = element_blank(), 
          axis.text.x = element_blank(), axis.text.y = element_blank(), 
          axis.title.y = element_blank(), axis.title.x = element_blank()
    ) +
    coord_map("bonne", lat0 = 35.8, 
              xlim = c(-78.6560,-78.630),
              #ylim = c( 35.7685, 35.793)
              ylim = c(35.76655, 35.79664)
    ) +
    #geom_polygon(data = circs1, aes(x, y, alpha = I(0.5), fill = I("light blue"), group = row)) +
    geom_path(data = dtplan_xy, aes(x = X, y = Y, color = I("pink"))) +
    geom_path(data = r_path, aes(x = x, y = y, color = I("white"), size = I(1.25), alpha = I(0.2)))) +
  geom_path(data = (filter(hfa2,  best >0 & best <= nstops) %>% arrange(desc(excluded))), aes(x = Longitude, y = Latitude, size = I(0.5), alpha = I(0.5), color = I("orange"))) +
  #geom_point(data = filter(hfa2, best >0 & best <= nstops ), 
  #           aes(x = Longitude, y = Latitude, size = 1/log(excluded+1)*8)) +
  geom_point(data = filter(hfa2, excluded >0 & excluded <= nstops ), 
             aes(x = Longitude, y = Latitude, color = I(excluded),
                 alpha = I(0.5), size = I(0.5))) +
  geom_text(data = filter(hfa2, best >0 & best <= nstops ), aes(x = Longitude, y = Latitude, label = best, color = I("white"))) +
  labs(caption = (round(sum(hfa2$HEATEDAREA[hfa2$excluded <= nstops]) / sum(hfa2$HEATEDAREA)*100,1)))

# pp +  stat_density2d(data = hfa2[rep(row.names(hfa2), hfa2$qmw_hfa), 1:21],
#                      bins = 10,
#                      n = 100,
#                      aes(x = Longitude, y = Latitude,
#                          #group = as.factor(excluded),
#                          #alpha = alpha_c,
#                          alpha = I(0.9)
#                          #color = I("orange")
#                          #color = as.factor(pts_in_cir+1)
#                          #color = ..level..
#                      ))
pp + geom_point(data = hfa2, aes(x = Longitude, y = Latitude, ))
