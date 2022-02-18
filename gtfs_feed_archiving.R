# GTFS Feed Fetching and Inventorying

# updated 11/11/2021 to work in tempdir()
# Updated 4/14/2021 to include transloc public api gtfs urls and accommodations
# for checking those md5sums and comparing them to the trillium live feed links

library("crayon")
library("glue")
library("dplyr")
library("readr")
library("tools") #for md5sum
library("lubridate")
library("ggplot2")

cat("\f")
rm(list=ls())
setwd("~/GoTriangle/GTFS_feeds")


# Functions----
get_newest_filename <- function(dir = tempdir()){
  require(dplyr)
  out <- file.info(list.files()) %>% 
    .[!.$isdir,] %>% 
    .[.$ctime == max(.$ctime),] %>% rownames(.)
  return(out)
}


# Vars----
gtfs_dirs <- list.dirs() %>% .[. != "."]
temp.dir <- tempdir()

#urls
car.url <- "http://data.trilliumtransit.com/gtfs/cary-transit-nc-us/cary-transit-nc-us.zip"
cht.url <- "http://data.trilliumtransit.com/gtfs/chapel-hill-transit-nc-us/chapel-hill-transit-nc-us.zip"
duk.url <- "http://data.trilliumtransit.com/gtfs/duke-nc-us/duke-nc-us.zip"
god.url <- "http://data.trilliumtransit.com/gtfs/durham-area-transit-authority-nc-us/durham-area-transit-authority-nc-us.zip"
got.url <- "http://data.trilliumtransit.com/gtfs/tta-regionalbus-nc-us/tta-regionalbus-nc-us.zip"
#oct.url <- "http://data.trilliumtransit.com/gtfs/ocpt-nc-us/ocpt-nc-us.zip"
ral.url <- "http://data.trilliumtransit.com/gtfs/capital-area-transit-nc-us/capital-area-transit-nc-us.zip"

#transloc urls
tl.car.url <- "https://api.transloc.com/gtfs/ctran.zip"
tl.cht.url <- "https://api.transloc.com/gtfs/cht.zip"
tl.duk.url <- "https://api.transloc.com/gtfs/duke.zip"
tl.god.url <- "https://api.transloc.com/gtfs/data.zip"
tl.got.url <- "https://api.transloc.com/gtfs/tt.zip"
tl.ral.url <- "https://api.transloc.com/gtfs/cat.zip"

# For Trillium live----
setwd("~/GoTriangle/temp_gtfs")
setwd(tempdir())

live.feed <- NULL

file.remove(list.files())
for(dl in c(car.url, cht.url, duk.url, god.url, got.url, ral.url)){
  download.file(dl, destfile = last(unlist(strsplit(dl, "/"))))
}

dl.zip.files <- list.files() %>%
  .[grepl(pattern = "zip$", x = .)]


live.feed <- NULL
for(z in dl.zip.files){
  cal <- read_csv(unz(z, filename = "calendar.txt"))
  
  cal$start_date <- ymd(cal$start_date)
  cal$end_date <- ymd(cal$end_date)

  md5_these_files <- c("trips.txt", "stops.txt", "stop_times.txt", 
                       "calendar.txt", "routes.txt", "shapes.txt")
  md5.df <- NULL
  for(fn in md5_these_files){
    print(fn)
    temp <- read_csv(unz(z, filename = fn))
    temp.filename <- get_newest_filename()
    temp.md5sum <- md5sum(temp.filename) %>% unname()
    
    md5.df <- rbind(md5.df, 
                    data.frame(file = fn, 
                               md5sum_val = temp.md5sum))
    rm(temp, temp.filename, temp.md5sum)
  }
  
  md5.df <- rbind(md5.df, 
                  data.frame(file = z, 
                             md5sum_val = unname(md5sum(z))))
  
  live.feed <- rbind(live.feed,
                     data.frame(zip_filename = z,
                                #url = k,
                                source = "Trillium",
                                #feed_version = read_csv("feed_info.txt")$feed_version,
                                feed_version = read_csv(unz(z, filename = "feed_info.txt"))$feed_version,  
                                start_date = min(cal$start_date),
                                end_date = max(cal$end_date),
                                live.ctime = file.info(z)$ctime,
                                md5sum_trips = md5.df$md5sum_val[md5.df$file == "trips.txt"], #md5sum((unz(z, filename = "trips.txt", open = ""))),
                                md5sum_stops = md5.df$md5sum_val[md5.df$file == "stops.txt"], #md5sum("stops.txt"),
                                md5sum_stoptimes = md5.df$md5sum_val[md5.df$file == "stop_times.txt"], #md5sum("stop_times.txt"),
                                md5sum_calendar = md5.df$md5sum_val[md5.df$file == "calendar.txt"], #md5sum("calendar.txt"),
                                md5sum_routes = md5.df$md5sum_val[md5.df$file == "routes.txt"], #md5sum("routes.txt"),
                                md5sum_shapes = md5.df$md5sum_val[md5.df$file == "shapes.txt"], #md5sum("shapes.txt"),
                                md5sum = md5.df$md5sum_val[md5.df$file == z] #md5sum("google_transit.zip")
                                )) %>% as_tibble()
  Sys.sleep(1)
}

live.feed <- live.feed[order(live.feed$zip_filename),]

live.feed$agency <- c("gor", "goc", "cht", "duk", "god", "got")
live.feed$url <- c(ral.url, car.url, cht.url, duk.url, god.url, got.url)
live.feed <- live.feed %>% as_tibble()
setwd("~/GoTriangle/GTFS_feeds")


# For TransLoc live----
setwd("~/GoTriangle/temp_gtfs")
setwd(tempdir())
live.feed_tl <- NULL


for(k in c(tl.car.url, tl.cht.url, tl.duk.url, tl.god.url, tl.got.url, tl.ral.url)){
  print(k)
  file.remove(list.files())
  download.file(k, destfile = "google_transit_tl.zip")
  
  z <- "google_transit_tl.zip"
  
  # new vvv ----
  md5_these_files <- c("trips.txt", "stops.txt", "stop_times.txt", 
                       "calendar.txt", "routes.txt", "shapes.txt")
  md5.df <- NULL
  for(fn in md5_these_files){
    print(fn)
    temp <- read_csv(unz(z, filename = fn))
    temp.filename <- get_newest_filename()
    temp.md5sum <- md5sum(temp.filename) %>% unname()
    
    md5.df <- rbind(md5.df, 
                    data.frame(file = fn, 
                               md5sum_val = temp.md5sum))
    rm(temp, temp.filename, temp.md5sum)
  }
  
  md5.df <- rbind(md5.df, 
                  data.frame(file = z, 
                             md5sum_val = unname(md5sum(z))))
  
  #get feed start_ and end_dates
  
  cal <- read_csv(unz(z, filename = "calendar.txt"))
  cal$start_date <- ymd(cal$start_date)
  cal$end_date <- ymd(cal$end_date)
  
  live.feed_tl <- rbind(live.feed_tl,
                        data.frame(zip_filename = z,
                                   #url = k,
                                   source = "TransLoc",
                                   feed_version = read_csv(unz(z, filename = "feed_info.txt"))$feed_version,
                                   start_date = min(cal$start_date),
                                   end_date = max(cal$end_date),
                                   live.ctime = file.info(z)$ctime,
                                   md5sum_trips = md5.df$md5sum_val[md5.df$file == "trips.txt"], #md5sum((unz(z, filename = "trips.txt", open = ""))),
                                   md5sum_stops = md5.df$md5sum_val[md5.df$file == "stops.txt"], #md5sum("stops.txt"),
                                   md5sum_stoptimes = md5.df$md5sum_val[md5.df$file == "stop_times.txt"], #md5sum("stop_times.txt"),
                                   md5sum_calendar = md5.df$md5sum_val[md5.df$file == "calendar.txt"], #md5sum("calendar.txt"),
                                   md5sum_routes = md5.df$md5sum_val[md5.df$file == "routes.txt"], #md5sum("routes.txt"),
                                   md5sum_shapes = md5.df$md5sum_val[md5.df$file == "shapes.txt"], #md5sum("shapes.txt"),
                                   md5sum = md5.df$md5sum_val[md5.df$file == z] #md5sum("google_transit.zip")
                        )) %>% as_tibble()
  Sys.sleep(1)
}

live.feed_tl$agency <- c("goc", "cht", "duk", "god", "got", "cat")
live.feed_tl$url   <- c(tl.car.url, tl.cht.url, tl.duk.url,
                        tl.god.url, tl.got.url, tl.ral.url)
live.feed_tl <- live.feed_tl %>% as_tibble()
setwd("~/GoTriangle/GTFS_feeds")



transloc_vs_trillium <- rbind(live.feed,
      live.feed_tl) %>%
  group_by(agency, feed_version) %>%
  summarise(n_source = n_distinct(source))

transloc_vs_trillium %>%
  .[!.$n_source > 1,] %>%
  .$agency %>%
  unique() %>%
  paste(., collapse = ", ") %>%
  paste("These feeds need to be deployed to TransLoc: ",
        .,
        collapse = "", sep = "") %>%
  yellow() %>% bgRed() %>%
  bold() %>%
  inverse() %>%
  cat()

Sys.sleep(15)

# for each gtfs directory
disk.df <- NULL
for(i in gtfs_dirs){
  setwd(i)
  normalizePath(getwd())

  #find the .zip file
  for(j in list.files()[grepl(pattern = ".*zip$", x = list.files())]){
    #print(j)
    file.rename(from = j, to = "google_transit.zip")
    if(!file.exists("calendar.txt")){
      #download.file("google_transit.zip")
      unzip("google_transit.zip")
    }
    #get cal date ranges
    cal <- read_csv("calendar.txt")
    cal$start_date <- ymd(cal$start_date)
    cal$end_date <- ymd(cal$end_date)

    disk.df <- rbind(disk.df,
                     data.frame(dir = i,
                                file = j,
                                start_date = min(cal$start_date),
                                end_date = max(cal$end_date),
                                disk.ctime = file.info("google_transit.zip")$ctime,
                                md5sum = trimws(as.character(tools::md5sum("google_transit.zip"))),
                                stringsAsFactors = F))
    file.rename(to = j, from = "google_transit.zip")
  }
  setwd("~/GoTriangle/GTFS_feeds")
}

setwd("~/GoTriangle/GTFS_feeds")



# Tidy----

disk.df <- disk.df %>% as_tibble()


disk.df$agency <- disk.df$dir %>%
  gsub(pattern = "[[:punct:]]", replacement = "", x = .) %>%
  gsub("\\d", "", .)
disk.df$agency[disk.df$agency == "duke"] <- "duk"
disk.df$agency[disk.df$agency == "gota"] <- "got"
disk.df$agency[disk.df$agency == "ocpta"] <- "ocpt"

disk.df

ggplot() +
  geom_segment(data = slice_max(group_by(disk.df[!disk.df$agency %in% c("wfl", "ocpt"),], agency),
                                order_by = disk.ctime, n = 2),
             aes(x = start_date, xend = end_date,
                 y = dir, yend = dir, color = agency), size = 2) +
  geom_vline(data = expand.grid(agency = unique(disk.df[!disk.df$agency %in% c("wfl", "ocpt"),]$agency),
                                dt = Sys.Date()),
             aes(xintercept = dt))+
  facet_grid(agency~., scales = "free", space = "free") +
  theme(strip.text.y = element_text(angle = 0),
        legend.position = "bottom")+
  labs(title = "File backup status",
       subtitle = "newest files on disk by agency")

Sys.sleep(10)

# Analysis----
live.feed$live <- T
live.feed
disk.df$in.dir <- T
disk.df

# Plot----
disk.df$dup.md5 <- duplicated(disk.df$md5sum)

disk.df %>%
  group_by(agency,md5sum,start_date,end_date,
           now = Sys.Date() >= start_date & Sys.Date() <= end_date) %>%
  summarise(dup = sum(dup.md5),
            n_dirs = n_distinct(dir)) %>%
  .[order(.$dup, .$now, .$md5sum, decreasing = T),] %>%
  group_by(agency, n_dirs) %>%
  slice_max(., order_by = end_date, n = 1) %>%
  ggplot(data = .,
         aes(xend = 0, x = n_dirs, y = md5sum, yend = md5sum,
             color = factor(dup))) +
  geom_point() +
  geom_segment() +
  facet_grid(agency~., scales = "free", space = "free") +
  theme(strip.text.y = element_text(angle = 0)) +
  labs(title = "md5sum hashes for most recent file save date per agency",
       subtitle = "including duplicity")

Sys.sleep(5)

fj <- full_join(live.feed, disk.df, by = c("agency", "md5sum", "start_date", "end_date"))
fj$live[is.na(fj$live)] <- F
fj$in.dir[is.na(fj$in.dir)] <- F

fj$age_days <- as.numeric(as.duration((as_datetime(fj$disk.ctime) %--% Sys.Date())),
                          "days")

not.on.disk <- fj %>%
  group_by(live, dir, agency, in.dir, md5sum, live.ctime, age_days) %>%
  summarise(n_files = n_distinct(md5sum)) %>%
  .[.$in.dir == F| .$live == T,] %>%
  .[colnames(.) %in% c("dir", "agency", "in.dir", "live", "md5sum", "age_days")]



# Download live but not on disk-----
dl.agencies <- not.on.disk$agency[not.on.disk$in.dir == F]

for(i in dl.agencies){
  setwd("~/GoTriangle/GTFS_feeds")
  #create new directory
  temp.month <- month(Sys.Date())
  if(nchar(temp.month)<2){
    temp.month <- glue("0{temp.month}")
  }
  temp.day <- mday(Sys.Date())
  if(nchar(temp.day)<2){
    temp.day <- glue("0{temp.day}")
  }
  temp.dir <- glue("./{i}_{year(Sys.Date())}{temp.month}{temp.day}")
  temp.dir %>% bold() %>% bgYellow() %>% black %>% cat()
  # first check to see if it already exists
  if(!dir.exists(temp.dir)){
    dir.create(temp.dir)
    glue(" DIR CREATED ") %>% bgCyan() %>% cat()
  }else{
    glue(" DIR exists already ") %>% inverse() %>% cat()
  }
  # set wd
  setwd(temp.dir)
  # download zip file
  file.remove(list.files()[grepl(pattern = ".zip$", x = list.files())])
  #list.files()[grepl(pattern = ".zip$", x = list.files())]
  download.file(live.feed$url[live.feed$agency %in% i], 
                destfile = glue("{toupper(i)}_gtfs_{year(Sys.Date())}{temp.month}{temp.day}.zip"))
}

# Set Reminder to Upload to transloc & steer----
reminder.df <- full_join(disk.df, live.feed, by = c("agency", "md5sum"))
reminder.df$live[is.na(reminder.df$live)] <- F
reminder.df$in.dir[is.na(reminder.df$in.dir)] <- F

reminder.df <- reminder.df %>%
  .[.$live == T,] %>%
  group_by(agency, md5sum, today = as_date(disk.ctime) == Sys.Date()) %>%
  summarise(n = n()) %>%
  .[.$today == T,]

cat("\f")
glue("Upload {not.on.disk$agency[not.on.disk$in.dir == F]} to transloc and steer\n\n") %>% 
  bgRed() %>% cat()
glue("Upload {reminder.df$agency} to transloc and steer\n\n") %>% 
  bgRed() %>% cat()


# temp----
disk.df
fj
live.feed
not.on.disk # live feeds not on disk
reminder.df

today.df <- data.frame(live = c(T,F), 
                       date = Sys.Date(), 
                       stringsAsFactors = F)

fj_now <- fj[fj$start_date <= Sys.Date() & 
               fj$end_date >= Sys.Date(),]
fj_now$md5sum_f <- factor(fj_now$md5sum, 
                          levels = unique(fj_now$md5sum[order(fj_now$live.ctime)]))
ggplot() + 
  geom_segment(data = slice_max(group_by(fj_now, agency, live), 
                                order_by = end_date, n = 3), 
               size = 2,
             aes(x = start_date, xend = end_date, 
                 y = md5sum_f, yend = md5sum_f, color = live)) +
  geom_vline(data = today.df, 
             aes(xintercept = date), 
             color = "black", linetype = 2232) +
  geom_point(data = slice_max(group_by(fj_now, agency, live), 
                              order_by = end_date, n = 3), 
             shape = 21, size = 3,
             aes(x = as_date(disk.ctime), y = md5sum_f)) + 
  theme(legend.position = "bottom", 
        strip.text.y = element_text(angle = 0)) +
  facet_grid(agency~., scales = "free", space = "free")+
  labs(title = "Current feeds and md5sums", 
       subtitle = "Calendars and age of feed")

names(fj)
fj %>%
  group_by(agency,md5sum) %>%
  summarise(n_dirs = n_distinct(dir),
            n_live = n_distinct(live), 
            live = all(live)) %>%
  .[order(.$live, decreasing = T),]


# plot newest directory for each agency----

ggplot(data = slice_max(group_by(disk.df[disk.df$agency != "wfl",], agency), 
                        order_by = disk.ctime, n = 1))+
  geom_point(aes(x = as_date(disk.ctime), y = dir, 
                 color = "File Write Date"), 
             size = 4) +
  geom_segment(aes(x = start_date, xend = end_date, 
                   y = dir, yend = dir, 
                   color = agency))+
  geom_vline(aes(xintercept = Sys.Date(), 
                 color = "Today"), 
             linetype = 2232)+
  theme(legend.position = "bottom")
