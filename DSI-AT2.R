library(tidyverse)
library(jsonlite)
library(ggplot2)
library(ggmap)
library(gganimate)
library(gifski)
library(zoo)
library(lubridate)

getwd()

system.time(x <- fromJSON("Location History.json"))

# extracting locations of data frame
loc= x$locations
summary(loc)
head(loc)
#converting time colum from posix millisecond into readible time frame
loc$time= as.POSIXct(as.numeric(x$locations$timestampMs)/1000,origin = "1970-01-01")

#converting longitude and latitude from E7 to GPS Coordinates
loc$lat= loc$latitudeE7 / 1e7
loc$lon= loc$longitudeE7 / 1e7

head(loc)

nrow(loc)
min(loc$time)
max(loc$time)


# number of datapoint per day
library(lubridate)
library(zoo)

# How are they dssributed over days, months and years
loc$date<- as.Date(loc$time, '%Y/%m.%d' )
loc$year<- year(loc$date)
loc$month_year <- as.yearmon(loc$date)

#new dataframe with the important units
maps<- data.frame(loc$lat,loc$long,loc$date,loc$time,loc$year)
maps

#filter out the year and convert the longitude to the proper unit.
maps1<-maps%>%filter(loc.year==2018) %>% mutate(longitude = loc.long/10^7)
maps2<-maps%>%filter(loc.year==2019) %>% mutate(longitude = loc.long/10^7)

#choose the 10. measurement of each day. not very elegant, but good enough.
maps1_a<- maps1 %>% group_by(loc.date) %>% 
  summarise(long=(longitude[10]),
            lat=(loc.lat[10]))

# save api key
register_google(key = "YOUR_API_KEY")

#get background map. set size, zoom, kind of map)
mamap <- get_map(location=c(mean(maps1_a$long,na.rm=T),mean(maps1_a$lat,na.rm=TRUE)+3), source = "google", maptype = "satellite",zoom=5)

#put it all together.
ggmap(mamap)+
  geom_point(data=maps2,aes(x=long,y=lat),size=4, col="red")+
  geom_label(data=maps2,x=1.5,y=56,aes(label=format(as.Date(loc.date),format= "%d.%m") ),size=10,col="black")+
  theme_void()+
  #the animation part
  transition_time(loc.date)+
  shadow_trail(alpha=0.3,colour="#ff695e",size=2,max_frames = 6)

#############################################################################################

points_p_day <- data.frame(table(loc$date), group= "day")
points_p_month <- data.frame(table(loc$month_year), group="month")
points_p_year<- data.frame(table(loc$year),group="year")

# how many days were recorded?
nrow(points_p_day)
nrow(points_p_month)
nrow(points_p_year)

# set up plotting theme
library(ggplot2)
library(ggmap)

my_theme <- function(base_size = 12, base_family = "sans"){
  theme_grey(base_size = base_size, base_family = base_family) +
    theme(
      axis.text = element_text(size = 12),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      axis.title = element_text(size = 14),
      panel.grid.major = element_line(color = "grey"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "aliceblue"),
      strip.background = element_rect(fill = "lightgrey", color = "grey", size = 1),
      strip.text = element_text(face = "bold", size = 12, color = "navy"),
      legend.position = "right",
      legend.background = element_blank(),
      panel.margin = unit(.5, "lines"),
      panel.border = element_rect(color = "grey", fill = NA, size = 0.5)
    )
}

points<- rbind(points_p_day[,-1],points_p_month[,-1],points_p_year[,-1])

ggplot(points, aes(x=group, y=Freq))+
  geom_point(position = position_jitter(width = 0.2),alpha=0.3)+
  geom_boxplot(aes(color+group), size=1, outlier.color = NA)+
  facet_grid(group~.,scales = "free")+my_theme()+
  theme(
    legend.position = "none",
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text = element_text(angle =0,vjust = 0.5,hjust = 0.5)
  )+
  labs(
    x="",
    y="Number of data points",
    title = "how many data points did google collect about me?",
    subtitle= "number of data points per day, month and year",
    caption="\nGoogle collected between 0and 1500 data points perday(median~500), between 0 and 40,000 per month(median!15,000) and between 80,000 and 220,0000 per year(median~140,000)."
  )

# determing accuracy of the dat

accuracy <- data.frame(accuracy = loc$accuracy, group = ifelse(loc$accuracy < 800, "high", ifelse(loc$accuracy < 5000, "middle", "low")))
accuracy$group <- factor(accuracy$group, levels = c("high", "middle", "low"))

ggplot(accuracy, aes(x = accuracy, fill = group)) + 
  geom_histogram() + 
  facet_grid(group ~ ., scales="free") + 
  my_theme() +
  theme(
    legend.position = "none",
    strip.placement = "outside",
    strip.background = element_blank(),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5)
  ) +
  labs(
    x = "Accuracy in metres",
    y = "Count",
    title = "How accurate is the location data?",
    subtitle = "Histogram of accuracy of location points",
    caption = "\nMost data points are pretty accurate, 
    but there are still many data points with a high inaccuracy.
    These were probably from areas with bad satellite reception."
  )

loc3 <- with(loc, subset(loc, loc$time > as.POSIXct('2016-01-01 0:00:01')))
loc3 <- with(loc, subset(loc3, loc$time < as.POSIXct('2016-12-22 23:59:59')))


loc3$lat.p1 <- shift.vec(loc3$lat, -1)
loc3$lon.p1 <- shift.vec(loc3$lon, -1)

# Calculating distances between points (in metres) with the function pointDistance from the 'raster' package.
library(raster)
loc3$dist.to.prev <- apply(loc3, 1, FUN = function(row) {
  pointDistance(c(as.numeric(as.character(row["lat.p1"])),
                  as.numeric(as.character(row["lon.p1"]))),
                c(as.numeric(as.character(row["lat"])), as.numeric(as.character(row["lon"]))),
                lonlat = T) # Parameter 'lonlat' has to be TRUE!
})

# distance in km

dist_km <-round(sum(as.numeric(as.character(loc3$dist.to.prev)), na.rm = TRUE)*0.001, digits = 2)
dist_km

distance_p_month <- aggregate(loc3$dist.to.prev, by = list(month_year = as.factor(loc3$month_year)), FUN = sum)
distance_p_month$x <- distance_p_month$x*0.001


ggplot(distance_p_month[-1], aes(x = month_year, y = x,  fill = month_year)) + 
  geom_bar(stat = "identity")  + 
  guides(fill = FALSE) +
  my_theme() +
  labs(
    x = "",
    y = "Distance in km",
    title = "Distance traveled per month in 2016",
    caption = "This barplot shows the sum of distances 
    "
  )

#Activities
activities <- loc3$activitys

list.condition <- sapply(activities, function(x) !is.null(x[[1]]))
activities  <- activities[list.condition]

df <- do.call("rbind", activities)
main_activity <- sapply(df$activities, function(x) x[[1]][1][[1]][1])

activities_2 <- data.frame(main_activity = main_activity, 
                           time = as.POSIXct(as.numeric(df$timestampMs)/1000, origin = "1970-01-01"))

head(activities_2)
#####################333

ggplot(activities_2, aes(x = main_activity, group = main_activity, fill = main_activity)) + 
  geom_bar()  + 
  guides(fill = FALSE) +
  my_theme() +
  labs(
    x = "",
    y = "Count",
    title = "Main activities in 2016",
    caption = "Associated activity for recorded positions in 2016. 
    Because Google records activity probabilities for each position, 
    only the activity with highest likelihood were chosen for each position."
  )

