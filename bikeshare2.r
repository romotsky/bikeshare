

##<span style="color:red">  To view the full analysis with animation, please refer to the [HTML version](https://romotsky.github.io/bikeshare/)</span>
***

# install tinytex::install_tinytex()
## install necessary packages ---
# install.packages(
#   "ggmap"
# , "tinytex"
# , "knitr"
# , "kableExtra" # for formatting
# , "tidyverse"
# , "caret"
# , "geosphere"
# , "scales"
# , "gganimate"
# , "lubridate"
# , "GSODR"
# , "rvest"
# , "magrittr")
library(tinytex)
library(knitr)
library(kableExtra) # for formatting
library(tidyverse)
library(caret)
library(gganimate)
# library(data.table)



library(ggmap)
register_google(key = "INSERT YOUR KEY") ## input your google api key


# Introduction
# As an LA native, I know that it is a driver's city. Traffic is always expected and road rage is narrowly avoided daily.  Up until the 2010's, you had to own a car to get anywhere. However, tech and the share economy has started to change that assumption, with Uber and Lyft, more accurate bus times, delivery services, and flexible work making it somewhat easier to get to where you need to be.

# Of all the new advancements, the greenest opportunity for transportation is city bike share programs, which seem to now be available in most metropolitan cities. When I moved back to Los Angeles, I was quite surprised how many bikers there were on the road.  I'm an avid biker myself and wanted to learn more about the bike share program in the city.  With minimal research, I found that the LA Metro dedicated a portion of it's website to [Bike Share](https://bikeshare.metro.net), including historical rider [data](https://bikeshare.metro.net/about/data/) for public consumption.

# The aim of this project is to leverage this data to analyze bike share rider behavior, find patterns, and create a model that will predict the rider volume across the city.

### Outline
# * Get the Data
# * Exploratory Analysis
#   * Pre-processing/data-cleansing
#   * Pull in other factors/variables
# * Predictive Model
# * Model Results
# * Conclusion

# *Note that much of the code has been suppressed for ease of reading.  Please refer to the r code for more-detailed explanations.* 

# *Also, in order to run the full R code, you will need to input your own Google Maps API key.  More info on how to do this can be found [here](https://cloud.google.com/maps-platform/).*

# Get the Data
# There are various csv files provided at <https://bikeshare.metro.net/about/data/>

# The files types are

# - Trip Data

# - Station Lookup

# To get the data, I used the **rvest** and **magrittr** packages & followed these steps

# - Read the html code on BikeShare's About page: *https://bikeshare.metro.net/about/data/*

# - From inspecting the page, I know that the data file names live in the HTML class titled *a*.  I save the respective nodes.

# - Using the **stringr** package, I then strip out the names I want.

# save the url where the files live
url <- "https://bikeshare.metro.net/about/data/"

library(rvest)
library(magrittr)

readit <- read_html(url)
# vignette("selectorgadget")
nodes <- html_nodes(readit, "a") #using rvest

# Sample of the data nodes on the page:

# html_text(nodes) %>% head()
node_index <- str_detect(html_text(nodes), "20[12][0-9]") #grab proper names
station_index <- str_detect(html_text(nodes), "Station Table")
html_text(nodes)[node_index] %>% head()

# And the sample of file names:

html_attr(nodes, "href")[node_index] %>% head() # here are the zipped files!
csv_list <- html_attr(nodes, "href")[node_index]

# - I then download the zipped files to my local drive and unzip them.
# - The names in each file don't exactly match up, so I created a short function to line everything up.


# set working directory
setwd("/Users/danielromotsky/Documents/bike_files")
outDir<-getwd()

#download unzipped file into working directory
sapply(csv_list, FUN = function(x) {
  download.file(x, "temp")
  unzip("temp", exdir=outDir)
} )

files <- list.files()
files
## not the best naming convention, but that's ok.  The data within is hopefully the same! and with dates

d_index <- str_detect(list.files(), ".csv") & str_detect(tolower(list.files()), "metro")

head(read.csv(files[d_index][1])) %>% kable() %>%
  kable_styling(full_width = F)

df <- sapply(files[d_index], FUN = function(x){ 
               read.csv(x, stringsAsFactors = FALSE)})

length(df) # 13 data frames

sapply(df, FUN = names) # almost the same set of names

df_final <- data.frame()
for (i in 1:length(df)){
  df_temp <- df[[i]][c(1:14)]
  names(df_temp) <- c("trip_id", "duration", "start_time", "end_time", "start_station_id",   
                      "start_lat",  "start_lon", "end_station_id", "end_lat",
                      "end_lon", "bike_id", "plan_duration", "trip_route_category", "passholder_type")
  df_final <- rbind(df_final, df_temp)
}

# - Here's the final bike riding data set from the site:

# head(df_final)
glimpse(df_final)



# - Now let's add some more vars for easier analysis, starting with the dates and times.  I've created new variables for *YYYY-MM-DD* and *time* using the **Lubridate package**.

library(lubridate)
bike_data <- df_final %>% mutate(
  start_time = parse_date_time(start_time, orders = c("Ymd HMS", "mdY HM")),
  end_time = parse_date_time(end_time, orders = c("Ymd HMS", "mdY HM")),
                                 start_date = ymd(as_date(start_time)),
                                 end_date = ymd(as_date(end_time)))


library(lubridate)
date <- tail(df_final$start_time)
# ymd_hm(date)
# parse_date_time(date, "%d/%m/%y H:M")

## different formats of dates in the files!!
parse_date_time(c("1/1/2017 0:15", "2019-06-05 19:56:00"), 
                orders = c("Ymd HMS", "mdY HM"))

ymd(parse_date_time(date, "%d/%m/%y H:M"))
bike_data <- df_final %>% mutate(
  start_time = parse_date_time(start_time, orders = c("Ymd HMS", "mdY HM")),
  end_time = parse_date_time(end_time, orders = c("Ymd HMS", "mdY HM")),
                                 start_date = ymd(as_date(start_time)),
                                 end_date = ymd(as_date(end_time))) #,"-",month(start_time), sep="") ) 

# - Now let's pull in the station lookup table. 
# - We bring that into the data set, joined by start and end station.  Then make some updates to variable naming. 

station_index <- str_detect(html_text(nodes), "Station Table")
html_text(nodes)[station_index]
html_attr(nodes, "href")[station_index]
station_url <- html_attr(nodes, "href")[station_index]
station_url
download.file(station_url, "station.csv") 
station_lookup<- read.csv("station.csv", stringsAsFactors = FALSE)

# add that into the data table
# new_names <- c("start_station", "start_region","end_station", "end_region")
bike_data2 <- bike_data %>% 
  left_join(station_lookup, by = c("start_station_id"= "Station_ID")) %>%
  select(-Status, -Go_live_date) %>% 
  left_join(station_lookup, by = c("end_station_id"= "Station_ID")) %>%
  select(-Status, -Go_live_date)

names(bike_data2 %>% select(tail(names(.),4))) 
names(bike_data2) <- c(names(bike_data2[1:16]), "start_station", "start_region","end_station", "end_region")
names(bike_data2 %>% select(tail(names(.),4))) 


bike_data2 %>% group_by(start_station, end_station) %>% tally() %>% arrange(desc(n)) %>% head() %>% kable()



# - **Data cleansing #1**: According to the website where this data was pulled, "Virtual Station" is used by staff to check in or check out a bike remotely for a special event or in a situation in which a bike could not otherwise be checked in or out to a station.  I also needed to ensure each station has uniform sets of latitude and longitudinal coordinates.  I found that one station changed coordinates slightly in 2018, which threw off my analysis, so I went in an aligned it. 


## let's remove this virtual station
bike_data2 %>% filter(start_station == "Virtual Station" | end_station == "Virtual Station") %>% head() %>% kable()

na_index <- which(is.na(bike_data2$start_lat) | is.na(bike_data2$end_lat) | is.na(bike_data2$start_station))

bike_data2[-na_index,] %>% head()

bike_data2 <- bike_data2[-na_index,]

which(is.na(bike_data2$start_lat))

which(is.na(bike_data2$end_lat))

bike_data2$start_lat[bike_data2$start_station == "California & Lincoln"] <- 33.99834
bike_data2$start_lon[bike_data2$start_station == "California & Lincoln"] <- -118.461


# Exploratory Analysis

### Time-based
# - We see a clear increase in rides over time

# library(dplyr)
#daily trend
bike_data2 %>% group_by(start_date) %>% dplyr::summarise(n=n()) %>% arrange(start_date) %>%
  ggplot(aes(start_date, n)) + geom_point() + 
  geom_smooth() + 
  scale_y_continuous(limits=c(0,1000)) + ggtitle("Bike Share Rides over time")

# - It's not surprising to see that rides are highest during the summer:


#monthly
m <- bike_data2 %>% mutate(month = month(start_date, label = TRUE, abbr = TRUE), year = year(start_date)) %>%     
  # group_by(trip_route_category) %>% #tally() %>% 
  ggplot(aes(month)) + geom_histogram(stat="count") + ggtitle("Avg Rides by Month") +
  theme(axis.text.x = element_text(angle = 0, vjust = 0), axis.ticks.x = element_blank(), axis.title.x = element_blank()) 

#yearly
y <- bike_data2 %>% mutate(month = month(start_date, label = TRUE, abbr = TRUE), year = year(start_date)) %>%     
  # group_by(trip_route_category) %>% #tally() %>% 
  ggplot(aes(year)) + geom_histogram(stat="count") + ggtitle("Avg Rides by year") +
  theme(axis.text.x = element_text(angle = 0, vjust = 0), axis.ticks.x = element_blank(), axis.title.x = element_blank()) 

library(gridExtra)
grid.arrange(y, m, nrow = 1)


### Station performance
# Each station rolls into a larger region around the city of Los Angeles:


## let's start looking at the bike stations
start <- bike_data2 %>% group_by(start_station_id, start_station, start_region, start_lat, start_lon) %>% dplyr::summarise(num_rides=n()) %>% arrange(desc(num_rides))
## what's the dist by region?
bike_data2 %>% mutate(month = month(start_date, label = TRUE, abbr = TRUE)) %>% #group_by(trip_route_category) %>% #tally() %>% 
  ggplot(aes(month, fill = start_region)) + geom_histogram(stat="count") + #facet_wrap(~start_region) + ggtitle("Starting Rides by Month") +
  theme(axis.text.x = element_text(angle = 0, vjust = 0, size = 8), axis.ticks.x = element_blank(), axis.title.x = element_blank()) #+ theme(legend.position = "none")

# # library(waffle)
# ## what's the dist by region?
# bike_data2 %>% mutate(month = month(start_date, label = TRUE, abbr = TRUE)) %>% 
#   group_by(start_region, month) %>% tally() %>% 
#   ggplot(aes(month, month, fill=start_region)) + geom_tile()
#   
  # geom_raster() + facet_wrap(~start_region) + ggtitle("Starting Rides by Month") +
  # theme(axis.text.x = element_text(angle = 0, vjust = 0, size = 8), axis.ticks.x = element_blank(), axis.title.x = element_blank()) + theme(legend.position = "none")


# Obviously the most rides begin in Downtown LA.  We'll use free scales and view over months so that we can analyze the differences in distribution by region


## what's the dist by region?
bike_data2 %>% mutate(month = month(start_date, label = TRUE, abbr = TRUE)) %>% #group_by(trip_route_category) %>% #tally() %>% 
  ggplot(aes(month, color = start_region)) + geom_histogram(stat="count") + facet_wrap(~start_region, scales = "free_y") + ggtitle("Starting Rides by Month (free scales)") +
  theme(axis.text.x = element_text(angle = 0, vjust = 0, size = 8), axis.ticks.x = element_blank(), axis.title.x = element_blank()) + theme(legend.position = "none")

# The regions all follow similar distributions - increases in the summer, however there are some regions that have unknown regions or very low ride counts. Have bikes only been available for short periods of time in different regions?  This can be viewed in two ways.
# 
# **Time-series view of each region**


bike_data2 %>% group_by(start_date, start_region) %>% tally() %>% arrange(start_date) %>%
  ggplot(aes(start_date, n, color = start_region)) + geom_point() + 
  geom_smooth() + 
  scale_y_continuous(limits=c(0,1000)) + ggtitle("Bike Share Rides over time by Starting Region") + annotate("text", size=8, color = "red", x = as_date('2018-01-01'), y = 970, label = "Bikes have always been available in DTLA")


# **Heatmap view of each region**
# It's a bit more clear of when each station was available with a heatmap:


## better view of when each region has been in business
heat <- bike_data2 %>% mutate(month = month(start_date, label = TRUE, abbr = TRUE), year = year(start_date), y_m = format(start_date, "%Y-%m")) %>% group_by(start_region, y_m) %>% tally()

## tile view
heat %>% ggplot(aes(y_m, start_region)) + geom_tile(aes(fill=n)) +
  scale_fill_gradient(low = "grey", high = "green") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8))


# More **data cleansing #2** ! Because North Hollywood region is so new and Pasadena ceased operations, I will remove both for the predictive model. NAs and Free Bikes will be omitted from further analysis as well.


# bike_data2$start_lat[bike_data2$start_station == "California & Lincoln"] <- 33.99832 ## fix a dedupe issue found later

bike_data2 <- bike_data2 %>% filter(start_region != "North Hollywood") %>%
  filter(start_region != "Pasadena") %>% filter(end_region != "North Hollywood") %>%
  filter(end_region != "Pasadena")



### Spatial/Mapping Data & Analysis
# Let's look at some spatial data to help visualize movement of these bike rides. I used the **ggmap** package, which integrates with a Google Maps API key.  I can then plot point directly on a map, using the latitude and longitude coordinates from the data set. 

# *Note* that you will need to input your own key in order to run this mapping script.


# install.packages("ggmap")
library(ggmap)
losangeles <- "los angeles"
map <- get_map(losangeles, zoom=10)

## starting stations
# head(start,10) %>% kable(digits=2) %>% kable_styling(full_width = F)
start <- bike_data2 %>% group_by(start_station_id, start_station, start_region, start_lat, start_lon) %>% dplyr::summarise(num_rides=n()) %>% arrange(desc(num_rides))

#Full LA city
map %>% ggmap() + geom_point(data = start, aes(start_lon, start_lat, size = num_rides, fill=start_region), shape = 21) + ggtitle("Starting Stations")


# Main areas of operation are in Downtown LA, Santa Monica, and Long Beach.


sm <- get_map(location = c(lon=-118.43, lat=34), zoom=12) %>% ggmap() 

sm2 <- sm + geom_point(data = filter(start, start_region=="Westside"), aes(start_lon, start_lat, size = num_rides), fill="blue", shape=21) + ggtitle("Santa Monica")

lb <- get_map(location = c(lon = -118.28, lat = 33.75), zoom=13) %>% ggmap()

lb2 <- lb + geom_point(data = filter(start, start_region=="Port of LA"), aes(start_lon, start_lat, size = num_rides), fill=" dark green", shape=21) + ggtitle("Long Beach")

dtla <- get_map("downtown los angeles", zoom=12) %>% ggmap() 
dtla2 <- dtla + geom_point(data = filter(start, start_region=="DTLA"), aes(start_lon, start_lat, size = num_rides), fill="red", shape=21) + ggtitle("DTLA")

# grid.arrange(sm2, lb2, dtla2, nrow=1)
sm2
lb2
dtla2


# The data set provides a start station and an end station, which allows me to look at general routes people take on their shared bikes.  As mentioned previously, the three main regions are Downtown, Santa Monica, and Long Beach... and for those familiar with LA will know that they are nowhere near each other!
# \n
# Using the **geosphere** package...


avg_coord <- bike_data2 %>% group_by(start_region) %>% summarise(avg_lat = mean(start_lat), avg_lon = mean(start_lon))

library(geosphere)

DTLA_LB <- distm(c(avg_coord$avg_lon[avg_coord$start_region=="DTLA"], avg_coord$avg_lat[avg_coord$start_region=="DTLA"]), c(avg_coord$avg_lon[avg_coord$start_region=="Port of LA"], avg_coord$avg_lat[avg_coord$start_region=="Port of LA"]), fun = distHaversine)

cat("Distance from DTLA and Long Beach is about", DTLA_LB, "meters or", DTLA_LB/1609.344, "miles!\n")

DTLA_SM <- distm(c(avg_coord$avg_lon[avg_coord$start_region=="DTLA"], avg_coord$avg_lat[avg_coord$start_region=="DTLA"]), c(avg_coord$avg_lon[avg_coord$start_region=="Westside"], avg_coord$avg_lat[avg_coord$start_region=="Westside"]), fun = distHaversine)

cat("Distance between DTLA and Santa Monica is about", DTLA_SM, "meters or", DTLA_SM/1609.344, "miles!\n")

LB_SM <- distm(c(avg_coord$avg_lon[avg_coord$start_region=="Port of LA"], avg_coord$avg_lat[avg_coord$start_region=="Port of LA"]), c(avg_coord$avg_lon[avg_coord$start_region=="Westside"], avg_coord$avg_lat[avg_coord$start_region=="Westside"]), fun = distHaversine)

cat("Distance between Long Beach and Santa Monica is about", LB_SM, "meters or", LB_SM/1609.344, "miles!\n")



# Surely, noone is crazy enough to bike between regions... right? Wrong!


end <- bike_data2 %>% group_by(end_station_id, end_station, end_region, end_lat, end_lon) %>% dplyr::summarise(num_rides=n()) %>% arrange(desc(num_rides))

diff_region <- bike_data2 %>% filter(start_region != end_region)
## 500 rides!

library(scales)
cat("Apparently, there are", nrow(diff_region),"rides that went from one region to the other. The percentage of rides out of the total is", percent(nrow(diff_region) / nrow(bike_data2)),"\n Where are they starting and ending?") ## proportion of rides that end in a different region

#Where are they going?
map2 <- get_map(location = c(lon = -118.33, lat = 33.9), zoom = 11, 
                maptype = "toner",  
                source = "stamen")

map2 %>% ggmap() + geom_point(data = diff_region, aes(start_lon, start_lat), fill="green", shape=21, alpha=0.2) + geom_point(data = diff_region, aes(end_lon, end_lat), fill="red", shape=21, alpha=.2) + ggtitle("Start and End for Rides between Regions")




# It looks like most of them originated in DTLA.  However, this static visualization is rather tough to understand.
# \n
# *Side note* about myself: I am really into animation and video production. A true novice in these fields, I've made a few animated shorts and parody videos for friends and families.  I also love being analytical and got into data visualization and analytics because it's a great blend of both sides of the brain.  However, I didn't think there was opportunity to infuse movement of data in a professional field until I learned about the... 
# **gganimate** package - this is developed to work in tandem with ggplot functions to create frames of graphs which shows change over time.  I will use this extensively for any animated graphics you see moving forward.
# So... where are these bikers heading?


#animate it

s <- diff_region %>% select(trip_id, start_lat, start_lon) %>% mutate(leg = "start")
e <- diff_region %>% select(trip_id, end_lat, end_lon) %>% mutate(leg = "end")
names(s) <- c("trip_id", "lat", "lon", "leg")
names(e) <- c("trip_id", "lat", "lon", "leg")
diff_anim <- rbind(s,e) %>% arrange(trip_id)
diff_anim$leg <- factor(diff_anim$leg, levels = c("start", "end"))

## try gganimate again
# install.packages('devtools')
# devtools::install_github('thomasp85/gganimate')
# library(gganimate)

# m + geom_point(data = diff_anim, aes(lon, lat, frame = leg)) +
#   gganimate() + ggtitle("Start and End for Rides between Regions")

# install.packages("gganimate")
# library(purrr) 
library(gganimate)

m <- map2 %>% ggmap()
m + 
  geom_point(data = (diff_anim %>% mutate(r = ifelse(row_number(trip_id)%%2==0, 
                                row_number(trip_id)-1,
                                row_number(trip_id)))), 
             aes(y=lat, x=lon, fill = scale(r)), shape=21, size=5, alpha=0.5, show.legend = FALSE) + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text = element_blank()) + scale_fill_gradient2(low="purple", mid = "turquoise", high="gold") +
transition_states(leg, wrap=FALSE) + ease_aes('cubic-in-out') + enter_fade() +
  ggtitle("Where are these crazy bikers going?\n{closest_state}") 



# The majority of these over-achievers are heading to or from downtown - likely for work. Very few of the long rides are along the coast (SM to LB) since the [Marvin Braude Bike Trail](https://upload.wikimedia.org/wikipedia/commons/5/55/Lasouthbaybiketrail.jpg) ends at Torrance - well north of Long Beach.

# Let's look a little closer at the rides within each region.


## set up data for animation
s2 <- bike_data2 %>% group_by(start_lat, start_lon, start_region) %>% dplyr::summarise(n=n()) %>% mutate(leg = "start") %>% ungroup()
e2 <- bike_data2 %>% group_by(end_lat, end_lon, end_region) %>% dplyr::summarise(n=n()) %>% mutate(leg = "end") %>% ungroup()
names(s2) <- c("lat", "lon", "region", "count", "leg")
names(e2) <- c("lat", "lon", "region", "count", "leg")
stacked <- rbind(s2,e2) %>% arrange(count)
stacked$leg <- factor(stacked$leg, levels=c("start", "end"))


sm +  
  geom_point(data = (stacked %>% filter(region == "Westside")), aes(y=lat, x=lon, size = count), fill = "yellow", shape=21, show.legend = FALSE) +
  transition_states(leg, wrap=FALSE) + 
  ggtitle("Santa Monica routes: {closest_state}") 


dtla +  
  geom_point(data = (stacked %>% filter(region == "DTLA")), aes(y=lat, x=lon, size = count), fill="blue", shape=21, show.legend = FALSE) +
  transition_states(leg, wrap=FALSE) + 
  ggtitle("DTLA routes: {closest_state}") 


lb +  
  geom_point(data = (stacked %>% filter(region == "Port of LA")), aes(y=lat, x=lon, size = count), fill="green", shape=21, show.legend = FALSE) +
  transition_states(leg, wrap=FALSE) + 
  ggtitle("Long Beach Routes: {closest_state}") 


# Are there any hourly trends that we can see? Again, I will use the **lubridate** package to parse the hour, and **gganimate** to visualize the trends, if any.


# library(lubridate)

##using tweener for transition
# library(tweenr)

# m +
#   geom_point(data = (bike_data2 %>% 
#                        mutate(hour = hour(start_time), start_lat = round(start_lat,4), start_lon = round(start_lon,4)) %>% 
#                 group_by(hour, start_lat, start_lon, start_station) %>% 
#                 tally()), 
#              aes(y=start_lat, x=start_lon, size = n, fill = hour), 
#              shape=21, alpha=.5, show.legend = TRUE) + labs(title = "Hour of day: {frame_time}") +
#   scale_fill_continuous(low="green", high="orange") + transition_time(hour) +
#   # transition_states(states = hour) +
#   # ggtitle("Hour of day: {closest_state}") +
#   # ease_aes('cubic-in-out') + 
#   enter_fade() + exit_fade()

m +
  geom_point(data = (bike_data2 %>% 
                       mutate(hour = hour(start_time), start_lat = round(start_lat,4), start_lon = round(start_lon,4)) %>% 
                group_by(hour, start_lat, start_lon, start_station) %>% 
                tally()), 
             aes(y=start_lat, x=start_lon, size = n, fill = hour, group=hour), 
             shape=21, alpha=.85, show.legend = TRUE) + 
  # labs(title = "Hour of day: {frame_time}") +
  scale_fill_continuous(low="green", high="orange") + 
  transition_states(hour) +
  # transition_time(hour) +
  # transition_states(states = hour) +
  ggtitle("Hour of day: {closest_state}") +
  ease_aes('cubic-in-out') + 
  enter_fade() + exit_fade()


# A heatmap of hours by station doesn't really show any large trends.


## heatmap of the hours
## order by region, filter out smaller stations
bike_data2 %>% mutate(hour = hour(start_time),
                      start_station = fct_reorder(start_station, start_region)) %>% 
                group_by(hour, start_station) %>% 
                tally() %>%
  ggplot(aes(hour, start_station)) + 
  geom_raster(aes(fill=n), colour = "grey100", linejoin = "bevel") +
  theme(axis.text.y = element_text(angle = 0, vjust = 0, size = 4)) +
  ggtitle("Heatmap of Hourly Rides per station")


# Furthermore, looking at boxplots of hourly rides by stations, hourly data will not be a really solid predictor.


# this is not a really solid predictor since there are so few rides hourly

bike_data2 %>% mutate(hour = hour(start_time)) %>%
  group_by(start_date, start_station, hour, start_region) %>% dplyr::summarise(avg_hr_rides = n()) %>%
  ggplot() + geom_boxplot(aes(hour, avg_hr_rides, fill=as.character(hour))) + 
  facet_wrap(~start_region, scales="free") + theme(legend.position = "none") + ggtitle("Hourly rides by region - Many Outliers")



# Hours will not be used in the predictive model.

### Day of week
# What can the day of week tell us?


bike_data2 %>% 
  mutate(wday = wday(start_date, label=TRUE)) %>% ggplot(aes(wday,  fill=wday)) + stat_count() + ggtitle("Rides per Day of Week")


# ## by station
# bike_data2 %>% 
#   mutate(wday = wday(start_date, label=TRUE)) %>% 
#   ggplot() + 
#   geom_bar(aes(wday,  shape = start_region, fill=start_station), color="gray", size=0.25) + 
#   ggtitle("Rides per Day of Week by Region, split by Station") + facet_wrap(~start_region, scales = "free") +  theme(legend.position = "none")  #+ transition_reveal(as.integer(wday))


# There's nothing really discernible here, but if you recall, we know that ride behavior is different by region.  Splitting this view out by region shows a clear difference in the rides.


## by station
bike_data2 %>% 
  mutate(wday = wday(start_date, label=TRUE)) %>% 
  ggplot() + 
  geom_bar(aes(wday,  shape = start_region, fill=wday), color="gray", size=0.25) + 
  ggtitle("Rides per Day of Week by Region, split by Station") + facet_wrap(~start_region, scales = "free") +  theme(legend.position = "none")  #+ transition_reveal(as.integer(wday))


# There are more weekend bike riders in Westside and Long Beach, since that's where the fun is at... on the beach!  It's pretty apparent that bikers from DTLA are commuters, versus Westside and Port of LA, which are more popular on the weekends.


bike_model <- bike_data2 %>% 
  mutate(#hour = hour(start_time),
         month = month(start_date, label = TRUE, abbr = TRUE), 
         year = year(start_date),
         wday = wday(start_date, label=TRUE),
         day = day(start_date)) %>%
  group_by(start_region, start_station, start_lat, start_lon, start_date, year, month, day, wday)  %>% # remove some vars that we wont be using
  tally() %>% ungroup()

# length(unique(bike_model$start_lat[bike_model$start_station=="California & Lincoln"]))

head(bike_model, 10) %>% kable() %>%
  kable_styling(full_width = F)


# So far, I've explored date-based trends and have found that *month* and *day of week + region* will be useful predictors in our model. One reason why certain months have more or less rides is due to temperature. Is there correlation between bike rides and temperature?

# ### Temperature
# The data set from Bike Share does not include temperature. Fortunately, there's an R package called "*GSODR*" that I'm trying out. 

# To map the temp data into the bike info, I had to match the weather station IDs with the start_regions.  It took a bit of inspection of the station names, but I found: 

# if (!require("remotes")) {
#   install.packages("remotes", repos = "http://cran.rstudio.com/")
#   library("remotes")
# }
# 
# install_github("ropensci/GSODR")
library(GSODR)

## let's search by proximity
coords <- bike_model[1,] %>% select(start_lat, start_lon)
# library(sp) # needed to search by coordinates
LAstatID <- nearest_stations(LAT = coords[[1]], LON = coords[[2]], distance = 30)

weather_full <- get_GSOD(c(2016:2019), station = LAstatID) # save the data

# glimpse(weather_full) # take a look

weather_full %>% group_by(NAME, STNID) %>% tally() # which station shall we use

my_stations <- data.frame(start_region = c("DTLA", "Port of LA", "Westside"),
                          sid = c("722874-93134", "722970-23129", "722885-93197"))
print("The names were pretty clear for me to align to my 3 regions:")
my_stations

#join this back into the bike_model
bike_model2 <- weather_full %>% 
  filter(STNID %in% my_stations[,2]) %>% left_join(my_stations, by=c("STNID" = "sid")) %>%
  select(start_region, YEARMODA, TEMP, DEWP, SLP, STP, VISIB, WDSP, MIN, MAX) %>% 
  right_join(bike_model, by = c("start_region", "YEARMODA" = "start_date"))

glimpse(bike_model2)

#are there an NA's?

na_index <- which(is.na(bike_model$TEMP) | is.na(bike_model2$DEWP) | is.na(bike_model2$VISIB))
# na_index #great! good data


# The weather vars collected are as follows:

# - TEMP = Avg temperature for the day, in Celsius.

# - MIN = Min temperature for the day, in Celsius.

# - MAX = Max temperature for the day, in Celsius.

# - DEWP = Mean daily dew point converted to degrees C to tenths.

# - SLP = Mean sea level pressure in millibars to tenths.

# - STP = Mean station pressure for the day in millibars to tenths.


# Let's plot each of these temp metrics against # of rides.


bike_model2 %>% dplyr::group_by(YEARMODA) %>% 
  dplyr::summarise(daily_rides = sum(n),
                   TEMP = mean(TEMP),
                   MIN = mean(MIN),
                   MAX = mean(MAX),
                   DEWP = mean(DEWP),
                   SLP = mean(SLP),
                   STP = mean(STP),
                   VISIB = mean(VISIB),
                   WDSP = mean(WDSP)) %>%
  gather("weather", "C", TEMP, MIN, MAX, DEWP, SLP, STP, VISIB, WDSP) %>%
  ggplot(aes(C, daily_rides, color = weather)) + geom_point(alpha=0.25) + facet_wrap(~weather, scales = "free") + 
  geom_smooth(span = 0.15, method.args = list(degree=3)) +
  ggtitle("# of Rides vs Weather Metrics") + theme(legend.position = "none")


# Taking a closer look at STP and TEMP...


bike_model2 %>% dplyr::group_by(YEARMODA) %>% 
  dplyr::summarise( daily_rides = sum(n),
                    STP = mean(STP) ) %>%
  ggplot(aes(daily_rides, STP)) + xlim(0,1000) + ylim(0,15) + geom_point() + geom_smooth() + ggtitle("STP up close - no correlation")

bike_model2 %>% dplyr::group_by(YEARMODA) %>% 
  dplyr::summarise(daily_rides = sum(n),
                   TEMP = mean(TEMP)) %>%
  ggplot(aes(TEMP, daily_rides, color = TEMP)) + geom_point(alpha=0.5) + #facet_wrap(~weather, scales = "free") + 
  geom_smooth(span = 0.15, method.args = list(degree=3)) +
  ggtitle("# of Rides vs Temperature") #+ theme(legend.position = "none")

cat("Correlation between TEMP and rides is", bike_model2 %>% filter(!is.na(TEMP)) %>% group_by(TEMP) %>% dplyr::summarise(rides = sum(n)) %>%
  filter(rides>1000) %>% dplyr::summarise(cor_temp_rides = cor(TEMP, rides)) %>% pull())
## not linearly related...


# The correlation between TEMP and rides is not linear, yet there is a clear increase in rides until around 25 degrees Celsius, where it drops off afterwards... too hot!

# Other metrics such as Visibility (VISIB) and STP do not have enough variance to spot a trend.

# TEMP, MIN, and MAX show interesting, similar trends, but do we need all three?


bike_model2 %>% dplyr::group_by(YEARMODA) %>% 
  dplyr::summarise(daily_rides = sum(n),
                   TEMP = mean(TEMP),
                   MIN = mean(MIN),
                   MAX = mean(MAX)) %>%
 # gather("weather", "C", TEMP, MIN, MAX) %>%
  ggplot(aes(TEMP, MIN, MAX), alpha=0.5) + 
  geom_line(aes(YEARMODA, TEMP), color="yellow") + #geom_smooth(aes(YEARMODA, TEMP), color="grey", alpha=0.5) +
  geom_line(aes(YEARMODA, MIN), color="blue") +
  geom_line(aes(YEARMODA, MAX), color="red") +
  theme(legend.position = "top", axis.title.y = element_blank() ) +
  transition_reveal(YEARMODA) +
  ggtitle("Daily Temps") + theme(axis.title.x = element_blank())


cat("Correlation between TEMP and MAX is", bike_model2 %>% filter(!is.na(TEMP)) %>% summarise(corr_TEMP_MAX = cor(TEMP, MAX)) %>% pull())

cat("Correlation between TEMP and MIN is",bike_model2 %>% filter(!is.na(TEMP)) %>% summarise(corr_TEMP_MIN = cor(TEMP, MIN)) %>% pull())

# Using all three won't have much impact on the model


# Due to such a high correlation, using all three won't have incremental impact on the model.  So I will only use TEMP.

# Let's look at wind speed a bit closer, stratified by Temperature.


bike_model2 %>% dplyr::group_by(YEARMODA) %>% 
  dplyr::summarise(daily_rides = sum(n),
                   WDSP = mean(WDSP),
                   TEMP_strata = round(mean(TEMP), 0)) %>%
  filter(WDSP < 1.8) %>%
  ggplot(aes(WDSP, daily_rides, color = WDSP)) + geom_point(alpha=0.5) +
  facet_wrap(~TEMP_strata, scales = "free") +
  geom_smooth(method = "lm", span = 0.15, method.args = list(degree=3)) +
  ggtitle("# of Rides vs Wind Speed stratified by Temperature") #+ theme(legend.position = "none")

bike_model2 %>% dplyr::group_by(YEARMODA) %>% 
  dplyr::summarise(daily_rides = sum(n),
                   WDSP = mean(WDSP),
                   TEMP = mean(TEMP)) %>%
  filter(WDSP < 1.8) %>% summarise(corr_TEMP_WDSP = cor(WDSP, TEMP)) ## may not be a decent predictor...


# Since the slopes are all over the place here, I do not see approximate bivariate normal distributions for WDSP versus rides when stratified by TEMP.

# # Building a Predictive Model

# I will build a model to predict the number of rides per station on any given day. From the previous analyses, I will be using the variables that for the prediction that showed strong correlation with rides: *month, day of week, station, region, & TEMP.


bike_model2 <- bike_model2 %>% mutate(key = paste0(start_station, "_",as.character(YEARMODA))) %>%
  select(-DEWP, -VISIB, -SLP, -STP, -MIN, -MAX)

length((bike_model2$key)) - length(unique(bike_model2$key)) # 0 dupes!

glimpse(bike_model2)

# The *key* will be the combination of start_station and the date.

# Using the **caret** package, I split the oberservations into training and test sets.  I will be setting aside 15% of the data for testing the model.


# length(bike_model2$key)
set.seed(3, sample.kind="Rounding")
test_index <- createDataPartition(y = bike_model2$n, times = 1, p = 0.15, list = FALSE)
train <- bike_model2[-test_index,]
temp <- bike_model2[test_index,]

## need to ensure station, temp, region, wday values that are in test are also in train
test <- temp %>%
  semi_join(train, by = "start_station") %>%
  semi_join(train, by = "TEMP") %>%
  semi_join(train, by = "YEARMODA") %>%
  semi_join(train, by = c("wday", "start_region"))

# Adding rows removed from validation set back into train set
removed <- anti_join(temp, test, by = c("start_region", "YEARMODA", "TEMP", "WDSP", "start_station"))
train <- rbind(train, removed)

head(train) %>% kable() %>% kable_styling(full_width=F)


# Quick inspection of the data sets.

# head(train$key)

library(scales)
grid.arrange(data.frame(group = c("train", "test"),
           value = c(length(train[,1]), length(test[,1]))) %>%
          ggplot(aes(x="", y=value, fill=group)) + geom_bar(width = 1, stat = "identity") + geom_text(aes(label=percent(value/(length(train[,1]) + length(test[,1])))), vjust=3, position = position_stack()) + theme(aspect.ratio = 5) + scale_y_continuous(labels = comma) +
    theme(axis.text.x=element_blank(), axis.title.x = element_blank()) + ggtitle("Breakout of Groups"),


train %>% ggplot(aes(n, YEARMODA)) + geom_point(aes(color = start_station), alpha=0.25, fill = "black") + facet_wrap(~start_region) +
  theme(legend.position = "none") + coord_flip() +
  ggtitle("Training Data - Rides over time colored by Station")
, nrow=1, ncol=2, widths=1:2)
# fit <- loess(n ~ as.numeric(wday), degree=1, data=bike_model2)


# ### Root Mean Square Error (AKA **RMSE**) will be the measure of accuracy of the model.
# The calculation used here is
# $\sqrt(1/n\sum_{1}^{n} (actualrides_{n} - predictedrides_{n})^2)$

# The RSME function has been saved for future use:

RMSE <- function(actual_rides, predicted_rides){
  sqrt(mean((actual_rides - predicted_rides)^2))
}
# RMSE


# Building the model using ...

# ### Overall Average


mu <- train %>% summarise(mu = mean(n)) %>% pull()

cat("Has a RSME of", RMSE(test$n, mu), "which is not very good!")

RMSE_avg <- RMSE(test$n, mu)

cat("\nThis means that the model is off by over", RMSE_avg, "rides per day per station.")


# This is ok since it's the baseline for the model.  I will then add iteratively each variable into the model.  The approach is to take the average at each variable roll-up and add as an adjustor to the overall average.


### Station Average

st_lookup <- train %>% group_by(start_station) %>% dplyr::summarise(p_station = mean(n - mu))

st_lookup %>% arrange(desc(p_station)) %>% head(10) %>% kable() #show highest hourly p

predict_station <- mu + test %>% left_join(st_lookup) %>% pull(p_station)

cat("Has a RSME of", RMSE(test$n, predict_station), "which is already much better!")
# RMSE(test$n, predict_station) ## already better

RMSE_station <- RMSE(test$n, predict_station)



wday_lookup <- train %>% left_join(st_lookup) %>% group_by(wday) %>%
  dplyr::summarise(p_wday = mean(n - mu - p_station))

predict_wday <- mu + test %>% left_join(st_lookup) %>% left_join(wday_lookup) %>%
  mutate(pred = p_station + p_wday) %>% pull(pred)

RMSE(test$n, predict_wday) ## not much better at all!

RMSE_wday <- RMSE(test$n, predict_wday)


### Day of Week and Region
# You may recall during the analysis, We found that there were distinct distributions by day of week across the regions.  I will work this into our model.


region_wday_lookup <- train %>% left_join(st_lookup) %>% group_by(start_region, wday) %>% dplyr::summarise(p_wday = mean(n - mu - p_station))

predict_st_region_wday <- mu + test %>% left_join(st_lookup) %>%
  left_join(region_wday_lookup, by=c("wday", "start_region")) %>%
  mutate(pred = p_station + p_wday) %>% pull(pred)

cat("Has a RSME of", RMSE(test$n, predict_st_region_wday), "which is slightly better.") ## a bit better

RMSE_wdayregion <- RMSE(test$n, predict_st_region_wday)


### Temperature inclusion
# I know weather is a factor, so let's use TEMP


temp_lookup <- train %>% left_join(st_lookup) %>% left_join(region_wday_lookup) %>%
  group_by(TEMP) %>% dplyr::summarise(p_temp = mean(n - mu - p_station - p_wday))

predict_st_region_wday_temp <- mu + test %>% left_join(st_lookup) %>%
  left_join(region_wday_lookup) %>% left_join(temp_lookup) %>% 
  mutate(pred = p_station + p_wday + p_temp)%>% pull(pred)

# RMSE(test$n, predict_st_region_wday_temp)
cat("Has a RSME of", RMSE(test$n, predict_st_region_wday_temp), "which is slightly better.") ## a bit better

RMSE_temp <- RMSE(test$n, predict_st_region_wday_temp)

# wdsp_lookup <- train %>% left_join(st_lookup) %>% left_join(region_wday_lookup) %>% left_join(temp_lookup) %>%
#   group_by(WDSP) %>% dplyr::summarise(p_wdsp = mean(n - mu - p_station - p_wday - p_temp))
# 
# predict_st_region_wday_temp_wdsp <- mu + test %>% left_join(st_lookup) %>%
#   left_join(region_wday_lookup) %>% left_join(temp_lookup) %>% left_join(wdsp_lookup) %>%
#   mutate(pred = p_station + p_wday + p_temp + p_wdsp)%>% pull(pred)
# 
# RMSE_wdsp <- RMSE(test$n, predict_st_region_wday_temp_wdsp)


### Monthly Adjustment

month_lookup <- train %>% 
  left_join(st_lookup) %>% left_join(region_wday_lookup) %>% 
  left_join(temp_lookup) %>%
  group_by(month) %>% dplyr::summarise(p_month = (mean(n - mu - p_station - p_wday - p_temp))) %>% ungroup()

predict_st_region_wday_temp_month <- mu + test %>% left_join(st_lookup) %>%
  left_join(region_wday_lookup) %>% left_join(temp_lookup) %>% 
  left_join(month_lookup) %>%
  mutate(pred =  p_station + p_wday + p_temp + p_month) %>% 
  pull(pred)

cat("Has a RSME of", RMSE(test$n, predict_st_region_wday_temp_month) , "which is a nice boost to error reduction.")
#small help
RMSE_month <- RMSE(test$n, predict_st_region_wday_temp_month)



### Yearly Adjustment

year_lookup <- train %>% 
  left_join(st_lookup) %>% left_join(region_wday_lookup) %>% 
  left_join(temp_lookup) %>% left_join(month_lookup) %>%
  group_by(start_region, year) %>% dplyr::summarise(p_year = (mean(n - mu - p_station - p_wday - p_temp - p_month))) %>% ungroup()

predict_st_region_wday_temp_month_year <- mu + test %>% left_join(st_lookup) %>%
  left_join(region_wday_lookup) %>% left_join(temp_lookup) %>% 
  left_join(month_lookup) %>% left_join(year_lookup) %>%
  mutate(pred =  p_station + p_wday + p_temp + p_month + p_year) %>% 
  pull(pred)

cat("Has a RSME of", RMSE(test$n, predict_st_region_wday_temp_month_year) , "which is a nice boost to error reduction.")
#small help
RMSE_year <- RMSE(test$n, predict_st_region_wday_temp_month_year)



### Residuals
# Where am I most off? Let's inspect the daily trend by region.

train %>% left_join(st_lookup) %>% 
  left_join(region_wday_lookup) %>% 
  left_join(temp_lookup) %>%
  left_join(month_lookup) %>%
  mutate(pred = mu + p_station + p_wday + p_temp + p_month, resid = n - pred) %>% 
  group_by(month, start_region) %>%
  dplyr::summarise(resid = sum(resid)) %>%
  ggplot(aes(month, resid)) + geom_point(aes(fill=abs(resid)), size = 12, shape=21) + scale_fill_continuous(low = "grey", high = "red") +
  ggtitle("Average Residual by Month") + facet_wrap(~start_region, nrow=3, scales="free")

#What's alarming with these residuals is that the summer months have predictions that are either too high or too low.  If they were both over or under, then we would be able to normalize within our model.  This tells me that there are other factors to consider that we don't already have data for.




residd <- train %>% left_join(st_lookup) %>% 
  left_join(region_wday_lookup) %>% 
  left_join(temp_lookup) %>%
  left_join(month_lookup) %>%
  left_join(year_lookup) %>%
  mutate(pred = mu + p_station + p_wday + p_temp + p_month + p_year, resid = n - pred)

residd %>% group_by(YEARMODA, start_region) %>% summarise(n = sum(n), pred = sum(pred)) %>%
  ggplot() +
    geom_line(aes(YEARMODA, n, color="Actual Rides")) +
  geom_line(aes(YEARMODA, pred, color="Predicted Rides"), alpha=0.5) +   
  theme(legend.position = "top", legend.title = element_blank()) +
    scale_color_manual(values = c("green", "purple")) +
  # ggplot() + geom_line(aes(YEARMODA, n), color="green") +
  # geom_line(aes(YEARMODA, pred), color="purple", alpha=0.5) +
  # theme(legend.position = "top") +
  facet_wrap(~start_region, nrow=3, scales="free") + ylab("Rides") + xlab("Date") +
  ggtitle("Training Data")



# There are big misses in the predictions for days where the rides are abnormally high:


train %>% left_join(st_lookup) %>% 
  left_join(region_wday_lookup) %>% 
  left_join(temp_lookup) %>%
  left_join(month_lookup) %>%
  left_join(year_lookup) %>%
  mutate(pred = mu + p_station + p_wday + p_temp + p_month + p_year, resid = n - pred) %>% 
  group_by(start_region, YEARMODA) %>% summarise(avg_resid=mean(resid)) %>% 
  arrange(desc((avg_resid))) %>% head(8) %>% kable(digits=2) %>%
  kable_styling(full_width = F)


# In doing a bit of research of what may have been going on for these dates, it became very apparent that Los Angeles is full of outdoor events.  One that is very popular is 

### [Ciclavia](https://www.ciclavia.org)
# **Ciclavia** "catalyzes vibrant public spaces, active transportation and good health through car-free streets." They put on large biking events around the city. The [past events](https://www.ciclavia.org/events_history) align quite closely to some of these large residuals, so I will scrape the site and pull in the dates and locations of each.


#pull dates and names of events
# save the url
url2 <- "https://www.ciclavia.org/events_history"

library(rvest)
library(magrittr)

readit <- read_html(url2)
# vignette("selectorgadget")
nodes <- html_nodes(readit, "h4") #using rvest
nodes2 <- html_nodes(readit, "h2") #using rvest

node_index <- str_detect(html_text(nodes), "Sunday") # grab only the dates
html_text(nodes)[node_index]
html_text(nodes2)
ciclavia <- data.frame(date = html_text(nodes)[node_index],
           location = html_text(nodes2))


head(ciclavia) %>% kable() %>%
  kable_styling(full_width = F)



# I then clean up the dates and join it against our bike model data


ciclavia %>% mutate(date2= str_sub(date, 8), date3 = parse_date(str_sub(date, 8), "%B %e, %Y")) %>% head() %>% kable() %>%
  kable_styling(full_width = F)

# parse date and remove events before the data we have in the model
ciclavia <- ciclavia %>% mutate(date2= str_sub(date, 8), date3 = parse_date(str_sub(date, 8), "%B %e, %Y")) %>% select(date3, location) %>% filter(date3 >= min(bike_model2$YEARMODA))

# bike_model2 <- bike_model2 %>% left_join(ciclavia, by = c("YEARMODA" = "date3"))

## average residuals for ciclavia vs non-ciclavia

print("Comparing residuals for days with Ciclavia events vs all other days...")

train %>% left_join(st_lookup) %>% 
  left_join(region_wday_lookup) %>% 
  left_join(temp_lookup) %>% 
  left_join(month_lookup) %>%
  left_join(year_lookup) %>%
  left_join(ciclavia, by = c("YEARMODA" = "date3")) %>%
   mutate(pred = mu + p_station + p_wday + p_temp + p_month + p_year, resid = n - pred) %>% 
  mutate(cic = ifelse(is.na(location), "non-ciclavia days", "ciclavia days")) %>%
  group_by(cic) %>% dplyr::summarise(avg_resid = mean(abs(resid)))
  

# So overall, the mean absolute residuals are almost twice as high during ciclavia days.  However, note that these events aren't usually across the whole city. Rather, these events are specific to certain areas...


m + geom_point( data = 
     train %>% left_join(st_lookup) %>% 
     left_join(region_wday_lookup) %>% 
      left_join(temp_lookup) %>%
  left_join(month_lookup) %>%
    left_join(year_lookup) %>%
      left_join(ciclavia[,c(1,2)], by = c("YEARMODA" = "date3")) %>%
 mutate(pred = mu + p_station + p_wday + p_temp + p_month + p_year, resid = n - pred) %>%
      mutate(cic = ifelse(is.na(location), "non-ciclavia days", "ciclavia days")) %>%
      group_by(start_region, start_station, start_lat, start_lon, cic, location) %>%
                 dplyr::summarise(avg_resid = mean(abs(resid))),
  aes(y=start_lat, x=start_lon, size=avg_resid, fill=start_region), pch = 21, show.legend=TRUE) +
  facet_wrap(~location) +
  theme_dark() + xlab("") + ylab("") + 
  theme(strip.text = element_text(size = 6), axis.text.y=element_blank(), , axis.text.x=element_blank()) +
  ggtitle("Training residuals by Ciclavia events")
    # transition_states(location, wrap=TRUE, state_length = 8) +
    # labs(title = "{closest_state}") 


 train %>% left_join(st_lookup) %>% 
     left_join(region_wday_lookup) %>% 
      left_join(temp_lookup) %>% 
  left_join(month_lookup) %>%
  left_join(year_lookup) %>%
      left_join(ciclavia[,c(1,2)], by = c("YEARMODA" = "date3")) %>%
      mutate(pred = mu + p_station + p_wday + p_temp + p_month + p_year, resid = n - pred) %>%
      mutate(cic = ifelse(is.na(location), "non-ciclavia days", "ciclavia days")) %>%
      group_by(start_region, cic, location) %>%
                 dplyr::summarise(avg_resid = mean(abs(resid))) %>%
   ggplot(aes(location, start_region)) + coord_flip() +
  geom_raster(aes(fill=avg_resid), linejoin = "bevel") +
   scale_fill_gradientn(colours=c("gray","red")) + geom_text(aes(label = round(avg_resid,2))) +
  theme(axis.text.y = element_text(angle = 0, vjust = 0, size = 8)) +
  ggtitle("Heatmap avg residual errors")



# We can now label the regions in which ciclavia would be impacting bike rides.


ciclavia <- ciclavia %>% mutate(start_region = 
                                  ifelse(str_detect(location, "Wilmington"), "Port of LA", 
                                     ifelse(str_detect(location, "Heart of LA") | str_detect(location, "Phil"), "DTLA",
                                            ifelse(str_detect(location, "Culver") | str_detect(location, "Foot"), "Westside",
                                     "Ignore")))) 

ciclavia %>% filter(start_region != "Ignore") %>% kable() %>%
  kable_styling(full_width = F)


# We can now add this as a new variable into the train and test data, joined off of date and region.  Then use it as a predictor.


# clean up data to join
ciclavia2 <- ciclavia %>% mutate(ciclavia_event = location, YEARMODA = date3) %>% select(YEARMODA, ciclavia_event, start_region)

#join to train
train2 <- train %>% left_join((ciclavia2 %>% filter( start_region != "Ignore") ), by = c("start_region", "YEARMODA")) 

#join to test
test2 <- test %>% left_join((ciclavia2 %>% filter( start_region != "Ignore") ), by = c("start_region", "YEARMODA")) 

# train2 %>% filter(!is.na(ciclavia_event)) %>% head()



# Viewing these events as labels in our original Residual view...


residd %>% left_join(ciclavia2) %>% group_by(YEARMODA, ciclavia_event, start_region) %>% summarise(n = sum(n), pred = sum(pred)) %>% ggplot(show.legend=TRUE) + 
  geom_line(aes(YEARMODA, n, color="Actual Rides")) +
  geom_line(aes(YEARMODA, pred, color="Predicted Rides"), alpha=0.5) + geom_label(aes(y=n,x=YEARMODA, label=ciclavia_event), position="dodge") + 
  theme(legend.position = "top", legend.title = element_blank()) +
    scale_color_manual(values = c("green", "purple")) +
  facet_wrap(~start_region, nrow=3, scales="free") + ylab("rides") + xlab("Date") +
  ggtitle("Training Data")


# Now let's add ciclavia into the prediction... This is a bit trickier than the other variables, given that the adjsutor should only apply to specific days and regions. 


cicla_lookup2 <- train2 %>% 
  filter(!is.na(ciclavia_event)) %>% # just the ciclavia days in the region that is impacted
  left_join(st_lookup) %>% left_join(region_wday_lookup) %>% 
  left_join(temp_lookup) %>% left_join(year_lookup) %>% left_join(month_lookup) %>%
  group_by(start_station) %>% dplyr::summarise(p_cicla = (mean(n - mu - p_station - p_wday - p_temp - p_year - p_month))) %>% ungroup()



# cicla_lookup <- train2 %>% 
#   filter(!is.na(ciclavia_event)) %>% # just the ciclavia days in the region that is impacted
#   left_join(st_lookup) %>% left_join(region_wday_lookup) %>% 
#   left_join(temp_lookup) %>% left_join(wdsp_lookup) %>%
#   group_by(start_region, YEARMODA) %>% dplyr::summarise(p_cicla = (mean(n - mu - p_station - p_wday - p_temp - p_wdsp))) %>% ungroup()

#let's try at station level
cicla_lookup2 <- train2 %>% 
  filter(!is.na(ciclavia_event)) %>% # just the ciclavia days in the region that is impacted
  left_join(st_lookup) %>% left_join(region_wday_lookup) %>% 
  left_join(temp_lookup) %>% left_join(year_lookup) %>% left_join(month_lookup) %>%
  group_by(start_region) %>% dplyr::summarise(p_cicla = (mean(n - mu - p_station - p_wday - p_temp - p_year - p_month))) %>% ungroup()


head(cicla_lookup2,10) %>% kable() %>% kable_styling(full_width=F)## this is the adjustor when there is an event in the region

# cicla_lookup2 <- train %>% filter(!is.na(ciclavia_event)) %>% 
#   left_join(st_lookup) %>% left_join(region_wday_lookup) %>% 
#   left_join(temp_lookup) %>% select()



# We then create some if statements for the values that don't exist for certain stations:

predict_st_region_wday_temp_cicla <- mu + test %>% 
  left_join(st_lookup) %>% left_join(region_wday_lookup) %>% left_join(temp_lookup) %>% left_join(year_lookup) %>% left_join(month_lookup) %>%
  left_join(cicla_lookup2) %>%
  left_join(ciclavia2) %>%
  mutate(pred = ifelse(is.na(ciclavia_event), 
                       p_station + p_wday + p_temp + p_year + p_month, 
                        ifelse(is.na(p_cicla),
                            p_station + p_wday + p_temp + p_year + p_month,    
                            p_station + p_wday + p_temp + p_year + p_month + p_cicla))) %>% 
  pull(pred)

cat("Yields an RMSE of", RMSE(test$n, predict_st_region_wday_temp_cicla),"which is better!") #nice help
RMSE_cicla <- RMSE(test$n, predict_st_region_wday_temp_cicla)


# Looks better for those days!


residd2 <- train %>% left_join(st_lookup) %>% 
  left_join(region_wday_lookup) %>% 
  left_join(temp_lookup) %>%
  left_join(month_lookup) %>%
  left_join(year_lookup) %>%
  left_join(cicla_lookup2) %>%
  left_join(ciclavia2) %>%
  mutate(pred = ifelse(is.na(ciclavia_event), 
                      mu + p_station + p_wday + p_temp + p_year + p_month, 
                        ifelse(is.na(p_cicla),
                           mu + p_station + p_wday + p_temp + p_year + p_month,    
                           mu + p_station + p_wday + p_temp + p_year + p_month + p_cicla)), resid = n - pred) 

residd2 %>% group_by(YEARMODA, ciclavia_event, start_region) %>% summarise(n = sum(n), pred = sum(pred)) %>% ggplot(show.legend=TRUE) + 
  geom_line(aes(YEARMODA, n, color="Actual Rides")) +
  geom_line(aes(YEARMODA, pred, color="Predicted Rides"), alpha=0.5) + geom_label(aes(y=n,x=YEARMODA, label=ciclavia_event), position="dodge") + 
  theme(legend.position = "top", legend.title = element_blank()) +
    scale_color_manual(values = c("green", "purple")) +
  facet_wrap(~start_region, nrow=3, scales="free") + ylab("rides") + xlab("Date") +
  ggtitle("Training Data")


# The event anomalies are accounted for! We are very accurate at the daily-regional level, but pretty a miss at the more granular station level.  


residd2 %>% group_by(start_station) %>% summarise(avg_n=mean(n),avg_pred=mean(pred), avg_resid=mean(resid), avg_st = mean(p_station)) %>% arrange(desc(abs(avg_resid))) %>% kable(digits=2) %>%
  kable_styling(full_width = F) %>% head()

residd2 %>% group_by(start_station) %>% summarise(avg_n=mean(n),avg_pred=mean(pred), avg_resid=mean(resid), avg_st = mean(p_station)) %>% 
  filter(abs(avg_resid) < 10 & avg_n < 27) %>% arrange(desc(abs(avg_resid))) %>%
  ggplot() + geom_point(aes(x=avg_n, y=avg_resid, color=abs(avg_resid))) + scale_color_continuous(low="green", high="red") + ggtitle("Daily average rides vs Average Residual Error") + annotate("text", size=8, color = "Orange", x = 15, y = 2.5, label = "Most large residual errors come\n from stations that have very few daily rides") +
  annotate("segment", y=2.75, yend=1.75, x=9, xend=3, color="Orange", arrow=arrow(length=unit(0.05,"npc")))


# We can assume that residuals will be larger the more rides there are, so I've created a new metric that measures the relative residual against the number of rides:

# Proportional Residual$_{n}$ = $|$Residual Error$_{n}$$/$Number of rides$_{n}$$|$.


residd2 %>% mutate(resid_to_n = abs(resid/n)) %>%
  ggplot() + geom_point(aes(x=n, y=resid_to_n, fill=(resid_to_n)), shape=21, alpha=0.25) + scale_fill_continuous(low="green", high="red") + ggtitle("Daily rides vs Proportion of Residual Error") +
  annotate("text", size=8, color = "Orange", x = 75, y = 20, label = "Most large relative residual errors come\n from stations that have\n very few daily rides") +
  annotate("segment", y=20, yend=15, x=40, xend=10, color="Orange", arrow=arrow(length=unit(0.05,"npc"))) +
  annotate("segment", y=24, yend=30, x=40, xend=8, color="Orange", arrow=arrow(length=unit(0.05,"npc")))




# In these cases of low daily rides, we should not use much of an adjustor other than the station average. This can be done with regularization!

#### [Regularization](https://cdn.shopify.com/s/files/1/0058/3532/products/SCAN0467_2000x.JPG?v=1466634135), mount up!


lambdas <- seq(0, 10, .10)

  rmses <- sapply(lambdas, function(l){
  predicted_ratings <- test2 %>% 
  left_join(st_lookup) %>% 
  left_join(region_wday_lookup) %>% 
  left_join(temp_lookup) %>%
  left_join(month_lookup) %>%
  left_join(year_lookup) %>%
  left_join(cicla_lookup2) %>%
  left_join(ciclavia2) %>%
    mutate(pred = mu + ifelse(is.na(ciclavia_event), 
                     p_station + (p_wday + p_temp + p_year + p_month)*(n/(n+l)), 
                        ifelse(is.na(p_cicla),
                           p_station + (p_wday + p_temp + p_year + p_month)*(n/(n+l)),    
                           p_station + (p_wday + p_temp + p_year + p_month + p_cicla)*(n/(n+l)))))%>%
    pull(pred)
    
  return(RMSE(predicted_ratings, test2$n))
  } )
  


qplot(lambdas, rmses) +
  annotate("text", x= lambdas[which.min(rmses)], y=rmses[which.min(rmses)] * 1.002, label="Best lambda", color="blue") +
  annotate("segment", x=lambdas[which.min(rmses)], xend=lambdas[which.min(rmses)], y=rmses[which.min(rmses)]*1.0015, yend=rmses[which.min(rmses)], color="blue", arrow=arrow(length=unit(0.05,"npc")))
cat("Best lambda to minimize RMSE:", lambdas[which.min(rmses)], "for a minimum RMSE of", rmses[which.min(rmses)], "!")


# With regularization, we are able to reduce the effects of day of week+region, temperature, year, and month for the keys (station+day) that have very few rides.  Let's apply this updated model and save the predictions.



l <- lambdas[which.min(rmses)]

predict_st_region_wday_temp_cicla_REG <- test2 %>% 
  left_join(st_lookup) %>% 
  left_join(region_wday_lookup) %>% 
  left_join(temp_lookup) %>%
  left_join(month_lookup) %>%
  left_join(year_lookup) %>%
  left_join(cicla_lookup2) %>%
  left_join(ciclavia2) %>%
    mutate(pred = mu + ifelse(is.na(ciclavia_event), 
                     p_station + (p_wday + p_temp + p_year + p_month)*(n/(n+l)), 
                        ifelse(is.na(p_cicla),
                           p_station + (p_wday + p_temp + p_year + p_month)*(n/(n+l)),    
                           p_station + (p_wday + p_temp + p_year + p_month + p_cicla)*(n/(n+l)))))%>%
    pull(pred)
    

cat("Has a RSME of", RMSE(predict_st_region_wday_temp_cicla_REG, test2$n) , "which is great!")

RMSE_reg <- RMSE(predict_st_region_wday_temp_cicla_REG, test2$n)




residd3 <- train %>% left_join(st_lookup) %>% 
  left_join(region_wday_lookup) %>% 
  left_join(temp_lookup) %>%
  left_join(month_lookup) %>%
  left_join(year_lookup) %>%
  left_join(cicla_lookup2) %>%
  left_join(ciclavia2) %>%
    mutate(pred = mu + ifelse(is.na(ciclavia_event), 
                     p_station + (p_wday + p_temp + p_year + p_month)*(n/(n+l)), 
                        ifelse(is.na(p_cicla),
                           p_station + (p_wday + p_temp + p_year + p_month)*(n/(n+l)),    
                           p_station + (p_wday + p_temp + p_year + p_month + p_cicla)*(n/(n+l)))),
           resid = n - pred) 

residd3 %>% group_by(YEARMODA, ciclavia_event, start_region) %>% summarise(n = sum(n), pred = sum(pred)) %>% ggplot(show.legend=TRUE) + 
  geom_line(aes(YEARMODA, n, color="Actual Rides")) +
  geom_line(aes(YEARMODA, pred, color="Predicted Rides"), alpha=0.5) + geom_label(aes(y=n,x=YEARMODA, label=ciclavia_event), position="dodge") + 
  theme(legend.position = "top", legend.title = element_blank()) +
    scale_color_manual(values = c("green", "purple")) +
  facet_wrap(~start_region, nrow=3, scales="free") + ylab("rides") + xlab("Date") +
  ggtitle("Training Data")



grid.arrange(residd2 %>% mutate(resid_to_n = abs(resid/n)) %>%
  ggplot() + geom_point(aes(x=n, y=resid_to_n, fill=(resid_to_n)), shape=21, alpha=0.25) + scale_fill_continuous(low="green", high="red") + ggtitle("Daily rides vs Proportion of Residual Error with Regularization") +
  annotate("text", size=8, color = "Orange", x = 75, y = 20, label = "Most large relative residual errors come\n from stations that have\n very few daily rides") +
  annotate("segment", y=20, yend=15, x=40, xend=10, color="Orange", arrow=arrow(length=unit(0.05,"npc"))) +
  annotate("segment", y=24, yend=30, x=40, xend=8, color="Orange", arrow=arrow(length=unit(0.05,"npc"))),
residd3 %>% mutate(resid_to_n = abs(resid/n)) %>%
  ggplot() + geom_point(aes(x=n, y=resid_to_n, fill=(resid_to_n)), shape=21, alpha=0.25) + scale_fill_continuous(low="green", high="red") + ggtitle("Daily rides vs Proportion of Residual Error with Regularization") +
  annotate("text", size=8, color = "Orange", x = 75, y = 20, label = "Most large relative residual errors come\n from stations that have\n very few daily rides") +
  annotate("segment", y=20, yend=15, x=40, xend=10, color="Orange", arrow=arrow(length=unit(0.05,"npc"))) +
  annotate("segment", y=24, yend=30, x=40, xend=8, color="Orange", arrow=arrow(length=unit(0.05,"npc"))), nrow=1)


# Model Results

# After running different combinations of variables, the best model (that yields the lowest RMSE) used starting station with regularization on region/day of week, temperature, month, year, and ciclavia event.  Here is the breakout of each iteration and the RMSEs. 


## final view

rmse_results <- rbind(RMSE_avg,
                      RMSE_station,
                      RMSE_wdayregion,
                      RMSE_month,
                      RMSE_year,
                      RMSE_cicla,
                      RMSE_reg
                      )
rownames(rmse_results) <- c('Overall Average'
                          ,"Added Station"
                          ,"Added Day of Week & Region"
                          ,"Added Month"
                          ,"Added Year"
                          ,"Added Ciclavia Events"
                          ,"Added Regularization"
                           )

colnames(rmse_results) <- "RMSE Results"
kable(rmse_results) %>% kable_styling(full_width=F)
## plot
data.frame(step = c(1:length(rmse_results)), 
           name = row.names(rmse_results), 
           RMSE = round(rmse_results[,1], 3)) %>%
  ggplot(aes(x=step, y=RMSE)) + geom_path(color="pink", alpha=0.75, linejoin="bevel") + geom_text(aes(label = name)) + 
  geom_text(aes(label=round(RMSE,5), color=RMSE), vjust=2) + scale_colour_gradient(low = "dark green", high = "red") + scale_y_log10() + ylab("RMSE (log-scale)") +
  transition_manual(step, cumulative = T) + 
  ggtitle("Final RMSE Results") + scale_x_continuous(limits = c(-.5,9)) + theme(legend.position = "none")


# <!-- The visualization below shows where we are most accurate/inaccurate, using the proportional residual calculation: -->

m <- map2 %>% ggmap()
m + geom_point(data = (
  residd3 %>% group_by(start_region, start_lat, start_lon) %>% summarise(n=sum(n), resid = sum(abs(resid))) %>% 
mutate(resid_to_n = abs(resid/n))
), aes(y=start_lat, x=start_lon, fill = start_region, size=(resid_to_n)), shape = 22)




# Conclusion
# After incrementing new effects into the model, we found that adding regularization helped improve the Root Mean Square Error for variables other than Station.  This was a pretty difficult model to build since I realized that the data set provided by Bike Share didn't have all the crucial variables need to create an effective predictive model.  Rather, we were only given the usage stats so we had to inspect and find outside variables that could impact the prediciton in a positive way.  In this case, we found temperature and anomalous events such as Ciclavia to help with the prediction.  This is probably true of any data set based on user usage of transportation.  It is usually necessary to look to outside factors such as holidays, natural disasters, etc.

# There are several limitation to consider with this model:

# **Bike availability** \n
# - The bike share program has station throughout the city and have a finite number of bikes that could fit in each.  There's always a chance that demand (people who want to ride) outweighs the actual supply.  There was no measurement of this factor in this data set and I'm not sure it actually exists in the real-world.\n
# - Furthermore, the number of rides are limited by how many bikes are put in that station on any given day.  I am unaware of what sort of guidance the program uses to load bikes in each station, but if that could misinform the true level of riding demand.\n
# - The model did not factor in where bike rides ended.  This will also impact the number of bikes available, hence skewing the historical data.\n
# \n **Other considerations**
# - Future Ciclavia events could be used to forecast ride demand, but we would need to make an educated guess as to which region will be impacted.  There were several events that did not have a substantial impact on any of the stations; we could use historical performance to estimate such as Heart of LA always impacts DTLA.  This is more of an art than a science.
# - Other city events such as marathons, holiday, and natural disasters were not included.  Natural disasters generally cannot be predicted anyway, so would not work in this model regardless.

# **Thanks for reading!**  
# **Feel free to check out my [Tableau Portfolio](https://public.tableau.com/profile/daniel.romotsky#!/vizhome/DanielRomotsky-Portfolio2/Portfolio) or add me on [LinkedIn](https://www.linkedin.com/in/daniel-romotsky/)!**
