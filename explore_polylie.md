---
title: "Take a look at the polylines"
output: html_document
---



```r
library(stargazer)
MAIN_DIRECTORY <- "~/Kaggle/K06012015"
```



```r
setwd(MAIN_DIRECTORY)
library(data.table)
```

```
## data.table 1.9.4  For help type: ?data.table
## *** NB: by=.EACHI is now explicit. See README to restore previous behaviour.
```

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:data.table':
## 
##     between, last
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(tidyr)
train_data <- fread(input = "data/train.csv/train.csv")
```

```
## Read 0.0% of 1710670 rowsRead 17.0% of 1710670 rowsRead 32.2% of 1710670 rowsRead 46.2% of 1710670 rowsRead 59.6% of 1710670 rowsRead 72.5% of 1710670 rowsRead 81.3% of 1710670 rowsRead 85.9% of 1710670 rowsRead 90.6% of 1710670 rowsRead 95.9% of 1710670 rowsRead 1710670 rows and 9 (of 9) columns from 1.809 GB file in 00:00:25
```


```r
library(OpenStreetMap)
```

```
## Loading required package: rJava
## Loading required package: raster
## Loading required package: sp
## 
## Attaching package: 'raster'
## 
## The following object is masked from 'package:tidyr':
## 
##     extract
## 
## The following object is masked from 'package:dplyr':
## 
##     select
## 
## Loading required package: rgdal
## rgdal: version: 0.9-2, (SVN revision 526)
## Geospatial Data Abstraction Library extensions to R successfully loaded
## Loaded GDAL runtime: GDAL 1.11.2, released 2015/02/10
## Path to GDAL shared files: C:/Users/Alexander/Documents/R/win-library/3.1/rgdal/gdal
## GDAL does not use iconv for recoding strings.
## Loaded PROJ.4 runtime: Rel. 4.9.1, 04 March 2015, [PJ_VERSION: 491]
## Path to PROJ.4 shared files: C:/Users/Alexander/Documents/R/win-library/3.1/rgdal/proj
```

```r
library(ggmap)
```

```
## Loading required package: ggplot2
## Google Maps API Terms of Service: http://developers.google.com/maps/terms.
## Please cite ggmap if you use it: see citation('ggmap') for details.
```

```r
library(readr)
library(rjson)

#Load street map of Portugal
map <- get_openstreetmap(
  bbox =  c(left = -8.70, bottom = 41.10, right = -8.40, top = 41.30), 
  scale = OSM_scale_lookup(zoom=12)
  )

#Sample a trip
trip.sample <- train_data %>% dplyr::select(TRIP_ID) %>% sample_n(size = 4)
trip <- train_data %>% dplyr::filter(TRIP_ID %in% trip.sample$TRIP_ID)

#Define core function for MapReduce/split-apply-combine
trip.function <- function(row) {
  #Extract Polyline and convert to columns
  polyline <- as.data.frame(do.call(rbind, fromJSON(row$POLYLINE)))
  #Format new data frame
  names(polyline) <- c("long", "lat")
  df <- data.frame(long = polyline[1], 
                   lat = polyline[2])
  number_poly <- nrow(df)
  df$TRIP_ID <- row$TRIP_ID
  df$PRIMARY_ID <- paste(df$TRIP_ID, 1:number_poly, sep = '_')
  df$CALL_TYPE <- row$CALL_TYPE
  df$ORIGIN_CALL <- row$ORIGIN_CALL
  df$ORIGIN_STAND <- row$ORIGIN_STAND
  df$TAXI_ID <- row$TAXI_ID
  df$TIMESTAMP <- row$TIMESTAMP
  df$DAY_TYPE <- row$DAY_TYPE
  df$MISSING_DATA <- row$MISSING_DATA
  df$STATUS <- c("Start", rep("During Trip", times = number_poly - 2), "Destination")
  df
}
#Run MapReduce
trip.splitkey <- trip$TRIP_ID
trip.split <- split(trip, f = trip.splitkey)
trip.apply <- lapply(trip.split, FUN = trip.function)
trip.combine <- Reduce('rbind', trip.apply)

png(filename = "a_few_sample_trips.png")
ggmap(map) +
     geom_point(data=trip.combine, aes(x=long, y=lat, color=STATUS, size = I(STATUS == 'During Trip'))) +
     scale_color_manual(values=c("blue", "#00CC00", "red")) +
     scale_size_manual(values=c(5, 2))+
  theme(legend.position = 'None')
dev.off()
```

```
## png 
##   2
```
