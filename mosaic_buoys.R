# custom install: https://github.com/MikkoVihtakari/PlotSvalbard#installation 
# requires additional libraries such as ggplot2, dplyr, please install as suggested (install.packages('packagename')
library(PlotSvalbard)
library(curl)

#install.packages('gifski')
#install.packages('png')
library(gganimate)
library(png)
library(gifsky)
library(transformr) #install.packages('transformr')
###############################
# START DOWNLOAD # files saved current working dir
###############################
# find polarstern coordinates and convert to decimal degree lon/lats, enter as LAT,LON below
polarstern_coord <- c(84.9,135.3)
# set page URL, output will be HTML, fun stuff getting download urls..
url = "https://data.meereisportal.de/download/buoys/"
# manually set wanted buoys
wanted_buoys <- c("2019P152","2019P156","2019P188","2019P190","2019P192","2019P194",
                  "2019P198","2019P203","2019P155","2019P157","2019P189","2019P191",
                  "2019P193","2019P196","2019P200","2019P206")

h = new_handle(dirlistonly=TRUE)
con = curl(url, "r", h)
tbl = readLines(con)
close(con)
head(tbl)

# take lines with 2019P*_proc.csv files
wanted <- tbl[grepl("2019P.*._proc\\.csv", tbl)] 
# remove html tags, replace by ,
wantedcsv <- strsplit(gsub("<.*?>", "\\,", wanted),",") 

# get appropiate columns out of 2 dimension matrix (filename, modify date)
wantedcsvdf <- NULL 
for(i in 1:length(wantedcsv)) {
  wantedcsvdf <- rbind(wantedcsvdf, wantedcsv[[i]][c(7,10)])
}

# select files todays modify date
files_to_download <- wantedcsvdf[which(wantedcsvdf[,2] == Sys.Date()),1] 
# take wanted buoys
files_to_download <- files_to_download[grepl(paste(wanted_buoys, collapse="|"),files_to_download)] 
#download..
lapply(files_to_download, function(x) curl_download(paste0(url,x), x))

#cleanup
rm(h, con, tbl, wanted, wantedcsv, wantedcsvdf,url, i)
###############################
# END DOWNLOAD #
###############################
#
###############################
# DATA READ & PLOT #
###############################
files_to_download <-list.files(pattern ='.csv')
# learning to properly do this.. rbind.fill requires plyr(installed as part of ggplot2/plotsvalbard),
# needed to bind data frame with lacking columns
library(plyr)
data <- do.call(rbind.fill, lapply(files_to_download, function(x) { 
  data <- read.csv(x)
  # substring first 8 characters of filename
  buoy <-rep(substr(x,1,8),nrow(data))
  # bind buoy name to data frame and return
  cbind(buoy, data)
}))
library(dplyr)

# convert dateimte to POSIXlt (R default datetime format)
data$time <- strptime(data$time,"%Y-%m-%dT%H:%M:%S")

# create UTM coords, results in meters measurement
data <- cbind(data,transform_coord(lon = data$longitude..deg., lat = data$latitude..deg., new.names = c("lon.utm", "lat.utm"), 
                                   proj.og = "+proj=longlat +datum=WGS84", 
                                   proj.out = "+proj=stere +lat_0=90 +lat_ts=0 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", 
                                   map.type = 'panarctic', 
                                   verbose = FALSE, 
                                   bind = F))
polarstern_coord <- transform_coord(lon = polarstern_coord[2], lat = polarstern_coord[1], new.names = c("lon.utm", "lat.utm"), 
                                    proj.og = "+proj=longlat +datum=WGS84", 
                                    proj.out = "+proj=stere +lat_0=90 +lat_ts=0 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", 
                                    map.type = 'panarctic', 
                                    verbose = FALSE, 
                                    bind = F)

limits <- c(
  min(data$lon.utm) - .5*(max(data$lon.utm) - min(data$lon.utm)),
  max(data$lon.utm) + .5*(max(data$lon.utm) - min(data$lon.utm)),
  min(data$lat.utm) - 5*(max(data$lat.utm) - min(data$lat.utm)),
  max(data$lat.utm) + 5*(max(data$lat.utm) - min(data$lat.utm))
)


#remove error high speeds & NA
data <- data[which(data$drift_speed..m.s. < .5),]
#
plot <- basemap("panarctic", bathymetry = F, bathy.style = "poly_greys", limits = limits) +
  geom_point(data = data, aes(x = lon.utm, y = lat.utm, color = drift_speed..m.s., group = buoy), size = 2  ) +  
  scale_color_viridis_c(name = "Drift Speed (m/s)" ) +
  geom_path(data = data, aes(x = lon.utm, y = lat.utm, color = drift_speed..m.s., group = buoy )) +
  # Here comes the gganimate specific bits
  transition_reveal(as.POSIXct(time)) +
  labs(title = "MOSAiC Buoys 27 Sept - 11 Oct 2019"  )


anim_save("MOSAiC Buoys 27 Sept - 11 Oct 2019.gif", plot)
