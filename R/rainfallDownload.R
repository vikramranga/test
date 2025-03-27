#
##
### Rainfall data download and extraction at GP level
##
#

## Libraries first
library(pacman)
library(reticulate)
reticulate::conda_list()
use_python('C:/Users/vikram.ranga/.conda/envs/r-reticulate/python.exe')
use_condaenv(condaenv = 'r-reticulate')
p_load(raster, terra, sf, tidyverse)
# Import the imdlib module
imd <- import("imdlib")
start_dy = "2023-06-01"
end_dy = "2023-12-31"
var_type = 'rain'
file_dir='D:/laptop data backup/d/Agri/Datasets/IMD/rain_realtime_23'
data = imd$get_real_data(var_type, start_dy, end_dy, file_dir)
data$to_netcdf(paste(start_dy, "To", end_dy, ".nc", sep = ""), file_dir)
source("D:/laptop data backup/d/Agri/Analysis - Scripts/R Scripts/IMD/IMD_functions.r")
crntRain <- rast(paste(file_dir, "/",start_dy, "To", end_dy, ".nc", sep = ""))
crntRain[crntRain == -999] <- NA
wbState <- st_read("D:/laptop data backup/d/Agri/Datasets/GIS datasets/India latest/India-State-and-Country-Shapefile-Updated-Jan-2020-master/India_State_Boundary.shp")
wbState <- wbState |> 
  filter(State_Name == "West Bengal")

rainJunetoDec23 <- terra::extract(crntRain, wbState, 
                                  fun = 'mean', 
                                  na.rm = TRUE,
                                  bind = TRUE)
rainJunetoDec23 <- st_as_sf(rainJunetoDec23)

a <- rainJunetoDec23 |> 
  st_drop_geometry() 
dateSeq <- seq(as.Date("2023-06-01"),
               as.Date("2023-12-31"),
               by = "days")
colnames(a) <- c("State", dateSeq)
a <- a |> 
  t()
a <- a[2:215,]
a <- data.frame(a)
b <- rownames_to_column(a)
b$Date <- lubridate::date(dateSeq)
b$State <- "West Bengal"
P1 <- b |> 
  mutate(a = base::round(as.numeric(a), 1)) |> 
  ggplot() +
  geom_line(aes(x = Date,
                y = a,
                group = State)) + 
  geom_point(aes(x = Date,
                 y = a,
                 group = State)) +
  scale_x_date(date_label = "%m/%d", date_breaks = '2 week') +
  theme_bw()
