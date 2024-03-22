### Reading TMS sensors
## Bruno De Vos - March 2024


library(myClim)
library(ggplot2)
library(dplyr)


### sensor_ids
# TMS_T1 = Soil temperature
# TMS_T2 = Surface temperature
# TMS_T3 = Air temperature
# TMS_moist = soil moisture


### sensor metadata
senmeta<- read.csv("c:/data/fourcast/Sensor_metadata_TMS4.csv")
View(senmeta)

names(senmeta)

senmeta$serial_number<-substring(senmeta$SENSOR_ID,4,11)

# plotlijst-sensor
plotlist<-senmeta[, c(1,21)]
plotlist

# read TMS4 sensor data
setwd("C:/DATA/FOURCAST/TMS4")
 

# read raw data (1 datafile)
BF02.TMS<-read.csv2("BF02_TMS_data_94253011_2024_02_14_0.csv"); dim(BF02.TMS)
head(BF02.TMS)
tail(BF02.TMS)


ELSTIN.TMS<-read.csv2("ELST_IN_data_94253047_2024_02_09_0.csv"); dim(BF02.TMS)
head(ELSTIN.TMS)
tail(ELSTIN.TMS)


ELSTOUT.TMS<-read.csv2("ELST_OUT_data_94233781_2024_02_09_0.csv"); dim(BF02.TMS)
head(ELSTOUT.TMS)
tail(ELSTOUT.TMS)



## Read pre-defined loggers without metadata
# read from Tomst files
#tms <- mc_read_files(c("BF02_TMS_data_94253011_2024_02_14_0.csv", "ELST_IN_data_94253047_2024_02_09_0.csv",
#                         "ELST_OUT_data_94233781_2024_02_09_0.csv"),
#                       dataformat_name = "TOMST", silent = T)


# read all Tomst files from current directory
tms.d <- mc_read_files(".", dataformat_name = "TOMST", recursive = F, silent = F)




## crop the time-series - from general installation date to end of year
#start <- as.POSIXct("2023-05-15", tz = "UTC")
#end   <- as.POSIXct("2023-12-31", tz = "UTC")

#tms.d   <- mc_prep_crop(tms, start, end)

## just month october 2023
start <- as.POSIXct("2023-10-01", tz = "UTC")
end   <- as.POSIXct("2023-11-01", tz = "UTC")

tms.oct   <- mc_prep_crop(tms.d, start, end)


### filter data with mc_filter for specific locality and data

#tms.m    <- mc_filter(tms.d, localities = "94253011", sensors = c("TMS_T3")) # keep only two sensor
#tms.info  <- mc_info(tms.m) # see info 



## ---- Plotting ''--------------------------------------------------
names(mc_data_sensors)
#  ## lines
tms.plot <- mc_filter(tms.d, localities = "94253011")
#  plot all sensors
p <- mc_plot_line(tms.plot)
p <- p+ggplot2::scale_x_datetime(date_breaks = "1 week", date_labels = "%W")
p <- p+ggplot2::xlab("week")
p <- p+ggplot2::aes(size = sensor_name)
p <- p+ggplot2::scale_size_manual(values = c(1,1,1,1))
p <- p+ggplot2::guides(size = "none")
p <- p+ggplot2::scale_color_manual(values = c("brown", "yellow", "red", "blue"), name = NULL)
p  

# plot one sensor

#  plot all sensors
p <- mc_plot_line(tms.plot,sensors = c("TMS_T3"))
p <- p+ggplot2::scale_x_datetime(date_breaks = "1 week", date_labels = "%W")
p <- p+ggplot2::xlab("week")
p <- p+ggplot2::aes(size = sensor_name)
p <- p+ggplot2::scale_size_manual(values = c(1,1,1,1))
p <- p+ggplot2::guides(size = "none")
p <- p+ggplot2::scale_color_manual(values = c("red"), name = "Air temp")
p  


## raster
#  mc_plot_raster(tms.plot, sensors = c("TMS_T3"))
  

  
  
  
  
#### data aggregation per day, month  
  
## ----eval=TRUE,warning=F------------------------------------------------------
# with defaults only convert Raw-format  to Agg-format
#tms.ag <- mc_agg(tms.d,fun = NULL, period = NULL)

# aggregate to daily mean, range, coverage, and 95 percentile. 
#tms.day <- mc_agg(tms.f, fun = c("mean", "range", "coverage", "percentile"),
#                  percentiles = 95, period = "day", min_coverage = 0.95)

# aggregate to daily mean, range, coverage, and 95 percentile. 
#tms.month <- mc_agg(tms.oct, fun = c("mean", "range", "coverage", "percentile"),
#                  percentiles = 95, period = "month", min_coverage = 0.95)



# aggregate all time-series, return one value per sensor.
#tms.all <- mc_agg(tms, fun = c("mean", "range", "coverage", "percentile"),
#                  percentiles = 95, period = "all", min_coverage = 0.95)

# aggregate with your custom function. (how many records are below -5°C per month)
#tms.all.custom <- mc_agg(tms.out, fun = list(TMS_T3 = "below5"), period = "month",
#                         custom_functions = list(below5 = function(x){length(x[x<(-5)])}))
#r <- mc_reshape_long(tms.all.custom)



## ----eval=TRUE,warning=F------------------------------------------------------
## calculate virtual sensor VWC from raw TMS moisture signal
#tms.calc <- mc_calc_vwc(tms, soiltype = "loamy sand A")
tms.calc.oct <- mc_calc_vwc(tms.oct, soiltype = "universal")    # no soil specific calibration => test difference between universal and specific calibration


## virtual sensor with growing and freezing degree days
#tms.calc <- mc_calc_gdd(tms.calc, sensor = "TMS_T3",)
#tms.calc <- mc_calc_fdd(tms.calc, sensor = "TMS_T3")

## virtual sensor to estimate snow presence from 2 cm air temperature 
##tms.calc <- mc_calc_snow(tms.calc, sensor = "TMS_T2")

## summary data.frame of snow estimation
#tms.snow <- mc_calc_snow_agg(tms.calc)


## ----OUTPUT TABLE---------------------------------------------------------------

## wide table of air temperature and soil moisture
tms.wide <- mc_reshape_wide(tms.calc.oct, sensors = c("TMS_T3", "TMS_T1"))

## long table of air temperature and soil moisture
tms.long <- mc_reshape_long(tms.calc.oct, sensors = c("TMS_T3", "vwc"))




#### Output table IN LONG FORMAT
tms.long.oct<- mc_reshape_long(tms.calc.oct)

tms.long.oct.plt<-right_join(tms.long.oct,plotlist)

sort(unique(tms.long.oct.plt$PLOTNAAM))
sort(unique(tms.long.oct.plt$sensor_name))
sort(unique(tms.long.oct.plt$height))
names(tms.long.oct.plt)

tms.long.oct.out<-tms.long.oct.plt[ ,c(8,2:7)]

write.csv2(tms.long.oct.out, "C:/R/OUT/TMS/tms_long_oct_out.csv")

## filter for specific plot

tms.plotspec<-tms.long.oct.out[tms.long.oct.out$PLOTNAAM=="BF49", ]



par(mfrow = c(2, 2))
plot(tms.plotspec$value[tms.plotspec$height=="air 15 cm"],type="l", col="purple", ylab=c("Temperature °C"), main="Air temp at 15cm")
plot(tms.plotspec$value[tms.plotspec$height=="air 2 cm"],type="l", col="yellow", ylab=c("Temperature °C"),main="Air temp at 2 cm")
plot(tms.plotspec$value[tms.plotspec$sensor=="VWC_moisture"],type="l", col="blue",ylab=c("Soil moisture m³/m³"),main="Volumetric Soil moisture 0-10 cm")
plot(tms.plotspec$value[tms.plotspec$height=="soil 8 cm"],type="l", col="brown", ylab=c("Temperature °C"),main="Soil temperature at -8 cm")
