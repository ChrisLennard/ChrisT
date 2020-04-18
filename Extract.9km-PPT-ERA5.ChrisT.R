##From the LAT-LON file sent by Nick, Using ERA5 data, compute SPI for each lat-lon coordinate 

##Where are we working
##CHANGE THIS
setwd("./")

##Packages to install
#install.packages("tidyverse")
library("stringr")
library(RNetCDF)
library(ncdf4)
library(ncdf.tools)
library(tools)
library(lattice)
library(chron)
library(RColorBrewer)

#0.Where are the data
ERAindir <- "./ERA5" ##9km ERA5 data
outdir <- "./OutData"

##Nicks table with the corrected Lat-lons
corlatlondir <- "./AfroBarom"
corlatlonfile <- "Lat-lon.Nick.csv"
file <- paste(corlatlondir,corlatlonfile,sep = "/")


##3. Read in the Afrobarom station coordinates
  print(paste("Reading in the station data for",file,sep=" "))
  stn <-read.csv(file, header = FALSE, sep=",")
  ##Convert to a data frame
  stn.df <- as.data.frame(stn, header = TRUE)

##4. Read in the ERA netcdf 9km data
  fnametest <- "ERA5.Jan2008.9km.nc" ##9km ERA5 data; units already in mm/month
  eravartest <- "pr"
  ncin.test <- nc_open(paste(ERAindir, fnametest,sep="/"))

##5. Plotting to check data is sane
  #Get dimensions
  eralon <- ncvar_get(ncin.test,"longitude")
  eralat <- ncvar_get(ncin.test,"latitude")
  nlonera <- dim(eralon)
  nlatera <- dim(eralat)
  ##TIME
  time <- ncvar_get(ncin.test,"time") 
  tunits <- ncatt_get(ncin.test,"time","units")
  nt <- dim(time)
  ##PPT DATA
  ppt_array <- ncvar_get(ncin.test,eravartest)
  dim(ppt_array)
  length(ppt_array)
  fillvalue <- ncatt_get(ncin.test,eravartest,"_FillValue") 
  # replace netCDF fill_values with NA's
  #ppt_array[ppt_array==fillvalue$value] <- NA ##There are no Fillvalue grids
  #Checking for how many non-NA data there are
  num.nonNAs <- length(na.omit(as.vector(ppt_array[,])))
  dim(ppt_array)
  dim(eralon)
  dim(eralat)
  
  # levelplot of the slice
  grid <- expand.grid(eralon=eralon, eralat=eralat)
  cutpts <- c(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30)
  ?levelplot(ppt_array ~ eralon * eralat, data=grid, at=cutpts, cuts=16, pretty=T, 
            col.regions=((brewer.pal(9,"RdYlBu"))))  
  
##6. Looping through the rows in the Afrobaraomter data
  #for(row in 1:nrow(stn.df)) {
  #print("STARTING LOOP THROUGH ROWS")
  row <- 1 
#  row <- 592 ##....this is the first NA point
    id <- as.character(stn.df[row, "V1"]) 
    lat <- as.numeric(as.character(stn.df[row, "V2"]))
    lon <- as.numeric(as.character(stn.df[row, "V3"]))
  
#Here I work out the X-Y coordinate of the lat-lon and extract teh data for that x-y coordinate
    era.latlontest <- ncvar_get(ncin.test, eravartest,
                          start= c(which.min(abs(ncin.test$dim$longitude$vals - lon)), # compute the closest long - X dim
                                   which.min(abs(ncin.test$dim$latitude$vals - lat)),  # compute the closest lat - Y dim
                                   1), # Z dim
                          count = c(1,1,-1)  # T dim: count '-1' means 'all values along that dimension'that dimension'
  )

#CHRIS: If you look at "era.latlontest" you will see 1 value which is rainfall. If you do the same thing for row 592 you will get a NA.

#What are the X-Y coordinates used above..  
#  longitude size 1001 (dim X)
  xdim <- which.min(abs(ncin.test$dim$longitude$vals - lon))
#  latitude size:891; (dim Y)
  ydim <- which.min(abs(ncin.test$dim$latitude$vals - lat))
  xyzdim <- c(xdim,ydim,1)
 
#}  
  
  
