getwd()


library(hotspots)
library(spatstat)
library(stats)
library(rgdal)
library(plyr)
library(rgeos)
library(RColorBrewer)
library(raster)
library(maptools)
library(psych)
library(VGAM)
setwd("C:/Users/KT/Dropbox/R_data")
caltrans <- read.csv("DBFFlatFauna Query1.csv")
head(caltrans)
table(caltrans$District)
set_ll_warn(TRUE)

setwd("C:/Users/KT/Dropbox/Arc_GIS_data")
pm2012 <- readOGR("2012pm01", "2012pm01")

#rounding issue in caltrans data 
caltrans$PostmileR <- round(caltrans$Postmile, 1)
#factors
pm2012$PB <- as.numeric(as.character(pm2012$PB))

re <- as.data.frame( table(pm2012$ROUTE, pm2012$PB)) #by county as well?

df3 <- merge(caltrans, pm2012, 
             by.x=c("Route", "County","PostmileR"), 
             by.y=c("ROUTE", "CTY","PB")) 

fs <- df3[duplicated(df3$ID),] #717 duplicates 
#None of the coords match whatsoever. 


#Casey's working code....
library("data.table")
library("geosphere")
library("dismo")
library("sp")

options(stringsAsFactors=FALSE)

#Read in data
caltrans <- read.csv("DBFFlatFauna Query1.csv")
pm2012 <- readOGR("2012pm01", "2012pm01")

#Round postmile values per Kate
caltrans$PostmileR <- round(caltrans$Postmile, 1)
#pm2012$PBR <- round(pm2012$PB, 1) - perhaps pm2012 markers as well???  Will create duplicates...

#Examine data contents
unique(sort(pm2012$CTY))
unique(sort(caltrans$County))

unique(sort(pm2012$ROUTE))
unique(sort(caltrans$Route))

unique(sort(pm2012$PB))
unique(sort(caltrans$PostmileR))

#Set keys for data.tables
keycols = c("ID","ROUTE","COUNTY","PM")

#Build clean carcass data.table and create IDs
carcass <- data.table("ID"=as.numeric(caltrans$ID), "ROUTE"=as.numeric(caltrans$Route), "COUNTY"=toupper(as.character(caltrans$County)), "PM"=as.numeric(caltrans$PostmileR), "LON"=as.numeric(caltrans$x_coord), "LAT"=as.numeric(caltrans$y_coord))
setkeyv(carcass, keycols)
write.table(carcass, file = "carcass.csv", row.names=FALSE, col.names=TRUE, sep=",")

#Build clean postmile data.table and create IDs
postmile <- data.table("ID"=seq(1,nrow(pm2012@data),1), "ROUTE"=as.numeric(as.character(pm2012$ROUTE)), "COUNTY"=toupper(as.character(pm2012$CTY)), "PM"=as.numeric(as.character(pm2012$PB)), "LON"=as.numeric(pm2012@coords[,1]), "LAT"=as.numeric(pm2012@coords[,2]))
setkeyv(postmile, keycols)
write.table(postmile, file = "postmile.csv", row.names=FALSE, col.names=TRUE, sep=",")

#Combine records by matching ROUTE, COUNTY, and PM
compare <- merge(carcass, postmile, by=c("ROUTE","COUNTY","PM"), all=FALSE)

#Calculate distance discrepancy between reported and matched coordinates
compare$dist.delta <- pointDistance(compare[,.(LON.y,LAT.y)], compare[,.(LON.x,LAT.x)],longlat=T)*0.001

#Which are duplicated IDs?
x <- compare[duplicated(compare, by="ID.x"),]

#Unique IDs matched to postmile
final <- unique(compare, by="ID.x")
setnames(final,4,"ID")
setkey(final,ID)

#Merge corrected coordinates with Caltrans data
export <- merge(caltrans, final[,.(ID,LON.y,LAT.y)], by=c("ID"), all=FALSE)
write.table(export, file = "caltrans_cor.csv", row.names=FALSE, col.names=TRUE, sep=",")

#What is range of distance discrepancy in meters for locations with reported coordinates?
range(compare[LON.x!=0 & LON.y!=0,dist.delta])

#How many duplicates of coordinate discrepancies?
nrow(compare[LON.x!=0 & LON.y!=0,.N,by=dist.delta])
nrow(compare[LON.x!=0 & LON.y!=0,.N,by=.(LON.x,LON.y)]) #How many unique coordinate groups?  Should match unique dist.delta for systematic error...otherwise could be human error

#Which records cannot be matched to caltrans postmiles?
nomatch <- carcass[!compare]

#Test linear relationship between postmiles marks and actual distances along route - LA county route 1 as example
pts <- postmile[COUNTY=="LA" & ROUTE==1,.(LON,LAT)]
dists <- pointDistance(pts, longlat=T)

#Plot linear relationship
plot(dists[,1]*0.000621371, postmile[COUNTY=="LA" & ROUTE==1,PM], type="l", xlab="DISTANCE ALONG ROUTE (MILES)", ylab="POSTMILE MARK")