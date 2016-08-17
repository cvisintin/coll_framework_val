require(data.table)
require(geosphere)
require(rgeos)
require(maptools)
require(sp)
require(rgdal)
require(raster)
require(RPostgreSQL)

drv <- dbDriver("PostgreSQL")  #Specify a driver for postgreSQL type database
con <- dbConnect(drv, dbname="qaeco_spatial", user="qaeco", password="Qpostgres15", host="boab.qaeco.com", port="5432")  #Connection to database server on Boab

options(stringsAsFactors=FALSE)

#Read in data
caltrans <- read.csv("data/Caltrans_Carcass_Data.csv")
pm2012 <- readOGR("data/2012pm01", "2012pm01")

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


#Build clean carcass data.table
carcass <- data.table("ID"=as.numeric(caltrans$ID), "ROUTE"=as.numeric(caltrans$Route), "COUNTY"=toupper(as.character(caltrans$County)), "PM"=as.numeric(caltrans$PostmileR), "LON"=as.numeric(caltrans$x_coord), "LAT"=as.numeric(caltrans$y_coord))
setkeyv(carcass, c("ID","ROUTE","COUNTY","PM"))
write.table(carcass, file = "data/carcass.csv", row.names=FALSE, col.names=TRUE, sep=",")

#Build clean postmile data.table
postmile <- data.table("ROUTE"=as.numeric(as.character(pm2012$ROUTE)), "COUNTY"=toupper(as.character(pm2012$CTY)), "PM"=as.numeric(as.character(pm2012$PB)), "LON"=as.numeric(pm2012@coords[,1]), "LAT"=as.numeric(pm2012@coords[,2]))
setkeyv(postmile, c("ROUTE","COUNTY","PM"))
write.table(postmile[!duplicated(postmile[,.(COUNTY,ROUTE,PM)])], file = "data/postmile.csv", row.names=FALSE, col.names=TRUE, sep=",")

#Combine records by matching ROUTE, COUNTY, and PM
compare <- merge(carcass, postmile[!duplicated(postmile[,.(COUNTY,ROUTE,PM)])], by=c("ROUTE","COUNTY","PM"))

#Calculate distance discrepancy between reported and matched coordinates
compare$dist.delta <- pointDistance(compare[,.(LON.y,LAT.y)], compare[,.(LON.x,LAT.x)],longlat=T)*0.001

# #Which are duplicated IDs?
# x <- compare[duplicated(compare, by="ID.x"),]
# 
# #Unique IDs matched to postmile
# final <- unique(compare, by="ID.x")
# setnames(final,4,"ID")
# setkey(final,ID)

#Merge corrected coordinates with Caltrans data
export <- merge(caltrans, compare[,.(ID,"X"=LON.y,"Y"=LAT.y)], by=c("ID"), all=FALSE)
write.table(export, file = "data/caltrans_cor.csv", row.names=FALSE, col.names=TRUE, sep=",")

#Reproject coordinates to NAD83 zone 10
coordinates(export) <- c("X", "Y")
proj4string(export) <- CRS("+init=epsg:4326") # WGS84
CRS.nad8310 <- CRS("+init=epsg:3157") # NAD8310
carcass.data.nad8310 <- spTransform(export, CRS.nad8310)

writeOGR(carcass.data.nad8310[,c(1,6,10,13,15,32,17)], "data/", "CAL_NAD8310_FAUNA_CARCASS_CALTRANS", driver="ESRI Shapefile", overwrite=TRUE)

system("shp2pgsql -D -d -I -s 28355 data/CAL_NAD8310_FAUNA_CARCASS_CALTRANS.shp gis_california.cal_nad8310_fauna_caltrans2 | PGPASSWORD=Qpostgres15 psql -d qaeco_spatial -h boab.qaeco.com -p 5432 -U qaeco -w")

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
abline(a=0, b=1, lty=2, col="red")
