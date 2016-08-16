require("rgdal")
require("rgeos")
require("maptools")
require("sp")
require("data.table")

carcass.raw.data <- as.data.table(read.delim("data/Asset_Vision_Pavement_Cleaning.csv", header=T, sep=","))

carcass.data <- carcass.raw.data[Comments %like% "roo" & Longitude != "" & Latitude != "", .(Created_Date,Longitude,Latitude)]

coordinates(carcass.data) <- c("Longitude", "Latitude")
proj4string(carcass.data) <- CRS("+init=epsg:4326") # WGS84
CRS.mga9455 <- CRS("+init=epsg:28355") # MGA9455
carcass.data.mga9455 <- spTransform(carcass.data, CRS.mga9455)

writeOGR(carcass.data.mga9455, "data/", "VIC_GDA9455_FAUNA_EGK_WESTERNHWY", driver="ESRI Shapefile")
