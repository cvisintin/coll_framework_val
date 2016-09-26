require(rgdal)
require(rgeos)
require(maptools)
require(sp)
require(data.table)
require(RPostgreSQL)

drv <- dbDriver("PostgreSQL")  #Specify a driver for postgreSQL type database
con <- dbConnect(drv, dbname="qaeco_spatial", user="qaeco", password="Qpostgres15", host="boab.qaeco.com", port="5432")  #Connection to database server on Boab

carcass.raw.data <- as.data.table(read.delim("data/Asset_Vision_Pavement_Cleaning.csv", header=T, sep=","))

carcass.data <- carcass.raw.data[Comments %like% "roo" & Longitude != "" & Latitude != "", .(Created_Date,Longitude,Latitude)]

coordinates(carcass.data) <- c("Longitude", "Latitude")
proj4string(carcass.data) <- CRS("+init=epsg:4326") # WGS84
CRS.mga9455 <- CRS("+init=epsg:28355") # MGA9455
carcass.data.mga9455 <- spTransform(carcass.data, CRS.mga9455)

writeOGR(carcass.data.mga9455, "data/", "VIC_GDA9455_FAUNA_EGK_WESTERNHWY", driver="ESRI Shapefile")

writeOGR(carcass.data.mga9455,
         driver = "PostgreSQL",
         "PG:dbname=qaeco_spatial user=qaeco password=Qpostgres15 host=boab.qaeco.com port=5432",
         layer = "gis_victoria.vic_gda9455_fauna_egk_westerndist",
         layer_options = c("GEOMETRY_NAME=geom","FID=id"),
         overwrite_layer=TRUE)

dbGetQuery(con,"select UpdateGeometrySRID('gis_victoria', 'vic_gda9455_fauna_egk_westerndist', 'geom', 28355);")
