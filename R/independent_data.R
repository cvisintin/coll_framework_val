require(data.table)
require(RPostgreSQL)
require(raster)

drv <- dbDriver("PostgreSQL")  #Specify a driver for postgreSQL type database
con <- dbConnect(drv, dbname="qaeco_spatial", user="qaeco", password="Qpostgres15", host="boab.qaeco.com", port="5432")  #Connection to database server on Boab

roads <- as.data.table(dbGetQuery(con,"
  SELECT
                                  r.uid as uid, ST_X(r.geom) AS x, ST_Y(r.geom) AS y
                                  FROM
                                  (SELECT
                                  uid, ST_ClosestPoint(geom, ST_Centroid(geom)) AS geom
                                  FROM
                                  gis_victoria.vic_gda9455_roads_state) AS r
                                  "))
setkey(roads,uid)

tvol.preds <- as.data.table(read.csv("data/vic_tvol_preds_rf.csv"))  #Read in collision data training set (presences/absences of collisions and covariates)

tspd.preds <- as.data.table(read.csv("data/vic_tspd_preds_rf.csv"))  #Read in collision data training set (presences/absences of collisions and covariates)

cov.data <- Reduce(function(x, y) merge(x, y, all=TRUE), list(roads,tvol.preds,tspd.preds))

sdm.preds <- raster("data/egk_preds_brt.tif")

cov.data$egk <- raster::extract(sdm.preds,cov.data[,.(x,y)])

cov.data$coll <- as.integer(0)

coll_bendigo <- as.data.table(dbGetQuery(con,"
                                   SELECT DISTINCT ON (p.id)
                                    r.uid AS uid, CAST(1 AS INTEGER) AS coll
                                   FROM
                                    gis_victoria.vic_gda9455_roads_state as r,
                                    (SELECT DISTINCT ON (geom)
                                      id, geom
                                    FROM
                                      gis_victoria.vic_gda9455_fauna_egk_bendigo) AS p
                                    WHERE ST_DWithin(p.geom,r.geom,300)
                                   ORDER BY p.id, ST_Distance(p.geom,r.geom)
                                   "))
setkey(coll_bendigo,uid)

data1 <- copy(coll_bendigo)
data <- copy(cov.data)
data[data1, coll := i.coll]
data <- na.omit(data)
data <- data[!duplicated(data[,.(x,y)]),]
write.csv(data, file = "data/model_data_bendigo.csv", row.names=FALSE)


coll_western <- as.data.table(dbGetQuery(con,"
                                   SELECT DISTINCT ON (p.id)
                                    r.uid AS uid, CAST(1 AS INTEGER) AS coll
                                   FROM
                                    gis_victoria.vic_gda9455_roads_state as r,
                                    (SELECT DISTINCT ON (geom)
                                      id, geom
                                    FROM
                                      gis_victoria.vic_gda9455_fauna_egk_westerndist) AS p
                                    WHERE ST_DWithin(p.geom,r.geom,300)
                                   ORDER BY p.id, ST_Distance(p.geom,r.geom)
                                   "))
setkey(coll_western,uid)

data1 <- copy(coll_western)
data <- copy(cov.data)
data[data1, coll := i.coll]
data <- na.omit(data)
data <- data[!duplicated(data[,.(x,y)]),]
write.csv(data, file = "data/model_data_western.csv", row.names=FALSE)


coll_crashstats <- as.data.table(dbGetQuery(con,"
                                   SELECT DISTINCT ON (p.id)
                                    r.uid AS uid, CAST(1 AS INTEGER) AS coll
                                   FROM
                                    gis_victoria.vic_gda9455_roads_state as r,
                                    (SELECT DISTINCT ON (geom)
                                      id, geom
                                    FROM
                                      gis_victoria.vic_gda9455_fauna_egkcoll_crashstats) AS p
                                    WHERE ST_DWithin(p.geom,r.geom,300)
                                   ORDER BY p.id, ST_Distance(p.geom,r.geom)
                                   "))
setkey(coll_crashstats,uid)

data1 <- copy(coll_crashstats)
data <- copy(cov.data)
data[data1, coll := i.coll]
data <- na.omit(data)
data <- data[!duplicated(data[,.(x,y)]),]
write.csv(data, file = "data/model_data_crashstats.csv", row.names=FALSE)