require(data.table)
require(doMC)
require(maptools)
require(reshape2)
require(rgeos)
require(RPostgreSQL)
require(raster)
require(MASS)
require(vcd)
require(rethinking)
require(lme4)
require(arm)
source("R/R2glmm.R")

drv <- dbDriver("PostgreSQL")  #Specify a driver for postgreSQL type database
con <- dbConnect(drv, dbname="qaeco_spatial", user="qaeco", password="Qpostgres15", host="boab.qaeco.com", port="5432")  #Connection to database server on Boab

deer_risk.rds <- as.data.table(dbGetQuery(con,"
       SELECT 
        r.uid, ST_Length(r.geom)/1000 AS rdlength, r.collrisk AS collrisk
       FROM
        (SELECT
          x.uid AS uid, x.geom AS geom, y.collrisk AS collrisk
        FROM
          gis_california.cal_nad8310_roads_study AS x, gis_california.cal_nogeom_roads_deercollrisk AS y
        WHERE
          x.uid = y.uid) AS r, gis_california.cal_nad8310_admin_study_area AS p
       WHERE
        ST_Contains(p.geom, r.geom);
    "))
setkey(deer_risk.rds,uid)

# dbGetQuery(con,"
#   CREATE TABLE gis_victoria.vic_gda9455_fauna_egkcoll_western_onnetwork (id serial, date character varying, x double precision, y double precision, distance double precision);
#   SELECT AddGeometryColumn('gis_victoria', 'vic_gda9455_fauna_egkcoll_western_onnetwork','geom',28355,'POINT',2);
#   ")
# dbGetQuery(con,"
#   INSERT INTO
#     gis_victoria.vic_gda9455_fauna_egkcoll_western_onnetwork (date, x, y, distance, geom)
#   SELECT DISTINCT ON (p.id)
#     p.crtd_dt AS date, ST_X(ST_AsText(ST_ClosestPoint(l.geom,p.geom))) AS x, ST_Y(ST_AsText(ST_ClosestPoint(l.geom,p.geom))) AS y, min(ST_Distance(l.geom, p.geom)) AS distance, ST_ClosestPoint(l.geom,p.geom) AS geom
#   FROM
#     gis_victoria.vic_gda9455_roads_state AS l, gis_victoria.vic_gda9455_fauna_egk_westerndist AS p
#   WHERE
#     ST_DWithin(l.geom, p.geom, 20.0)
#   GROUP BY
#     p.id, l.id
#   ORDER BY
#     p.id, distance
#   ")

deer_coll.rds <- as.data.table(dbGetQuery(con,"
    SELECT
      r.uid AS uid, COUNT(p.id) AS ncoll
    FROM
        (SELECT
          uid, geom
        FROM
          gis_california.cal_nad8310_roads_study) AS r, 
        (SELECT DISTINCT ON (p.id)
          p.id AS id, ST_ClosestPoint(x.geom, p.geom) AS geom
        FROM
          gis_california.cal_nad8310_roads_study AS x, gis_california.cal_nad8310_fauna_caltrans AS p
        WHERE
          ST_DWithin(x.geom, p.geom, 50)
        AND
          species = 'Deer'
        AND
          year >= 2000) AS p
    WHERE
      ST_DWithin(r.geom, p.geom, .0001)
    GROUP BY
      r.uid;
    "))
setkey(deer_coll.rds,uid)

val.data <- merge(deer_risk.rds,deer_coll.rds, by="uid", all.x=TRUE)
val.data$ncoll[is.na(val.data$ncoll)] <- 0

val.data <- na.omit(val.data)

distplot(val.data$ncoll, type="poisson")
distplot(val.data$ncoll, type="nbinomial")

#model <- glm(formula=ncoll~log(collrisk) + log(rdlength), data=val.data, family=poisson)

model <- glm(formula=ncoll ~ log(collrisk), data=val.data, family=poisson)

summary(model)

paste("% Deviance Explained: ",round(((model$null.deviance - model$deviance)/model$null.deviance)*100,2),sep="")  #Report reduction in deviance

model.nb <- glm.nb(formula=ncoll ~ log(collrisk), data=val.data)
summary(model.nb)
paste("% Deviance Explained: ",round(((model.nb$null.deviance - model.nb$deviance)/model.nb$null.deviance)*100,2),sep="")  #Report reduction in deviance

plot(val.data$ncoll, exp(model.nb$coefficients[1]+model.nb$coefficients[2]*log(val.data$collrisk)),xlab="Observed Number of Collisions",ylab="Expected Number of Collisions")
abline(a=0,b=1,lty=2,col="red")
