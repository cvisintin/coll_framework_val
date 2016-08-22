require(data.table)
require(doMC)
require(maptools)
require(reshape2)
require(rgeos)
require(RPostgreSQL)
require(raster)
require(MASS)
require(vcd)

drv <- dbDriver("PostgreSQL")  #Specify a driver for postgreSQL type database
con <- dbConnect(drv, dbname="qaeco_spatial", user="qaeco", password="Qpostgres15", host="boab.qaeco.com", port="5432")  #Connection to database server on Boab

egk_risk.rds <- as.data.table(dbGetQuery(con,"
       SELECT 
        r.uid, ST_Length(r.geom)/1000 AS rdlength, r.egkrisk AS collrisk
       FROM
        (SELECT
          x.uid AS uid, x.geom AS geom, y.egk AS egkrisk
        FROM
          gis_victoria.vic_gda9455_roads_state as x, gis_victoria.vic_nogeom_roads_6spcollrisk as y
        WHERE
          x.uid = y.uid) as r, gis_victoria.vic_gda9455_admin_sla AS p
       WHERE
        ST_Contains(p.geom, r.geom)
       AND
        (p.sla_code11 = '210152491'
        OR
        p.sla_code11 = '215051831'
        OR
        p.sla_code11 = '215106261'
        OR
        p.sla_code11 = '215106265'
        OR
        p.sla_code11 = '220050571'
        OR
        p.sla_code11 = '220050572'
        OR
        p.sla_code11 = '220050573'
        OR
        p.sla_code11 = '220050574'
        OR
        p.sla_code11 = '220102911'
        OR
        p.sla_code11 = '220102912'
        OR
        p.sla_code11 = '220105154'
        OR
        p.sla_code11 = '220105155'
        OR
        p.sla_code11 = '220150260'
        OR
        p.sla_code11 = '220155991'
        OR
        p.sla_code11 = '220155994'
        OR
        p.sla_code11 = '225055811'
        OR
        p.sla_code11 = '225055814'
        OR
        p.sla_code11 = '235101671'
        OR
        p.sla_code11 = '235101674');
    "))
setkey(egk_risk.rds,uid)

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

egk_coll.rds <- as.data.table(dbGetQuery(con,"
    SELECT
      r.uid AS uid, COUNT(p.id) AS ncoll
    FROM
        (SELECT
          x.uid AS uid, x.geom AS geom, y.egk AS egkrisk
        FROM
          gis_victoria.vic_gda9455_roads_state as x, gis_victoria.vic_nogeom_roads_6spcollrisk as y
        WHERE
          x.uid = y.uid) as r, gis_victoria.vic_gda9455_fauna_egkcoll_western_onnetwork AS p
    WHERE
      ST_DWithin(r.geom, p.geom, .0001)
    GROUP BY
      r.uid;
    "))
setkey(egk_coll.rds,uid)

val.data <- merge(egk_risk.rds,egk_coll.rds, by="uid", all.x=TRUE)
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
