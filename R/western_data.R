drv <- dbDriver("PostgreSQL")  #Specify a driver for postgreSQL type database
con <- dbConnect(drv, dbname="qaeco_spatial", user="qaeco", password="Qpostgres15", host="boab.qaeco.com", port="5432")  #Connection to database server on Boab

egk_risk.rds <- as.data.table(dbGetQuery(con,"
       SELECT 
        r.id, ST_Length(r.geom)/1000 AS rdlength, r.egkrisk AS collrisk
       FROM
        gis_victoria.vic_gda9455_roads_state as r, gis_victoria.vic_gda9455_admin_sla AS p
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
setkey(egk_risk.rds,id)

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
      r.id AS id, COUNT(p.id) AS ncoll
    FROM
      gis_victoria.vic_gda9455_roads_state as r, gis_victoria.vic_gda9455_fauna_egkcoll_western_onnetwork AS p
    WHERE
      ST_DWithin(r.geom, p.geom, .0001)
    GROUP BY
      r.id;
    "))
setkey(egk_coll.rds,id)

val.data <- merge(egk_risk.rds,egk_coll.rds, by="id", all.x=TRUE)
val.data$ncoll[is.na(val.data$ncoll)] <- 0

val.data <- na.omit(val.data)

model <- glm(formula=ncoll~log(collrisk) + log(rdlength), data=val.data, family=poisson)

model <- glm(formula=ncoll~log(collrisk), data=val.data, family=poisson)

summary(model)

paste("% Deviance Explained: ",round(((model$null.deviance - model$deviance)/model$null.deviance)*100,2),sep="")  #Report reduction in deviance

model.nb <- glm.nb(formula=ncoll~collrisk + log(rdlength), data=val.data)

summary(model.nb)

paste("% Deviance Explained: ",round(((model.nb$null.deviance - model.nb$deviance)/model.nb$null.deviance)*100,2),sep="")  #Report reduction in deviance

val.data2 <- val.data
val.data2$ncoll[val.data2$ncoll >=1] <- 1

model.bin <- glm(formula=ncoll~log(collrisk/(1-collrisk)), data=rbind(val.data2[val.data2$ncoll==1],val.data2[sample(nrow(val.data2[val.data2$ncoll==0]),1000,replace=FALSE), ]), family=binomial)

summary(model.bin)

paste("% Deviance Explained: ",round(((model.bin$null.deviance - model.bin$deviance)/model.bin$null.deviance)*100,2),sep="")  #Report reduction in deviance
