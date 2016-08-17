require("data.table")
require("doMC")
require("maptools")
require("reshape2")
require("rgeos")
require("RPostgreSQL")
require("raster")

drv <- dbDriver("PostgreSQL")  #Specify a driver for postgreSQL type database
con <- dbConnect(drv, dbname="qaeco_spatial", user="qaeco", password="Qpostgres15", host="boab.qaeco.com", port="5432")  #Connection to database server on Boab


data <- as.data.table(read.csv("data/iag_kang_coll.csv"))


######Analysis by statistical local area########
data.sla <- data[abs_area!='missing',.N,by="abs_area"]
setnames(data.sla,c("sla","ncoll"))
setkey(data.sla,sla)

egk_risk.sla <- as.data.table(dbGetQuery(con,"
      SELECT 
        p.sla_name11 as sla, SUM(ST_Length(r.geom))/1000 AS rdlength, AVG(r.egkrisk) AS collrisk
      FROM
        (SELECT
          x.uid AS uid, x.geom AS geom, y.egk AS egkrisk
        FROM
          gis_victoria.vic_gda9455_roads_state AS x, gis_victoria.vic_nogeom_roads_6spcollrisk AS y
        WHERE
          x.uid = y.uid) AS r, gis_victoria.vic_gda9455_admin_sla AS p
      WHERE
        ST_Contains(p.geom, r.geom)
      GROUP BY
        p.sla_name11;
    "))
setkey(egk_risk.sla,sla)
egk_risk.sla$sla <- toupper(egk_risk.sla$sla)

model.data.sla <- merge(egk_risk.sla,data.sla, by="sla", all.x=TRUE)
model.data.sla$ncoll[is.na(model.data.sla$ncoll)] <- 0

no.match <- model.data.sla[is.na(rdlength),]

model.data.sla <- na.omit(model.data.sla)
plot(model.data.sla$ncoll/sum(model.data.sla$ncoll),model.data.sla$collrisk)

model.sla <- glm(formula=ncoll~log(collrisk), data=model.data.sla, family=poisson)

#log(y) = a + b*log(p)
#y = e^a*p^b
#model.sla <- glm(formula=ncoll~log(collrisk) + log(rdlength), data=model.data.sla, family=poisson)

summary(model.sla)

paste("% Deviance Explained: ",round(((model.sla$null.deviance - model.sla$deviance)/model.sla$null.deviance)*100,2),sep="")  #Report reduction in deviance


######Analysis by towns########
data.towns <- data[,.N,by="NA_TOWN"]
setnames(data.towns,c("towns","ncoll"))
setkey(data.towns,towns)

egk_risk.towns <- as.data.table(dbGetQuery(con,"
      SELECT 
        p.name_u as towns, SUM(ST_Length(r.geom))/1000 AS rdlength, AVG(r.egkrisk) AS collrisk
      FROM
        (SELECT
          x.uid AS uid, x.geom AS geom, y.egk AS egkrisk
        FROM
          gis_victoria.vic_gda9455_roads_state AS x, gis_victoria.vic_nogeom_roads_6spcollrisk AS y
        WHERE
          x.uid = y.uid) AS r, gis_victoria.vic_gda9455_admin_suburbs AS p
      WHERE
        ST_Contains(p.geom, r.geom)
      GROUP BY
        p.name_u;
    "))
setkey(egk_risk.towns,towns)

model.data.towns <- merge(egk_risk.towns,data.towns, by="towns", all.x=TRUE)
model.data.towns$ncoll[is.na(model.data.towns$ncoll)] <- 0

no.match <- model.data.towns[is.na(rdlength),]

model.data.towns <- na.omit(model.data.towns)
plot(model.data.towns$ncoll/sum(model.data.towns$ncoll),model.data.towns$collrisk)

model.towns <- glm(formula=ncoll ~ log(collrisk), data=model.data.towns, family=poisson)

summary(model.towns)

paste("% Deviance Explained: ",round(((model.towns$null.deviance - model.towns$deviance)/model.towns$null.deviance)*100,2),sep="")  #Report reduction in deviance
