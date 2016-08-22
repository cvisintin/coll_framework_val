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

data <- as.data.table(read.csv("data/iag_kang_coll.csv"))

######Analysis by statistical local area########
data.sla <- data[abs_area!='missing',.N,by="abs_area"]
setnames(data.sla,c("sla","ncoll"))
data.sla$sla <- as.character(data.sla$sla)
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

no.sla.coll <- model.data.sla[!data.sla,]
no.match.sla.coll <- data.sla[!model.data.sla,]

model.data.sla <- na.omit(model.data.sla)
plot(model.data.sla$ncoll/sum(model.data.sla$ncoll),log(model.data.sla$collrisk))

distplot(model.data.sla$ncoll, type="poisson")
distplot(model.data.sla$ncoll, type="nbinomial")

model.sla <- glm(formula=ncoll ~ log(collrisk), data=model.data.sla, family=poisson)

#log(y) = a + b*log(p)
#y = e^a*p^b
#model.sla <- glm(formula=ncoll~log(collrisk) + log(rdlength), data=model.data.sla, family=poisson)

summary(model.sla)

paste("% Deviance Explained: ",round(((model.sla$null.deviance - model.sla$deviance)/model.sla$null.deviance)*100,2),sep="")  #Report reduction in deviance


model.sla2 <- glm.nb(formula=ncoll ~ log(collrisk), data=model.data.sla)

summary(model.sla2)

paste("% Deviance Explained: ",round(((model.sla2$null.deviance - model.sla2$deviance)/model.sla2$null.deviance)*100,2),sep="")  #Report reduction in deviance

plot(model.data.sla$ncoll, exp(model.sla2$coefficients[1]+model.sla2$coefficients[2]*log(model.data.sla$collrisk)),xlab="Observed Number of Collisions",ylab="Expected Number of Collisions")
abline(a=0,b=1,lty=2,col="red")

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

no.towns.coll <- model.data.towns[!data.towns,]
no.match.towns.coll <- data.towns[!model.data.towns,]

model.data.towns <- na.omit(model.data.towns)
plot(model.data.towns$ncoll/sum(model.data.towns$ncoll),model.data.towns$collrisk)

distplot(model.data.towns$ncoll, type="poisson")
distplot(model.data.towns$ncoll, type="nbinomial")

model.towns <- glm(formula=ncoll ~ log(collrisk), data=model.data.towns, family=poisson)

summary(model.towns)

paste("% Deviance Explained: ",round(((model.towns$null.deviance - model.towns$deviance)/model.towns$null.deviance)*100,2),sep="")  #Report reduction in deviance

model.towns2 <- glm.nb(formula=ncoll ~ log(collrisk), data=model.data.towns)

summary(model.towns2)

paste("% Deviance Explained: ",round(((model.towns2$null.deviance - model.towns2$deviance)/model.towns2$null.deviance)*100,2),sep="")  #Report reduction in deviance

plot(model.data.towns$ncoll, exp(model.towns2$coefficients[1]+model.towns2$coefficients[2]*log(model.data.towns$collrisk)),xlab="Observed Number of Collisions",ylab="Expected Number of Collisions")
abline(a=0,b=1,lty=2,col="red")
