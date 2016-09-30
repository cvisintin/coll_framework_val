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
        r.uid as uid, ST_Length(r.geom)/1000 AS rdlength, r.egkrisk AS collrisk
       FROM
        (SELECT
          x.uid AS uid, x.geom AS geom, y.egk AS egkrisk
        FROM
          gis_victoria.vic_gda9455_roads_state as x, gis_victoria.vic_nogeom_roads_6spcollrisk as y
        WHERE
          x.uid = y.uid) as r, gis_victoria.vic_gda9455_admin_sla AS p
       WHERE
        ST_Contains(p.geom, r.geom);
    "))
setkey(egk_risk.rds,uid)

egk_coll.rds <- as.data.table(dbGetQuery(con,"
    SELECT
      r.uid AS uid, COUNT(p.id) AS ncoll
    FROM
        (SELECT
          uid, geom
        FROM
          gis_victoria.vic_gda9455_roads_state) AS r, 
        (SELECT DISTINCT ON (p.id)
          p.id AS id, ST_ClosestPoint(x.geom, p.geom) AS geom
        FROM
          gis_victoria.vic_gda9455_roads_state AS x, gis_victoria.vic_gda9455_fauna_wv_2015_egkcoll AS p
        WHERE
          ST_DWithin(x.geom, p.geom, 60)) AS p
    WHERE
      ST_DWithin(r.geom, p.geom, .0001)
    GROUP BY
      r.uid;
    "))
setkey(egk_coll.rds,uid)

val.data <- merge(egk_risk.rds,egk_coll.rds, by="uid", all.x=TRUE)
val.data$ncoll[is.na(val.data$ncoll)] <- 0

val.data.wv <- na.omit(val.data)

val.data.wv$nyears <- 1

#distplot(val.data.wv$ncoll, type="poisson")
#distplot(val.data.wv$ncoll, type="nbinomial")

#model <- glm(formula=ncoll~log(collrisk) + log(rdlength), data=val.data.wv, family=poisson)

model.wv <- glm(formula=ncoll ~ I(collrisk - log(5)) + offset(log(nyears)), data=val.data.wv, family=poisson)

summary(model.wv)

dev.wv <- paste("% Deviance Explained: ",round(((model.wv$null.deviance - model.wv$deviance)/model.wv$null.deviance)*100,2),sep="")  #Report reduction in deviance

dispersiontest(model.wv,trafo=1)

model.wv0 <- glmer(ncoll ~ 1 + offset(log(nyears)) + (1|id), data=cbind(val.data.wv,"id"=row(val.data.wv)[,1]), family=poisson)
model.wv2 <- glmer(ncoll ~ I(collrisk - log(5)) + offset(log(nyears)) + (1|id), data=cbind(val.data.wv,"id"=row(val.data.wv)[,1]), family=poisson)

summary(model.wv2)

R2_wv <- sem.model.fits(model.wv2)

Fixed <- fixef(model.wv2)[2] * getME(model.wv2,"X")[, 2]
VarF <- var(Fixed)

R2_m <- VarF/(VarF + VarCorr(model.wv2)$id[1] + log(1 + 1/exp(as.numeric(fixef(model.wv0)))))
dev.wv.m <- paste("% Deviance Explained: ",round(R2_m*100,2),sep="")  #Report reduction in deviance

R2_c <- (VarF + VarCorr(model.wv2)$id[1])/(VarF + VarCorr(model.wv2)$id[1] + log(1 + 1/exp(as.numeric(fixef(model.wv0)))))
dev.wv.c <- paste("% Deviance Explained: ",round(R2_c*100,2),sep="")  #Report reduction in deviance


# model.nb <- glm.nb(formula=ncoll ~ log(collrisk), data=val.data)
# summary(model.nb)
# paste("% Deviance Explained: ",round(((model.nb$null.deviance - model.nb$deviance)/model.nb$null.deviance)*100,2),sep="")  #Report reduction in deviance
# 
# plot(val.data$ncoll, exp(model.nb$coefficients[1]+model.nb$coefficients[2]*log(val.data$collrisk)),xlab="Observed Number of Collisions",ylab="Expected Number of Collisions")
# abline(a=0,b=1,lty=2,col="red")
