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

drv <- dbDriver("PostgreSQL")  #Specify a driver for postgreSQL type database
con <- dbConnect(drv, dbname="qaeco_spatial", user="qaeco", password="Qpostgres15", host="boab.qaeco.com", port="5432")  #Connection to database server on Boab

data <- as.data.table(read.csv("data/iag_kang_coll.csv"))

######Analysis by statistical local area########
# data.sla <- data[abs_area!='missing',.N,by="abs_area"]
# setnames(data.sla,c("sla","ncoll"))
# data.sla$sla <- as.character(data.sla$sla)
# setkey(data.sla,sla)
# 
# egk_risk.sla <- as.data.table(dbGetQuery(con,"
#       SELECT 
#         p.sla_name11 as sla, SUM(ST_Length(r.geom))/1000 AS rdlength, AVG(r.egkrisk) AS collrisk
#       FROM
#         (SELECT
#           x.uid AS uid, x.geom AS geom, y.egk AS egkrisk
#         FROM
#           gis_victoria.vic_gda9455_roads_state AS x, gis_victoria.vic_nogeom_roads_6spcollrisk AS y
#         WHERE
#           x.uid = y.uid) AS r, gis_victoria.vic_gda9455_admin_sla AS p
#       WHERE
#         ST_Contains(p.geom, r.geom)
#       GROUP BY
#         p.sla_name11;
#     "))
# setkey(egk_risk.sla,sla)
# egk_risk.sla$sla <- toupper(egk_risk.sla$sla)
# 
# model.data.sla <- merge(egk_risk.sla,data.sla, by="sla", all.x=TRUE)
# model.data.sla$ncoll[is.na(model.data.sla$ncoll)] <- 0
# 
# no.sla.coll <- model.data.sla[!data.sla,]
# no.match.sla.coll <- data.sla[!model.data.sla,]
# 
# model.data.sla <- na.omit(model.data.sla)
# plot(model.data.sla$ncoll/sum(model.data.sla$ncoll),log(model.data.sla$collrisk))
# 
# distplot(model.data.sla$ncoll, type="poisson")
# distplot(model.data.sla$ncoll, type="nbinomial")
# 
# model.sla <- glm(formula=ncoll ~ log(collrisk), data=model.data.sla, family=poisson)
# 
# #log(y) = a + b*log(p)
# #y = a*p^b
# #model.sla <- glm(formula=ncoll~log(collrisk) + log(rdlength), data=model.data.sla, family=poisson)
# 
# summary(model.sla)
# 
# paste("% Deviance Explained: ",round(((model.sla$null.deviance - model.sla$deviance)/model.sla$null.deviance)*100,2),sep="")  #Report reduction in deviance
# 
# 
# model.sla2 <- glm.nb(formula=ncoll ~ log(collrisk), data=model.data.sla)
# 
# summary(model.sla2)
# 
# paste("% Deviance Explained: ",round(((model.sla2$null.deviance - model.sla2$deviance)/model.sla2$null.deviance)*100,2),sep="")  #Report reduction in deviance
# 
# plot(model.data.sla$ncoll, exp(model.sla2$coefficients[1]+model.sla2$coefficients[2]*log(model.data.sla$collrisk)),xlab="Observed Number of Collisions",ylab="Expected Number of Collisions")
# abline(a=0,b=1,lty=2,col="red")

######Analysis by towns########
data.towns <- data[,.N,by="NA_TOWN"]
setnames(data.towns,c("towns","ncoll"))
setkey(data.towns,towns)

egk_risk.towns <- as.data.table(dbGetQuery(con,"
      SELECT 
        p.locality as towns, SUM(ST_Length(r.geom))/1000 AS rdlength, AVG(r.egkrisk) AS collrisk, SUM(EXP(r.egkrisk)) AS expcoll
      FROM
        (SELECT
          x.uid AS uid, x.geom AS geom, y.egk AS egkrisk
        FROM
          gis_victoria.vic_gda9455_roads_state AS x, gis_victoria.vic_nogeom_roads_6spcollrisk AS y
        WHERE
          x.uid = y.uid) AS r, gis_victoria.vic_gda9455_admin_localities AS p
      WHERE
        ST_Contains(p.geom, r.geom)
      GROUP BY
        p.locality;
    "))
setkey(egk_risk.towns,towns)

val.data.towns <- merge(egk_risk.towns,data.towns, by="towns", all.x=TRUE)
val.data.towns$ncoll[is.na(val.data.towns$ncoll)] <- 0

#write.csv(val.data.towns[,.(towns,ncoll)],"data/iag_kang_coll_sums.csv", row.names=FALSE)

no.towns.coll <- val.data.towns[!data.towns,]
no.match.towns.coll <- data.towns[!val.data.towns,]

val.data.iag <- na.omit(val.data.towns)

val.data.iag$nyears <- 3

#distplot(val.data.iag$ncoll, type="poisson")
#distplot(val.data.iag$ncoll, type="nbinomial")

model.iag <- glm(formula=ncoll ~ log(expcoll/5) + offset(log(nyears)), data=val.data.iag, family=poisson)

#model.iag2 <- glm(formula=ncoll ~ log(expcoll/5) + offset(log(nyears)), data=val.data.iag, family=quasipoisson)

summary(model.iag)

dev.iag <- paste("% Deviance Explained: ",round(((model.iag$null.deviance - model.iag$deviance)/model.iag$null.deviance)*100,2),sep="")  #Report reduction in deviance

dispersiontest(model.iag,trafo=1)

# yhat <- predict(model.iag, type="response")
# zed <- (val.data.iag$ncoll-yhat)/sqrt(yhat)
# sum(zed^2)/(nrow(val.data.iag)-1)
# pchisq(sum(zed^2),nrow(val.data.iag)-1)

#model.iag0 <- glmer(ncoll ~ 1 + offset(log(nyears)) + (1|id), data=cbind(val.data.iag,"id"=row(val.data.iag)[,1]), family=poisson)
model.iag2 <- glmer(ncoll ~ log(expcoll/5) + offset(log(nyears)) + (1|id), data=cbind(val.data.iag,"id"=row(val.data.iag)[,1]), family=poisson)

#model.iag2 <- MCMCglmm(ncoll ~ log(expcoll/5) + offset(log(nyears)), data=val.data.iag, family="poisson")

summary(model.iag2)

R2_iag <- sem.model.fits(model.iag2)

r.squaredGLMM(model.iag2)

Fixed <- fixef(model.iag2)[2] * getME(model.iag2,"X")[, 2]
VarF <- var(Fixed)

R2_m <- VarF/(VarF + VarCorr(model.iag2)$id[1] + log(1 + 1/exp(as.numeric(fixef(model.iag0)))))
dev.iag.m <- paste("% Deviance Explained: ",round(R2_m*100,2),sep="")  #Report reduction in deviance

R2_c <- (VarF + VarCorr(model.iag2)$id[1])/(VarF + VarCorr(model.iag2)$id[1] + log(1 + 1/exp(as.numeric(fixef(model.iag0)))))
dev.iag.c <- paste("% Deviance Explained: ",round(R2_c*100,2),sep="")  #Report reduction in deviance

set.seed(123)
coll.model.map <- map(
  alist(
    y ~ dbinom(1,p),
    p <- 1 - exp(-exp(a + b*log(x) + z)),
    a ~ dnorm(0,1),
    b ~ dnorm(0,1)
  ),
  start=list(a=-2.0,b=0.2),
  data=list(y=val.data.iag$ncoll,x=val.data.iag$expcoll/5,z=log(val.data.iag$nyears))
)
precis(coll.model.map)


# plot(model.towns$coefficients[1]+model.towns$coefficients[2]*model.data.towns$expcoll, model.data.towns$ncoll, xlab="Expected Number of Collisions",ylab="Observed Number of Collisions")
# abline(a=0,b=1,lty=2,col="red")
# 
# model.towns2 <- glm.nb(formula=ncoll ~ expcoll, data=model.data.towns)
# 
# summary(model.towns2)
# 
# paste("% Deviance Explained: ",round(((model.towns2$null.deviance - model.towns2$deviance)/model.towns2$null.deviance)*100,2),sep="")  #Report reduction in deviance
# 
# plot(model.data.towns$ncoll, exp(model.towns2$coefficients[1]+model.towns2$coefficients[2]*log(model.data.towns$collrisk)),xlab="Observed Number of Collisions",ylab="Expected Number of Collisions")
# abline(a=0,b=1,lty=2,col="red")
