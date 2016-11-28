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
        ST_Contains(p.geom, r.geom)
       AND
        (p.sla_code11 = '235052621'
        OR
        p.sla_code11 = '235052622'
        OR
        p.sla_code11 = '235052623'
        OR
        p.sla_code11 = '235052624'
        OR
        p.sla_code11 = '235052625'
        OR
        p.sla_code11 = '235052626'
        OR
        p.sla_code11 = '235102628');
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
          gis_victoria.vic_gda9455_roads_state AS x, gis_victoria.vic_gda9455_fauna_egk_bendigo AS p
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

val.data.bendigo <- na.omit(val.data)

val.data.bendigo$nyears <- 8

#distplot(val.data.bendigo$ncoll, type="poisson")
#distplot(val.data.bendigo$ncoll, type="nbinomial")

#model <- glm(formula=ncoll~log(collrisk) + log(rdlength), data=val.data.bendigo, family=poisson)

model.bendigo <- glm(formula=ncoll ~ I(collrisk - log(5)) + offset(log(nyears)), data=val.data.bendigo, family=poisson)

summary(model.bendigo)

dev.bendigo <- round(((model.bendigo$null.deviance - model.bendigo$deviance)/model.bendigo$null.deviance)*100,2)  #Report reduction in deviance

dispersiontest(model.bendigo,trafo=1)

model.bendigo2 <- glmer(ncoll ~ I(collrisk - log(5)) + offset(log(nyears)) + (1|id), data=cbind(val.data.bendigo,"id"=row(val.data.bendigo)[,1]), family=poisson)
#model.bendigo0 <- glmer(ncoll ~ 1 + offset(log(nyears)) + (1|id), data=cbind(val.data.bendigo,"id"=row(val.data.bendigo)[,1]), family=poisson)


summary(model.bendigo2)

qqnorm(resid(model.bendigo2))
qqline(resid(model.bendigo2))

qqnorm(ranef(model.bendigo2)$id[,1])
qqline(ranef(model.bendigo2)$id[,1])

scatter.smooth(fitted(model.bendigo2),resid(model.bendigo2))
plot(fitted(model.bendigo2),val.data.bendigo$ncoll)

dev.bendigo.glmm <- R2.glmm(model.bendigo2,data.frame(val.data.bendigo,"id"=as.factor(row(val.data.bendigo)[,1])),as.formula(~ I(collrisk - log(5)) + offset(log(nyears))))


Fixed <- fixef(model.bendigo2)[2] * getME(model.bendigo2,"X")[, 2]
VarF <- var(Fixed)

R2_m <- VarF/(VarF + VarCorr(model.bendigo2)$id[1] + log(1 + 1/exp(as.numeric(fixef(model.bendigo0)))))
dev.bendigo.m <- paste("% Deviance Explained: ",round(R2_m*100,2),sep="")  #Report reduction in deviance

R2_c <- (VarF + VarCorr(model.bendigo2)$id[1])/(VarF + VarCorr(model.bendigo2)$id[1] + log(1 + 1/exp(as.numeric(fixef(model.bendigo0)))))
dev.bendigo.c <- paste("% Deviance Explained: ",round(R2_c*100,2),sep="")  #Report reduction in deviance


N <- nrow(val.data.bendigo)
ncoll <- val.data.bendigo$ncoll
collrisk <- val.data.bendigo$collrisk
nyears <- val.data.bendigo$nyears
id <- seq(1:nrow(val.data.bendigo))

scode.bendigo <- "
data{
  int<lower=1> N;
  int<lower=0> ncoll[N];
  vector[N] collrisk;
  vector[N] nyears;
  int<lower=1> id[N];
}
transformed data{
  vector[N] l_nyears;
  vector[N] l_collrisk;
  l_nyears = log(nyears);
  l_collrisk = collrisk - log(5);
}
parameters{
  real a;
  real b;
  vector[N] a_olre;
  real<lower=0> sigma_olre;
}
model{
  vector[N] l;
  sigma_olre ~ cauchy( 0 , 2 );
  a_olre ~ normal( 0 , sigma_olre );
  b ~ normal( 0 , 1 );
  a ~ normal( 0 , 10 );
  for ( i in 1:N ) {
    l[i] = a + a_olre[id[i]] + b*l_collrisk[i] + l_nyears[i];
  }
  ncoll ~ poisson_log(l);
}
"

model.bendigo <- stan(model_code = scode.bendigo, iter = 500, chains = 1, cores = 1, seed=123)

precis(model.bendigo)


# model.nb <- glm.nb(formula=ncoll ~ log(collrisk), data=val.data)
# summary(model.nb)
# paste("% Deviance Explained: ",round(((model.nb$null.deviance - model.nb$deviance)/model.nb$null.deviance)*100,2),sep="")  #Report reduction in deviance
# 
# plot(val.data$ncoll, exp(model.nb$coefficients[1]+model.nb$coefficients[2]*log(val.data$collrisk)),xlab="Observed Number of Collisions",ylab="Expected Number of Collisions")
# abline(a=0,b=1,lty=2,col="red")
