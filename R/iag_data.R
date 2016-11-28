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

dev.iag <- round(((model.iag$null.deviance - model.iag$deviance)/model.iag$null.deviance)*100,2)

dispersiontest(model.iag,trafo=1)

# yhat <- predict(model.iag, type="response")
# zed <- (val.data.iag$ncoll-yhat)/sqrt(yhat)
# sum(zed^2)/(nrow(val.data.iag)-1)
# pchisq(sum(zed^2),nrow(val.data.iag)-1)

#model.iag0 <- lme(fixed = ncoll ~ 1 + offset(log(nyears)), random = ~ 1 + offset(log(nyears)) | id, data = cbind(val.data.iag,"id"=row(val.data.iag)[,1]))
#model.iag0 <- glmer(ncoll ~ 1 + offset(log(nyears)) + (1|id), data=cbind(val.data.iag,"id"=row(val.data.iag)[,1]), family=poisson)
#model.iag2 <- lme(fixed = ncoll ~ log(expcoll/5) + offset(log(nyears)), random = ~ 1 + offset(log(nyears)) | id, data = cbind(val.data.iag,"id"=row(val.data.iag)[,1]))
model.iag2 <- glmer(ncoll ~ log(expcoll/5) + offset(log(nyears)) + (1|id), data=cbind(val.data.iag,"id"=row(val.data.iag)[,1]), family=poisson)

#model.iag2 <- MCMCglmm(ncoll ~ log(expcoll/5) + offset(log(nyears)), data=val.data.iag, family="poisson")

summary(model.iag2)

qqnorm(resid(model.iag2))
qqline(resid(model.iag2))

qqnorm(ranef(model.iag2)$id[,1])
qqline(ranef(model.iag2)$id[,1])

scatter.smooth(fitted(model.iag2),resid(model.iag2))
plot(predict(model.iag2, type="response"),val.data.iag$ncoll, xlab="Predicted Counts", ylab="Observed Counts")
abline(0, 1, col="red", lty=2)


dev.iag.glmm <- R2.glmm(model.iag2,data.frame(val.data.iag,"id"=as.factor(row(val.data.iag)[,1])),as.formula(~ log(expcoll/5) + offset(log(nyears))))

# nsim <- 1000
# bsim <- arm::sim(model.iag2, n.sim=nsim)
# Xmat <- model.matrix(model.iag2)
# y.hat <- matrix(nrow=nrow(val.data.iag), ncol=nsim)
# resid.y <- matrix(nrow=nrow(val.data.iag), ncol=nsim)
# for(i in 1:nsim){
#   bsimi <- matrix(fixef(bsim)[i,], nrow(val.data.iag), ncol=length(fixef(model.iag2)), byrow=TRUE)
#   bsimi[,1] <- bsimi[,1] + ranef(bsim)$id[i,match(dat$id, levels(dat$id)),1]
#   for(j in 1:nrow(val.data.iag)){
#     y.hat[j,i] <- exp(Xmat[j,]%*%bsimi[j,])
#     resid.y[j,i] <- val.data.iag$ncoll[j]-y.hat[j,i]
#   }
# }
# dev.iag.c <- round((1-mean(apply(resid.y, 2, var))/var(val.data.iag$ncoll))*100, 2)
# 
# a.hat <- matrix(nrow=nrow(val.data.iag), ncol=nsim)
# Xmat <- model.matrix(~ log(expcoll/5) + offset(log(nyears)), data=dat)
# for(i in 1:nsim){
#   a.hat[,i] <- exp(Xmat%*%fixef(bsim)[i,])
# }
# 
# dev.iag.m <- round((1-mean(apply(ranef(bsim)$id[,,1], 1, var))/mean(apply(t(a.hat)+ranef(bsim)$id[,,1], 1, var)))*100, 2)



R2_iag <- sem.model.fits(model.iag2)

r.squaredGLMM(model.iag2)

Fixed <- fixef(model.iag2)[2] * getME(model.iag2,"X")[, 2]
VarF <- var(Fixed)

R2_m <- VarF/(VarF + VarCorr(model.iag2)$id[1] + log(1 + 1/exp(as.numeric(fixef(model.iag0)))))
dev.iag.m <- paste("% Deviance Explained: ",round(R2_m*100,2),sep="")  #Report reduction in deviance

R2_c <- (VarF + VarCorr(model.iag2)$id[1])/(VarF + VarCorr(model.iag2)$id[1] + log(1 + 1/exp(as.numeric(fixef(model.iag0)))))
dev.iag.c <- paste("% Deviance Explained: ",round(R2_c*100,2),sep="")  #Report reduction in deviance


set.seed(123)
model.iag.stan0 <- map2stan(
  alist(
    ncoll ~ dpois(l),
    log(l) <- a + log(nyears),
    a ~ dnorm(0,10)
  ),
  data=data.frame(val.data.iag,"id"=row(val.data.iag)[,1]),
  iter=3000,
  chains=3
)
precis(model.iag.stan0)


set.seed(123)
model.iag.stan1 <- map2stan(
  alist(
    ncoll ~ dpois(l),
    log(l) <- a + b*log(expcoll/5) + log(nyears),
    a ~ dnorm(0,10),
    b ~ dnorm(0,1)
  ),
  data=data.frame(val.data.iag,"id"=row(val.data.iag)[,1]),
  iter=3000,
  chains=3
)
precis(model.iag.stan1)


set.seed(123)
model.iag.stan2 <- map2stan(
  alist(
    ncoll ~ dpois(l),
    log(l) <- a + a_id[id] + log(nyears),
    a ~ dnorm(0,10),
    a_id[id] ~ dnorm(0, sigma_id),
    sigma_id ~ dcauchy(0,2)
  ),
  data=data.frame(val.data.iag,"id"=row(val.data.iag)[,1]),
  iter=3000,
  chains=1
)
precis(model.iag.stan2)


set.seed(123)
model.iag.stan3 <- map2stan(
  alist(
    ncoll ~ dpois(l),
    log(l) <- a + a_id[id] + b*log(expcoll/5) + log(nyears),
    a ~ dnorm(0,10),
    b ~ dnorm(0,1),
    a_id[id] ~ dnorm(0, sigma_id),
    sigma_id ~ dcauchy(0,2)
  ),
  data=data.frame(val.data.iag,"id"=row(val.data.iag)[,1]),
  iter=3000,
  chains=3
)
precis(model.iag.stan3)

compare(model.iag.stan0,model.iag.stan1,model.iag.stan2,model.iag.stan3)

int_out <- coda.samples(model.iag.stan2, n.iter=ni, thin=nthin, variable.names=vars)

N <- nrow(val.data.iag)
ncoll <- val.data.iag$ncoll
expcoll <- val.data.iag$expcoll
nyears <- val.data.iag$nyears
id <- seq(1:nrow(val.data.iag))

scode.iag <- "
data{
  int<lower=1> N;
  int<lower=0> ncoll[N];
  vector[N] expcoll;
  vector[N] nyears;
  int<lower=1> id[N];
}
transformed data{
  vector[N] l_nyears;
  vector[N] l_expcoll;
  l_nyears = log(nyears);
  l_expcoll = log(expcoll/5);
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
    l[i] = a + a_olre[id[i]] + b*l_expcoll[i] + l_nyears[i];
  }
  ncoll ~ poisson_log(l);
}
"

model.iag <- stan(model_code = scode.iag, iter = 5000, chains = 3, cores = 3, seed=123)

precis(model.iag)


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
