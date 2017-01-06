require(data.table)
require(RPostgreSQL)
require(plyr)

"roc" <- function (obsdat, preddat){
  if (length(obsdat) != length(preddat)) 
    stop("obs and preds must be equal lengths")
  n.x <- as.numeric(length(obsdat[obsdat == 0]))
  n.y <- as.numeric(length(obsdat[obsdat == 1]))
  xy <- c(preddat[obsdat == 0], preddat[obsdat == 1])
  rnk <- rank(xy)
  roc <- ((n.x * n.y) + ((n.x * (n.x + 1))/2) - sum(rnk[1:n.x]))/(n.x * n.y)
  return(round(roc, 4))
}

"dev" <- function (model){
  round(((model$null.deviance - model$deviance)/model$null.deviance)*100,2)
}

#Load data from Victoria EGK collision modelling
load("data/vic_coll_glm")
load(file="data/vic_coll_model_data")
egk.preds <- as.data.table(read.csv(file = "data/vic_coll_preds_glm.csv"))
setkey(egk.preds,uid)

#Load independent data for modelling and validation
bendigo.data <- as.data.table(read.csv(file="data/model_data_bendigo.csv"))
setkey(bendigo.data,uid)

western.data <- as.data.table(read.csv(file="data/model_data_western.csv"))
setkey(western.data,uid)

crashstats.data <- as.data.table(read.csv(file="data/model_data_crashstats.csv"))
setkey(crashstats.data,uid)

#Create copy of model data for additional dataset creation
wv.data <- copy(data)
setkey(wv.data,uid)

#Combine existing data with Bendigo
b.model.data <- copy(bendigo.data)
b.model.data[wv.data, `:=`(uid = i.uid, coll = coll + i.coll)]
# sum(b.model.data$coll>1)
# 21
b.model.data[coll>1,coll:=1]

#Combine existing data with Western
w.model.data <- copy(western.data)
w.model.data[wv.data, `:=`(uid = i.uid, coll = coll + i.coll)]
# sum(w.model.data$coll>1)
# 31
w.model.data[coll>1,coll:=1]

#Combine existing data with Crashstats
c.model.data <- copy(crashstats.data)
c.model.data[wv.data, `:=`(uid = i.uid, coll = coll + i.coll)]
# sum(c.model.data$coll>1)
# 47
c.model.data[coll>1,coll:=1]

#Combine existing data with Bendigo & Western
bw.model.data <- copy(bendigo.data)
setkey(bw.model.data,uid)
bw.model.data[wv.data, `:=`(uid = i.uid, coll = coll + i.coll)]
setkey(bw.model.data,uid)
bw.model.data[western.data, `:=`(uid = i.uid, coll = coll + i.coll)]
# sum(bw.model.data$coll>1)
# 52
bw.model.data[coll>1,coll:=1]

#Combine existing data with Western & Crashstats
wc.model.data <- copy(western.data)
setkey(wc.model.data,uid)
wc.model.data[wv.data, `:=`(uid = i.uid, coll = coll + i.coll)]
setkey(wc.model.data,uid)
wc.model.data[crashstats.data, `:=`(uid = i.uid, coll = coll + i.coll)]
# sum(wc.model.data$coll>1)
# 88
wc.model.data[coll>1,coll:=1]

#Combine existing data with Crashstats & Bendigo
cb.model.data <- copy(crashstats.data)
setkey(cb.model.data,uid)
cb.model.data[wv.data, `:=`(uid = i.uid, coll = coll + i.coll)]
setkey(cb.model.data,uid)
cb.model.data[bendigo.data, `:=`(uid = i.uid, coll = coll + i.coll)]
# sum(cb.model.data$coll>1)
# 71
cb.model.data[coll>1,coll:=1]

#Combine existing data with Bendigo, Western & Crashstats
bwc.model.data <- copy(bendigo.data)
setkey(bwc.model.data,uid)
bwc.model.data[wv.data, `:=`(uid = i.uid, coll = coll + i.coll)]
setkey(bwc.model.data,uid)
bwc.model.data[western.data, `:=`(uid = i.uid, coll = coll + i.coll)]
setkey(bwc.model.data,uid)
bwc.model.data[crashstats.data, `:=`(uid = i.uid, coll = coll + i.coll)]
# sum(bwc.model.data$coll>1)
# 112
bwc.model.data[coll>1,coll:=1]

#Create models
b.glm <- glm(formula = coll ~ log(egk) + log(tvol) + I(log(tvol)^2) + log(tspd), family=binomial(link = "cloglog"), data = b.model.data)  #Fit regression model
summary(b.glm)  #Examine fit of regression model
dev(b.glm)  #Report reduction in deviance
#save(b.glm,file="output/b_glm")

w.glm <- glm(formula = coll ~ log(egk) + log(tvol) + I(log(tvol)^2) + log(tspd), family=binomial(link = "cloglog"), data = w.model.data)  #Fit regression model
summary(w.glm)
dev(w.glm)
#save(w.glm,file="output/w_glm")

c.glm <- glm(formula = coll ~ log(egk) + log(tvol) + I(log(tvol)^2) + log(tspd), family=binomial(link = "cloglog"), data = c.model.data)  #Fit regression model
summary(c.glm)
dev(c.glm)
#save(c.glm,file="output/c_glm")

bw.glm <- glm(formula = coll ~ log(egk) + log(tvol) + I(log(tvol)^2) + log(tspd), family=binomial(link = "cloglog"), data = bw.model.data)  #Fit regression model
summary(bw.glm)
dev(bw.glm)
#save(bw.glm,file="output/bw_glm")

wc.glm <- glm(formula = coll ~ log(egk) + log(tvol) + I(log(tvol)^2) + log(tspd), family=binomial(link = "cloglog"), data = wc.model.data)  #Fit regression model
summary(wc.glm)
dev(wc.glm)
#save(wc.glm,file="output/wc_glm")

cb.glm <- glm(formula = coll ~ log(egk) + log(tvol) + I(log(tvol)^2) + log(tspd), family=binomial(link = "cloglog"), data = cb.model.data)  #Fit regression model
summary(cb.glm)
dev(cb.glm)
#save(cb.glm,file="output/cb_glm")

bwc.glm <- glm(formula = coll ~ log(egk) + log(tvol) + I(log(tvol)^2) + log(tspd), family=binomial(link = "cloglog"), data = bwc.model.data)  #Fit regression model
summary(bwc.glm)
dev(bwc.glm)
#save(bwc.glm,file="output/bwc_glm")

save(b.glm,w.glm,c.glm,bw.glm,wc.glm,cb.glm,bwc.glm,file="output/glms")

#Make predictions
preds <- as.data.table(cbind("uid"=wv.data$uid,"collrisk"=predict(coll.glm, type="response")))

# b.preds <- as.data.table(cbind("uid"=b.model.data$uid,"collrisk"=predict(b.glm, type="response")))
# w.preds <- as.data.table(cbind("uid"=w.model.data$uid,"collrisk"=predict(w.glm, type="response")))
# c.preds <- as.data.table(cbind("uid"=c.model.data$uid,"collrisk"=predict(c.glm, type="response")))
# bw.preds <- as.data.table(cbind("uid"=bw.model.data$uid,"collrisk"=predict(bw.glm, type="response")))
# wc.preds <- as.data.table(cbind("uid"=wc.model.data$uid,"collrisk"=predict(wc.glm, type="response")))
# cb.preds <- as.data.table(cbind("uid"=cb.model.data$uid,"collrisk"=predict(cb.glm, type="response")))
# bwc.preds <- as.data.table(cbind("uid"=bwc.model.data$uid,"collrisk"=predict(bwc.glm, type="response")))

id <- c("b","w","c","bw","wc","cb","bwc")

for(i in id){
  #uid <- mget(paste0(i,".model.data$uid"))
  #assign(paste0(i,"_collrisk"),predict(get(paste0(i,".glm")), type="response")
  preds <- cbind(preds,predict(get(paste0(i,".glm")), type="response"))
  colnames(preds)[length(colnames(preds))] <- paste0(i,"_collrisk")
}
save(preds, file="output/preds")

#Write predictions to database
drv <- dbDriver("PostgreSQL")  #Specify a driver for postgreSQL type database
con <- dbConnect(drv, dbname="qaeco_spatial", user="qaeco", password="Qpostgres15", host="boab.qaeco.com", port="5432")  #Connection to database server on Boab

dbWriteTable(con, c("gis_victoria", "vic_nogeom_roads_egkcollrisk"), value = preds[,.(uid,collrisk)], row.names=FALSE, overwrite=TRUE)

dbWriteTable(con, c("gis_victoria", "vic_nogeom_roads_egkcollrisk_b"), value = preds[,.(uid,"collrisk"=b_collrisk)], row.names=FALSE, overwrite=TRUE)
dbWriteTable(con, c("gis_victoria", "vic_nogeom_roads_egkcollrisk_w"), value = preds[,.(uid,"collrisk"=w_collrisk)], row.names=FALSE, overwrite=TRUE)
dbWriteTable(con, c("gis_victoria", "vic_nogeom_roads_egkcollrisk_c"), value = preds[,.(uid,"collrisk"=c_collrisk)], row.names=FALSE, overwrite=TRUE)

dbWriteTable(con, c("gis_victoria", "vic_nogeom_roads_egkcollrisk_bw"), value = preds[,.(uid,"collrisk"=bw_collrisk)], row.names=FALSE, overwrite=TRUE)
dbWriteTable(con, c("gis_victoria", "vic_nogeom_roads_egkcollrisk_wc"), value = preds[,.(uid,"collrisk"=wc_collrisk)], row.names=FALSE, overwrite=TRUE)
dbWriteTable(con, c("gis_victoria", "vic_nogeom_roads_egkcollrisk_cb"), value = preds[,.(uid,"collrisk"=cb_collrisk)], row.names=FALSE, overwrite=TRUE)

dbWriteTable(con, c("gis_victoria", "vic_nogeom_roads_egkcollrisk_bwc"), value = preds[,.(uid,"collrisk"=bwc_collrisk)], row.names=FALSE, overwrite=TRUE)

#Validate predictions with each independent dataset
val.preds <- data.frame("x"=rep(NA,3),"b"=rep(NA,3),"w"=rep(NA,3),"c"=rep(NA,3),"bw"=rep(NA,3),"wc"=rep(NA,3),"cb"=rep(NA,3))
row.names(val.preds) <- id[1:3]

val.b <- summary(glm(b.model.data$coll ~ predict(coll.glm, b.model.data, type="link"), family = binomial(link = "cloglog")))
val.w <- summary(glm(w.model.data$coll ~ predict(coll.glm, w.model.data, type="link"), family = binomial(link = "cloglog")))
val.c <- summary(glm(c.model.data$coll ~ predict(coll.glm, c.model.data, type="link"), family = binomial(link = "cloglog")))

val.preds[1,1] <- roc(b.model.data$coll, predict(coll.glm, b.model.data, type="response"))
val.preds[2,1] <- roc(w.model.data$coll, predict(coll.glm, w.model.data, type="response"))
val.preds[3,1] <- roc(c.model.data$coll, predict(coll.glm, c.model.data, type="response"))

b.val.w <- summary(glm(w.model.data$coll ~ predict(b.glm, w.model.data, type="link"), family = binomial(link = "cloglog")))
b.val.c <- summary(glm(c.model.data$coll ~ predict(b.glm, c.model.data, type="link"), family = binomial(link = "cloglog")))

val.preds[2,2] <- roc(w.model.data$coll, predict(b.glm, w.model.data, type="response"))
val.preds[3,2] <- roc(c.model.data$coll, predict(b.glm, c.model.data, type="response"))

w.val.b <- summary(glm(b.model.data$coll ~ predict(w.glm, b.model.data, type="link"), family = binomial(link = "cloglog")))
w.val.c <- summary(glm(c.model.data$coll ~ predict(w.glm, c.model.data, type="link"), family = binomial(link = "cloglog")))

val.preds[1,3] <- roc(b.model.data$coll, predict(w.glm, b.model.data, type="response"))
val.preds[3,3] <- roc(c.model.data$coll, predict(w.glm, c.model.data, type="response"))

c.val.b <- summary(glm(b.model.data$coll ~ predict(c.glm, b.model.data, type="link"), family = binomial(link = "cloglog")))
c.val.w <- summary(glm(w.model.data$coll ~ predict(c.glm, w.model.data, type="link"), family = binomial(link = "cloglog")))

val.preds[1,4] <- roc(b.model.data$coll, predict(c.glm, b.model.data, type="response"))
val.preds[2,4] <- roc(w.model.data$coll, predict(c.glm, w.model.data, type="response"))

bw.val.c <- summary(glm(c.model.data$coll ~ predict(bw.glm, c.model.data, type="link"), family = binomial(link = "cloglog")))

val.preds[3,5] <- roc(c.model.data$coll, predict(bw.glm, c.model.data, type="response"))

wc.val.b <- summary(glm(b.model.data$coll ~ predict(wc.glm, b.model.data, type="link"), family = binomial(link = "cloglog")))

val.preds[1,6] <- roc(b.model.data$coll, predict(wc.glm, b.model.data, type="response"))

cb.val.w <- summary(glm(w.model.data$coll ~ predict(cb.glm, w.model.data, type="link"), family = binomial(link = "cloglog")))

val.preds[2,7] <- roc(w.model.data$coll, predict(cb.glm, w.model.data, type="response"))

save(val.preds,val.b,val.w,val.c,b.val.w,b.val.c,w.val.b,w.val.c,c.val.b,c.val.w,bw.val.c,wc.val.b,cb.val.w,file="output/vals")

#Validate predictions with aggregated insurance data
iag.data <- as.data.table(read.csv("data/iag_kang_coll.csv")) #Load aggregated independent data for validation
data.towns <- iag.data[,.N,by="NA_TOWN"]
setnames(data.towns,c("towns","ncoll"))
setkey(data.towns,towns)

#Original predictions
preds.towns <- as.data.table(dbGetQuery(con,"
                                        SELECT
                                        r.uid AS uid, p.locality AS towns, ST_Length(r.geom)/1000 AS length, r.collrisk AS collrisk
                                        FROM
                                        (SELECT
                                        x.uid AS uid, x.geom AS geom, y.collrisk AS collrisk
                                        FROM
                                        gis_victoria.vic_gda9455_roads_state AS x, gis_victoria.vic_nogeom_roads_egkcollrisk AS y
                                        WHERE
                                        x.uid = y.uid) AS r, gis_victoria.vic_gda9455_admin_localities AS p
                                        WHERE
                                        ST_Contains(p.geom, r.geom);
                                        "))
setkey(preds.towns,towns)

val.data.towns <- merge(preds.towns[,sum(exp(collrisk)),by="towns"],data.towns, by="towns", all.x=TRUE)
val.data.towns$ncoll[is.na(val.data.towns$ncoll)] <- 0
colnames(val.data.towns)[2] <- "expcoll"

#no.towns.coll <- val.data.towns[!data.towns,]
#no.match.towns.coll <- data.towns[!val.data.towns,]

val.data.iag <- na.omit(val.data.towns)

val.data.iag$nyears <- 3

val.iag <- summary(glm(formula=ncoll ~ log(expcoll/5) + offset(log(nyears)), data=val.data.iag, family=poisson))

val.iag.dev <- dev(glm(formula=ncoll ~ log(expcoll/5) + offset(log(nyears)), data=val.data.iag, family=poisson))

#Predictions with Bendigo data
b.preds.towns <- as.data.table(dbGetQuery(con,"
      SELECT
        r.uid AS uid, p.locality AS towns, ST_Length(r.geom)/1000 AS length, r.collrisk AS collrisk
      FROM
        (SELECT
          x.uid AS uid, x.geom AS geom, y.collrisk AS collrisk
        FROM
          gis_victoria.vic_gda9455_roads_state AS x, gis_victoria.vic_nogeom_roads_egkcollrisk_b AS y
        WHERE
          x.uid = y.uid) AS r, gis_victoria.vic_gda9455_admin_localities AS p
      WHERE
        ST_Contains(p.geom, r.geom);
    "))
setkey(b.preds.towns,towns)

val.data.towns <- merge(b.preds.towns[,sum(exp(collrisk)),by="towns"],data.towns, by="towns", all.x=TRUE)
val.data.towns$ncoll[is.na(val.data.towns$ncoll)] <- 0
colnames(val.data.towns)[2] <- "expcoll"

#no.towns.coll <- val.data.towns[!data.towns,]
#no.match.towns.coll <- data.towns[!val.data.towns,]

val.data.iag <- na.omit(val.data.towns)

val.data.iag$nyears <- 3

val.iag.b <- summary(glm(formula=ncoll ~ log(expcoll/5) + offset(log(nyears)), data=val.data.iag, family=poisson))

val.iag.dev[2] <- dev(glm(formula=ncoll ~ log(expcoll/5) + offset(log(nyears)), data=val.data.iag, family=poisson))


#Predictions with Western data
w.preds.towns <- as.data.table(dbGetQuery(con,"
      SELECT
        r.uid AS uid, p.locality AS towns, ST_Length(r.geom)/1000 AS length, r.collrisk AS collrisk
      FROM
        (SELECT
          x.uid AS uid, x.geom AS geom, y.collrisk AS collrisk
        FROM
          gis_victoria.vic_gda9455_roads_state AS x, gis_victoria.vic_nogeom_roads_egkcollrisk_w AS y
        WHERE
          x.uid = y.uid) AS r, gis_victoria.vic_gda9455_admin_localities AS p
      WHERE
        ST_Contains(p.geom, r.geom);
    "))
setkey(w.preds.towns,towns)

val.data.towns <- merge(w.preds.towns[,sum(exp(collrisk)),by="towns"],data.towns, by="towns", all.x=TRUE)
val.data.towns$ncoll[is.na(val.data.towns$ncoll)] <- 0
colnames(val.data.towns)[2] <- "expcoll"

#no.towns.coll <- val.data.towns[!data.towns,]
#no.match.towns.coll <- data.towns[!val.data.towns,]

val.data.iag <- na.omit(val.data.towns)

val.data.iag$nyears <- 3

val.iag.w <- summary(glm(formula=ncoll ~ log(expcoll/5) + offset(log(nyears)), data=val.data.iag, family=poisson))

val.iag.dev[3] <- dev(glm(formula=ncoll ~ log(expcoll/5) + offset(log(nyears)), data=val.data.iag, family=poisson))


#Predictions with Crashstats data
c.preds.towns <- as.data.table(dbGetQuery(con,"
      SELECT
        r.uid AS uid, p.locality AS towns, ST_Length(r.geom)/1000 AS length, r.collrisk AS collrisk
      FROM
        (SELECT
          x.uid AS uid, x.geom AS geom, y.collrisk AS collrisk
        FROM
          gis_victoria.vic_gda9455_roads_state AS x, gis_victoria.vic_nogeom_roads_egkcollrisk_c AS y
        WHERE
          x.uid = y.uid) AS r, gis_victoria.vic_gda9455_admin_localities AS p
      WHERE
        ST_Contains(p.geom, r.geom);
    "))
setkey(c.preds.towns,towns)

val.data.towns <- merge(c.preds.towns[,sum(exp(collrisk)),by="towns"],data.towns, by="towns", all.x=TRUE)
val.data.towns$ncoll[is.na(val.data.towns$ncoll)] <- 0
colnames(val.data.towns)[2] <- "expcoll"

#no.towns.coll <- val.data.towns[!data.towns,]
#no.match.towns.coll <- data.towns[!val.data.towns,]

val.data.iag <- na.omit(val.data.towns)

val.data.iag$nyears <- 3

val.iag.c <- summary(glm(formula=ncoll ~ log(expcoll/5) + offset(log(nyears)), data=val.data.iag, family=poisson))

val.iag.dev[4] <- dev(glm(formula=ncoll ~ log(expcoll/5) + offset(log(nyears)), data=val.data.iag, family=poisson))


#Predictions with Bendigo & Western data
bw.preds.towns <- as.data.table(dbGetQuery(con,"
      SELECT
        r.uid AS uid, p.locality AS towns, ST_Length(r.geom)/1000 AS length, r.collrisk AS collrisk
      FROM
        (SELECT
          x.uid AS uid, x.geom AS geom, y.collrisk AS collrisk
        FROM
          gis_victoria.vic_gda9455_roads_state AS x, gis_victoria.vic_nogeom_roads_egkcollrisk_bw AS y
        WHERE
          x.uid = y.uid) AS r, gis_victoria.vic_gda9455_admin_localities AS p
      WHERE
        ST_Contains(p.geom, r.geom);
    "))
setkey(bw.preds.towns,towns)

val.data.towns <- merge(bw.preds.towns[,sum(exp(collrisk)),by="towns"],data.towns, by="towns", all.x=TRUE)
val.data.towns$ncoll[is.na(val.data.towns$ncoll)] <- 0
colnames(val.data.towns)[2] <- "expcoll"

#no.towns.coll <- val.data.towns[!data.towns,]
#no.match.towns.coll <- data.towns[!val.data.towns,]

val.data.iag <- na.omit(val.data.towns)

val.data.iag$nyears <- 3

val.iag.bw <- summary(glm(formula=ncoll ~ log(expcoll/5) + offset(log(nyears)), data=val.data.iag, family=poisson))

val.iag.dev[5] <- dev(glm(formula=ncoll ~ log(expcoll/5) + offset(log(nyears)), data=val.data.iag, family=poisson))


#Predictions with Western data & Crashstats
wc.preds.towns <- as.data.table(dbGetQuery(con,"
      SELECT
        r.uid AS uid, p.locality AS towns, ST_Length(r.geom)/1000 AS length, r.collrisk AS collrisk
      FROM
        (SELECT
          x.uid AS uid, x.geom AS geom, y.collrisk AS collrisk
        FROM
          gis_victoria.vic_gda9455_roads_state AS x, gis_victoria.vic_nogeom_roads_egkcollrisk_wc AS y
        WHERE
          x.uid = y.uid) AS r, gis_victoria.vic_gda9455_admin_localities AS p
      WHERE
        ST_Contains(p.geom, r.geom);
    "))
setkey(wc.preds.towns,towns)

val.data.towns <- merge(wc.preds.towns[,sum(exp(collrisk)),by="towns"],data.towns, by="towns", all.x=TRUE)
val.data.towns$ncoll[is.na(val.data.towns$ncoll)] <- 0
colnames(val.data.towns)[2] <- "expcoll"

#no.towns.coll <- val.data.towns[!data.towns,]
#no.match.towns.coll <- data.towns[!val.data.towns,]

val.data.iag <- na.omit(val.data.towns)

val.data.iag$nyears <- 3

val.iag.wc <- summary(glm(formula=ncoll ~ log(expcoll/5) + offset(log(nyears)), data=val.data.iag, family=poisson))

val.iag.dev[6] <- dev(glm(formula=ncoll ~ log(expcoll/5) + offset(log(nyears)), data=val.data.iag, family=poisson))


#Predictions with Crashstats & Bendigo
cb.preds.towns <- as.data.table(dbGetQuery(con,"
      SELECT
        r.uid AS uid, p.locality AS towns, ST_Length(r.geom)/1000 AS length, r.collrisk AS collrisk
      FROM
        (SELECT
          x.uid AS uid, x.geom AS geom, y.collrisk AS collrisk
        FROM
          gis_victoria.vic_gda9455_roads_state AS x, gis_victoria.vic_nogeom_roads_egkcollrisk_cb AS y
        WHERE
          x.uid = y.uid) AS r, gis_victoria.vic_gda9455_admin_localities AS p
      WHERE
        ST_Contains(p.geom, r.geom);
    "))
setkey(cb.preds.towns,towns)

val.data.towns <- merge(cb.preds.towns[,sum(exp(collrisk)),by="towns"],data.towns, by="towns", all.x=TRUE)
val.data.towns$ncoll[is.na(val.data.towns$ncoll)] <- 0
colnames(val.data.towns)[2] <- "expcoll"

#no.towns.coll <- val.data.towns[!data.towns,]
#no.match.towns.coll <- data.towns[!val.data.towns,]

val.data.iag <- na.omit(val.data.towns)

val.data.iag$nyears <- 3

val.iag.cb <- summary(glm(formula=ncoll ~ log(expcoll/5) + offset(log(nyears)), data=val.data.iag, family=poisson))

val.iag.dev[7] <- dev(glm(formula=ncoll ~ log(expcoll/5) + offset(log(nyears)), data=val.data.iag, family=poisson))


#Predictions with Bendigo, Western & Crashstats
bwc.preds.towns <- as.data.table(dbGetQuery(con,"
      SELECT
        r.uid AS uid, p.locality AS towns, ST_Length(r.geom)/1000 AS length, r.collrisk AS collrisk
      FROM
        (SELECT
          x.uid AS uid, x.geom AS geom, y.collrisk AS collrisk
        FROM
          gis_victoria.vic_gda9455_roads_state AS x, gis_victoria.vic_nogeom_roads_egkcollrisk_bwc AS y
        WHERE
          x.uid = y.uid) AS r, gis_victoria.vic_gda9455_admin_localities AS p
      WHERE
        ST_Contains(p.geom, r.geom);
    "))
setkey(bwc.preds.towns,towns)

val.data.towns <- merge(bwc.preds.towns[,sum(exp(collrisk)),by="towns"],data.towns, by="towns", all.x=TRUE)
val.data.towns$ncoll[is.na(val.data.towns$ncoll)] <- 0
colnames(val.data.towns)[2] <- "expcoll"

#no.towns.coll <- val.data.towns[!data.towns,]
#no.match.towns.coll <- data.towns[!val.data.towns,]

val.data.iag <- na.omit(val.data.towns)

val.data.iag$nyears <- 3

val.iag.bwc <- summary(glm(formula=ncoll ~ log(expcoll/5) + offset(log(nyears)), data=val.data.iag, family=poisson))

val.iag.dev[8] <- dev(glm(formula=ncoll ~ log(expcoll/5) + offset(log(nyears)), data=val.data.iag, family=poisson))

save(val.iag.dev,val.iag,val.iag.b,val.iag.w,val.iag.c,val.iag.bw,val.iag.wc,val.iag.cb,val.iag.bwc,file="output/glm_sums_iag")
