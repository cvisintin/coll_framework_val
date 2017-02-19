require(data.table)
require(RPostgreSQL)
require(plyr)
require(xtable)

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

bendigo.data.trunc <- as.data.table(read.csv(file="data/model_data_bendigo_trunc.csv"))
setkey(bendigo.data.trunc,uid)
bendigo.data.trunc$offset <- log(nrow(bendigo.data.trunc))

western.data <- as.data.table(read.csv(file="data/model_data_western.csv"))
setkey(western.data,uid)

western.data.trunc <- as.data.table(read.csv(file="data/model_data_western_trunc.csv"))
setkey(western.data.trunc,uid)
western.data.trunc$offset <- log(nrow(western.data.trunc))

crashstats.data <- as.data.table(read.csv(file="data/model_data_crashstats.csv"))
setkey(crashstats.data,uid)
crashstats.data$offset <- log(nrow(crashstats.data))

#Create models with independent datasets
b.glm <- glm(formula = coll ~ log(egk) + log(tvol) + I(log(tvol)^2) + log(tspd), family=binomial(link = "cloglog"), data = bendigo.data.trunc)  #Fit regression model
summary(b.glm)
dev(b.glm)

w.glm <- glm(formula = coll ~ log(egk) + log(tvol) + I(log(tvol)^2) + log(tspd), family=binomial(link = "cloglog"), data = western.data.trunc)  #Fit regression model
summary(w.glm)
dev(w.glm)

c.glm <- glm(formula = coll ~ log(egk) + log(tvol) + I(log(tvol)^2) + log(tspd), family=binomial(link = "cloglog"), data = crashstats.data)  #Fit regression model
summary(c.glm)
dev(c.glm)

#model data with glm and summarize model output data (for latex table)
glm_sums <- data.frame(character(),character(),numeric(),numeric(),numeric(),numeric(),numeric(),stringsAsFactors=FALSE,row.names=NULL)
colnames(glm_sums) <- c("Dataset","Variable","Coefficient","Standard Error","$Z\\text{-value}$","$\\PRZ$","Deviance Explained")
model.list <- list(b.glm,w.glm,c.glm)
name.list <- list("City of Bendigo","Western District","Crashstats")
for (i in 1:3) {
  x.names <- c("Intercept", "EGK", "TVOL", "TVOL$^2$", "TSPD")
  x.species <- c(name.list[[i]], NA, NA, NA, NA)
  x.coef <- signif(coef(summary(model.list[[i]]))[,1],digits=4)
  x.se <- signif(coef(summary(model.list[[i]]))[,2],digits=4)
  x.zvalue <- signif(coef(summary(model.list[[i]]))[,3],digits=4)
  x.prz <- signif(coef(summary(model.list[[i]]))[,4],digits=2)
  x.prz <- sapply(x.prz, function(x) ifelse(x < 2e-16, 2e-16, x))
  x.dev <- c(round(((model.list[[i]]$null.deviance - model.list[[i]]$deviance)/model.list[[i]]$null.deviance)*100,2), NA, NA, NA, NA)
  x.all <- data.frame(cbind(x.species,x.names,x.coef,x.se,x.zvalue,x.prz,x.dev),stringsAsFactors=FALSE,row.names=NULL)
  colnames(x.all) <- c("Dataset","Variable","Coefficient","Standard Error","$Z\\text{-value}$","$\\PRZ$","Deviance Explained")

  newrow = rep(NA,length(x.all))
  glm_sums <- rbind(glm_sums, x.all, newrow)

  rm(x.names,x.coef,x.se,x.zvalue,x.prz,x.dev,x.species)
  rm(x.all)
  rm(newrow)
}
print(xtable(glm_sums), include.rownames=FALSE, sanitize.text.function=function(x){x}, floating=FALSE)

#Create copy of model data for additional dataset creation
wv.data <- copy(data)
setkey(wv.data,uid)

#Combine existing data with Bendigo
ob.model.data <- copy(bendigo.data)
ob.model.data[wv.data, `:=`(uid = i.uid, coll = coll + i.coll)]
# sum(b.model.data$coll>1)
# 21
ob.model.data[coll>1,coll:=1]

#Combine existing data with Western
ow.model.data <- copy(western.data)
ow.model.data[wv.data, `:=`(uid = i.uid, coll = coll + i.coll)]
# sum(w.model.data$coll>1)
# 31
ow.model.data[coll>1,coll:=1]

#Combine existing data with Crashstats
oc.model.data <- copy(crashstats.data)
oc.model.data[wv.data, `:=`(uid = i.uid, coll = coll + i.coll)]
# sum(c.model.data$coll>1)
# 47
oc.model.data[coll>1,coll:=1]

#Combine existing data with Bendigo & Western
obw.model.data <- copy(bendigo.data)
setkey(obw.model.data,uid)
obw.model.data[wv.data, `:=`(uid = i.uid, coll = coll + i.coll)]
setkey(obw.model.data,uid)
obw.model.data[western.data, `:=`(uid = i.uid, coll = coll + i.coll)]
# sum(bw.model.data$coll>1)
# 52
obw.model.data[coll>1,coll:=1]

#Combine existing data with Western & Crashstats
owc.model.data <- copy(western.data)
setkey(owc.model.data,uid)
owc.model.data[wv.data, `:=`(uid = i.uid, coll = coll + i.coll)]
setkey(owc.model.data,uid)
owc.model.data[crashstats.data, `:=`(uid = i.uid, coll = coll + i.coll)]
# sum(wc.model.data$coll>1)
# 88
owc.model.data[coll>1,coll:=1]

#Combine existing data with Crashstats & Bendigo
ocb.model.data <- copy(crashstats.data)
setkey(ocb.model.data,uid)
ocb.model.data[wv.data, `:=`(uid = i.uid, coll = coll + i.coll)]
setkey(ocb.model.data,uid)
ocb.model.data[bendigo.data, `:=`(uid = i.uid, coll = coll + i.coll)]
# sum(cb.model.data$coll>1)
# 71
ocb.model.data[coll>1,coll:=1]

#Combine existing data with Bendigo, Western & Crashstats
obwc.model.data <- copy(bendigo.data)
setkey(obwc.model.data,uid)
obwc.model.data[wv.data, `:=`(uid = i.uid, coll = coll + i.coll)]
setkey(obwc.model.data,uid)
obwc.model.data[western.data, `:=`(uid = i.uid, coll = coll + i.coll)]
setkey(obwc.model.data,uid)
obwc.model.data[crashstats.data, `:=`(uid = i.uid, coll = coll + i.coll)]
# sum(bwc.model.data$coll>1)
# 112
obwc.model.data[coll>1,coll:=1]

#Sum total collision for each data combination
all.data <- list(wv.data,ob.model.data,ow.model.data,oc.model.data,obw.model.data,owc.model.data,ocb.model.data,obwc.model.data)
total.coll <- lapply(all.data, function(x) sum(x[coll==1,coll]))

save(all.data,total.coll,file="data/all_data")

#Create models
o.glm <- coll.glm
rm(coll.glm)

ob.glm <- glm(formula = coll ~ log(egk) + log(tvol) + I(log(tvol)^2) + log(tspd), family=binomial(link = "cloglog"), data = ob.model.data)  #Fit regression model
summary(ob.glm)  #Examine fit of regression model
dev(ob.glm)  #Report reduction in deviance
#save(b.glm,file="output/b_glm")

ow.glm <- glm(formula = coll ~ log(egk) + log(tvol) + I(log(tvol)^2) + log(tspd), family=binomial(link = "cloglog"), data = ow.model.data)  #Fit regression model
summary(ow.glm)
dev(ow.glm)
#save(w.glm,file="output/w_glm")

oc.glm <- glm(formula = coll ~ log(egk) + log(tvol) + I(log(tvol)^2) + log(tspd), family=binomial(link = "cloglog"), data = oc.model.data)  #Fit regression model
summary(oc.glm)
dev(oc.glm)
#save(c.glm,file="output/c_glm")

obw.glm <- glm(formula = coll ~ log(egk) + log(tvol) + I(log(tvol)^2) + log(tspd), family=binomial(link = "cloglog"), data = obw.model.data)  #Fit regression model
summary(obw.glm)
dev(obw.glm)
#save(bw.glm,file="output/bw_glm")

owc.glm <- glm(formula = coll ~ log(egk) + log(tvol) + I(log(tvol)^2) + log(tspd), family=binomial(link = "cloglog"), data = owc.model.data)  #Fit regression model
summary(owc.glm)
dev(owc.glm)
#save(wc.glm,file="output/wc_glm")

ocb.glm <- glm(formula = coll ~ log(egk) + log(tvol) + I(log(tvol)^2) + log(tspd), family=binomial(link = "cloglog"), data = ocb.model.data)  #Fit regression model
summary(ocb.glm)
dev(ocb.glm)
#save(cb.glm,file="output/cb_glm")

obwc.glm <- glm(formula = coll ~ log(egk) + log(tvol) + I(log(tvol)^2) + log(tspd), family=binomial(link = "cloglog"), data = obwc.model.data)  #Fit regression model
summary(obwc.glm)
dev(obwc.glm)
#save(bwc.glm,file="output/bwc_glm")

save(b.glm,w.glm,c.glm,o.glm,ob.glm,ow.glm,oc.glm,obw.glm,owc.glm,ocb.glm,obwc.glm,file="output/glms")

#Make predictions
preds <- as.data.table(cbind("uid"=wv.data$uid,"collrisk"=predict(o.glm, type="response")))

# b.preds <- as.data.table(cbind("uid"=b.model.data$uid,"collrisk"=predict(b.glm, type="response")))
# w.preds <- as.data.table(cbind("uid"=w.model.data$uid,"collrisk"=predict(w.glm, type="response")))
# c.preds <- as.data.table(cbind("uid"=c.model.data$uid,"collrisk"=predict(c.glm, type="response")))
# bw.preds <- as.data.table(cbind("uid"=bw.model.data$uid,"collrisk"=predict(bw.glm, type="response")))
# wc.preds <- as.data.table(cbind("uid"=wc.model.data$uid,"collrisk"=predict(wc.glm, type="response")))
# cb.preds <- as.data.table(cbind("uid"=cb.model.data$uid,"collrisk"=predict(cb.glm, type="response")))
# bwc.preds <- as.data.table(cbind("uid"=bwc.model.data$uid,"collrisk"=predict(bwc.glm, type="response")))

id <- c("ob","ow","oc","obw","owc","ocb","obwc")

for(i in id){
  #uid <- mget(paste0(i,".model.data$uid"))
  #assign(paste0(i,"_collrisk"),predict(get(paste0(i,".glm")), type="response")
  preds <- cbind(preds,predict(get(paste0(i,".glm")), type="response"))
  colnames(preds)[length(colnames(preds))] <- paste0(i,"_collrisk")
}
colnames(preds)[2] <- "o_collrisk"

save(preds, file="output/preds")

#Write predictions to database
drv <- dbDriver("PostgreSQL")  #Specify a driver for postgreSQL type database
con <- dbConnect(drv, dbname="qaeco_spatial", user="qaeco", password="Qpostgres15", host="boab.qaeco.com", port="5432")  #Connection to database server on Boab

dbWriteTable(con, c("gis_victoria", "vic_nogeom_roads_egkcollrisk"), value = preds[,.(uid,collrisk)], row.names=FALSE, overwrite=TRUE)

dbWriteTable(con, c("gis_victoria", "vic_nogeom_roads_egkcollrisk_b"), value = preds[,.(uid,"collrisk"=ob_collrisk)], row.names=FALSE, overwrite=TRUE)
dbWriteTable(con, c("gis_victoria", "vic_nogeom_roads_egkcollrisk_w"), value = preds[,.(uid,"collrisk"=ow_collrisk)], row.names=FALSE, overwrite=TRUE)
dbWriteTable(con, c("gis_victoria", "vic_nogeom_roads_egkcollrisk_c"), value = preds[,.(uid,"collrisk"=oc_collrisk)], row.names=FALSE, overwrite=TRUE)

dbWriteTable(con, c("gis_victoria", "vic_nogeom_roads_egkcollrisk_bw"), value = preds[,.(uid,"collrisk"=obw_collrisk)], row.names=FALSE, overwrite=TRUE)
dbWriteTable(con, c("gis_victoria", "vic_nogeom_roads_egkcollrisk_wc"), value = preds[,.(uid,"collrisk"=owc_collrisk)], row.names=FALSE, overwrite=TRUE)
dbWriteTable(con, c("gis_victoria", "vic_nogeom_roads_egkcollrisk_cb"), value = preds[,.(uid,"collrisk"=ocb_collrisk)], row.names=FALSE, overwrite=TRUE)

dbWriteTable(con, c("gis_victoria", "vic_nogeom_roads_egkcollrisk_bwc"), value = preds[,.(uid,"collrisk"=obwc_collrisk)], row.names=FALSE, overwrite=TRUE)

#Validate predictions with each independent dataset
val.preds <- data.frame("o"=rep(NA,3),"ob"=rep(NA,3),"ow"=rep(NA,3),"oc"=rep(NA,3),"obw"=rep(NA,3),"owc"=rep(NA,3),"ocb"=rep(NA,3))
row.names(val.preds) <- c("b","w","c")

o.val.b <- summary(glm(bendigo.data$coll ~ predict(o.glm, bendigo.data, type="link"), family = binomial(link = "cloglog")))
o.val.w <- summary(glm(western.data$coll ~ predict(o.glm, western.data, type="link"), family = binomial(link = "cloglog")))
o.val.c <- summary(glm(crashstats.data$coll ~ predict(o.glm, crashstats.data, type="link"), family = binomial(link = "cloglog")))

val.preds[1,1] <- roc(bendigo.data$coll, predict(o.glm, bendigo.data, type="response"))
val.preds[2,1] <- roc(western.data$coll, predict(o.glm, western.data, type="response"))
val.preds[3,1] <- roc(crashstats.data$coll, predict(o.glm, crashstats.data, type="response"))

ob.val.w <- summary(glm(western.data$coll ~ predict(ob.glm, western.data, type="link"), family = binomial(link = "cloglog")))
ob.val.c <- summary(glm(crashstats.data$coll ~ predict(ob.glm, crashstats.data, type="link"), family = binomial(link = "cloglog")))

val.preds[2,2] <- roc(western.data$coll, predict(ob.glm, western.data, type="response"))
val.preds[3,2] <- roc(crashstats.data$coll, predict(ob.glm, crashstats.data, type="response"))

ow.val.b <- summary(glm(bendigo.data$coll ~ predict(ow.glm, bendigo.data, type="link"), family = binomial(link = "cloglog")))
ow.val.c <- summary(glm(crashstats.data$coll ~ predict(ow.glm, crashstats.data, type="link"), family = binomial(link = "cloglog")))

val.preds[1,3] <- roc(bendigo.data$coll, predict(ow.glm, bendigo.data, type="response"))
val.preds[3,3] <- roc(crashstats.data$coll, predict(ow.glm, crashstats.data, type="response"))

oc.val.b <- summary(glm(bendigo.data$coll ~ predict(oc.glm, bendigo.data, type="link"), family = binomial(link = "cloglog")))
oc.val.w <- summary(glm(western.data$coll ~ predict(oc.glm, western.data, type="link"), family = binomial(link = "cloglog")))

val.preds[1,4] <- roc(bendigo.data$coll, predict(oc.glm, bendigo.data, type="response"))
val.preds[2,4] <- roc(western.data$coll, predict(oc.glm, western.data, type="response"))

obw.val.c <- summary(glm(crashstats.data$coll ~ predict(obw.glm, crashstats.data, type="link"), family = binomial(link = "cloglog")))

val.preds[3,5] <- roc(crashstats.data$coll, predict(obw.glm, crashstats.data, type="response"))

owc.val.b <- summary(glm(bendigo.data$coll ~ predict(owc.glm, bendigo.data, type="link"), family = binomial(link = "cloglog")))

val.preds[1,6] <- roc(bendigo.data$coll, predict(owc.glm, bendigo.data, type="response"))

ocb.val.w <- summary(glm(western.data$coll ~ predict(ocb.glm, western.data, type="link"), family = binomial(link = "cloglog")))

val.preds[2,7] <- roc(western.data$coll, predict(ocb.glm, western.data, type="response"))

save(val.preds,o.val.b,o.val.w,o.val.c,ob.val.w,ob.val.c,ow.val.b,ow.val.c,oc.val.b,oc.val.w,obw.val.c,owc.val.b,ocb.val.w,file="output/vals")
write.csv(val.preds,file="output/val_preds.csv")

#Validate predictions with aggregated insurance data
iag.data <- as.data.table(read.csv("data/iag_kang_coll.csv")) #Load aggregated independent data for validation
data.towns <- iag.data[,.N,by="NA_TOWN"]
setnames(data.towns,c("towns","ncoll"))
setkey(data.towns,towns)

drv <- dbDriver("PostgreSQL")  #Specify a driver for postgreSQL type database
con <- dbConnect(drv, dbname="qaeco_spatial", user="qaeco", password="Qpostgres15", host="boab.qaeco.com", port="5432")  #Connection to database server on Boab

#Original predictions
o.preds.towns <- as.data.table(dbGetQuery(con,"
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
setkey(o.preds.towns,towns)

val.data.towns <- merge(o.preds.towns[,sum(exp(collrisk)),by="towns"],data.towns, by="towns", all.x=TRUE)
val.data.towns$ncoll[is.na(val.data.towns$ncoll)] <- 0
colnames(val.data.towns)[2] <- "expcoll"

#no.towns.coll <- val.data.towns[!data.towns,]
#no.match.towns.coll <- data.towns[!val.data.towns,]

val.data.iag <- na.omit(val.data.towns)

val.data.iag$nyears <- 3

o.val.iag <- summary(glm(formula=ncoll ~ log(expcoll/5) + offset(log(nyears)), data=val.data.iag, family=poisson))

val.iag.dev <- dev(glm(formula=ncoll ~ log(expcoll/5) + offset(log(nyears)), data=val.data.iag, family=poisson))

#Predictions with Bendigo data
ob.preds.towns <- as.data.table(dbGetQuery(con,"
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
setkey(ob.preds.towns,towns)

val.data.towns <- merge(ob.preds.towns[,sum(exp(collrisk)),by="towns"],data.towns, by="towns", all.x=TRUE)
val.data.towns$ncoll[is.na(val.data.towns$ncoll)] <- 0
colnames(val.data.towns)[2] <- "expcoll"

#no.towns.coll <- val.data.towns[!data.towns,]
#no.match.towns.coll <- data.towns[!val.data.towns,]

val.data.iag <- na.omit(val.data.towns)

val.data.iag$nyears <- 3

ob.val.iag <- summary(glm(formula=ncoll ~ log(expcoll/5) + offset(log(nyears)), data=val.data.iag, family=poisson))

val.iag.dev[2] <- dev(glm(formula=ncoll ~ log(expcoll/5) + offset(log(nyears)), data=val.data.iag, family=poisson))


#Predictions with Western data
ow.preds.towns <- as.data.table(dbGetQuery(con,"
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
setkey(ow.preds.towns,towns)

val.data.towns <- merge(ow.preds.towns[,sum(exp(collrisk)),by="towns"],data.towns, by="towns", all.x=TRUE)
val.data.towns$ncoll[is.na(val.data.towns$ncoll)] <- 0
colnames(val.data.towns)[2] <- "expcoll"

#no.towns.coll <- val.data.towns[!data.towns,]
#no.match.towns.coll <- data.towns[!val.data.towns,]

val.data.iag <- na.omit(val.data.towns)

val.data.iag$nyears <- 3

ow.val.iag <- summary(glm(formula=ncoll ~ log(expcoll/5) + offset(log(nyears)), data=val.data.iag, family=poisson))

val.iag.dev[3] <- dev(glm(formula=ncoll ~ log(expcoll/5) + offset(log(nyears)), data=val.data.iag, family=poisson))


#Predictions with Crashstats data
oc.preds.towns <- as.data.table(dbGetQuery(con,"
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
setkey(oc.preds.towns,towns)

val.data.towns <- merge(oc.preds.towns[,sum(exp(collrisk)),by="towns"],data.towns, by="towns", all.x=TRUE)
val.data.towns$ncoll[is.na(val.data.towns$ncoll)] <- 0
colnames(val.data.towns)[2] <- "expcoll"

#no.towns.coll <- val.data.towns[!data.towns,]
#no.match.towns.coll <- data.towns[!val.data.towns,]

val.data.iag <- na.omit(val.data.towns)

val.data.iag$nyears <- 3

oc.val.iag <- summary(glm(formula=ncoll ~ log(expcoll/5) + offset(log(nyears)), data=val.data.iag, family=poisson))

val.iag.dev[4] <- dev(glm(formula=ncoll ~ log(expcoll/5) + offset(log(nyears)), data=val.data.iag, family=poisson))


#Predictions with Bendigo & Western data
obw.preds.towns <- as.data.table(dbGetQuery(con,"
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
setkey(obw.preds.towns,towns)

val.data.towns <- merge(obw.preds.towns[,sum(exp(collrisk)),by="towns"],data.towns, by="towns", all.x=TRUE)
val.data.towns$ncoll[is.na(val.data.towns$ncoll)] <- 0
colnames(val.data.towns)[2] <- "expcoll"

#no.towns.coll <- val.data.towns[!data.towns,]
#no.match.towns.coll <- data.towns[!val.data.towns,]

val.data.iag <- na.omit(val.data.towns)

val.data.iag$nyears <- 3

obw.val.iag <- summary(glm(formula=ncoll ~ log(expcoll/5) + offset(log(nyears)), data=val.data.iag, family=poisson))

val.iag.dev[5] <- dev(glm(formula=ncoll ~ log(expcoll/5) + offset(log(nyears)), data=val.data.iag, family=poisson))


#Predictions with Western data & Crashstats
owc.preds.towns <- as.data.table(dbGetQuery(con,"
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
setkey(owc.preds.towns,towns)

val.data.towns <- merge(owc.preds.towns[,sum(exp(collrisk)),by="towns"],data.towns, by="towns", all.x=TRUE)
val.data.towns$ncoll[is.na(val.data.towns$ncoll)] <- 0
colnames(val.data.towns)[2] <- "expcoll"

#no.towns.coll <- val.data.towns[!data.towns,]
#no.match.towns.coll <- data.towns[!val.data.towns,]

val.data.iag <- na.omit(val.data.towns)

val.data.iag$nyears <- 3

owc.val.iag <- summary(glm(formula=ncoll ~ log(expcoll/5) + offset(log(nyears)), data=val.data.iag, family=poisson))

val.iag.dev[6] <- dev(glm(formula=ncoll ~ log(expcoll/5) + offset(log(nyears)), data=val.data.iag, family=poisson))


#Predictions with Crashstats & Bendigo
ocb.preds.towns <- as.data.table(dbGetQuery(con,"
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
setkey(ocb.preds.towns,towns)

val.data.towns <- merge(ocb.preds.towns[,sum(exp(collrisk)),by="towns"],data.towns, by="towns", all.x=TRUE)
val.data.towns$ncoll[is.na(val.data.towns$ncoll)] <- 0
colnames(val.data.towns)[2] <- "expcoll"

#no.towns.coll <- val.data.towns[!data.towns,]
#no.match.towns.coll <- data.towns[!val.data.towns,]

val.data.iag <- na.omit(val.data.towns)

val.data.iag$nyears <- 3

ocb.val.iag <- summary(glm(formula=ncoll ~ log(expcoll/5) + offset(log(nyears)), data=val.data.iag, family=poisson))

val.iag.dev[7] <- dev(glm(formula=ncoll ~ log(expcoll/5) + offset(log(nyears)), data=val.data.iag, family=poisson))


#Predictions with Bendigo, Western & Crashstats
obwc.preds.towns <- as.data.table(dbGetQuery(con,"
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
setkey(obwc.preds.towns,towns)

val.data.towns <- merge(obwc.preds.towns[,sum(exp(collrisk)),by="towns"],data.towns, by="towns", all.x=TRUE)
val.data.towns$ncoll[is.na(val.data.towns$ncoll)] <- 0
colnames(val.data.towns)[2] <- "expcoll"

#no.towns.coll <- val.data.towns[!data.towns,]
#no.match.towns.coll <- data.towns[!val.data.towns,]

val.data.iag <- na.omit(val.data.towns)

val.data.iag$nyears <- 3

obwc.val.iag <- summary(glm(formula=ncoll ~ log(expcoll/5) + offset(log(nyears)), data=val.data.iag, family=poisson))

val.iag.dev[8] <- dev(glm(formula=ncoll ~ log(expcoll/5) + offset(log(nyears)), data=val.data.iag, family=poisson))

save(val.iag.dev,o.val.iag,ob.val.iag,ow.val.iag,oc.val.iag,obw.val.iag,owc.val.iag,ocb.val.iag,obwc.val.iag,file="output/glm_sums_iag")
