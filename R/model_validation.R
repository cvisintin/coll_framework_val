require(data.table)
require(plyr)

"roc" <- function (obsdat, preddat){
  if (length(obsdat) != length(preddat)) 
    stop("obs and preds must be equal lengths")
  n.x <- length(obsdat[obsdat == 0])
  n.y <- length(obsdat[obsdat == 1])
  xy <- c(preddat[obsdat == 0], preddat[obsdat == 1])
  rnk <- rank(xy)
  roc <- ((n.x * n.y) + ((n.x * (n.x + 1))/2) - sum(rnk[1:n.x]))/(n.x * n.y)
  return(round(roc, 4))
}

"dev" <- function (model){
  paste0("% Deviance Explained: ",round(((model$null.deviance - model$deviance)/model$null.deviance)*100,2))
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

#Load aggregated independent data for validation
iag.data <- as.data.table(read.csv("data/iag_kang_coll.csv"))
data.towns <- iag.data[,.N,by="NA_TOWN"]
setnames(data.towns,c("towns","ncoll"))
setkey(data.towns,towns)

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

w.glm <- glm(formula = coll ~ log(egk) + log(tvol) + I(log(tvol)^2) + log(tspd), family=binomial(link = "cloglog"), data = w.model.data)  #Fit regression model
summary(w.glm)  #Examine fit of regression model
dev(w.glm)  #Report reduction in deviance

c.glm <- glm(formula = coll ~ log(egk) + log(tvol) + I(log(tvol)^2) + log(tspd), family=binomial(link = "cloglog"), data = c.model.data)  #Fit regression model
summary(c.glm)  #Examine fit of regression model
dev(c.glm)  #Report reduction in deviance

bw.glm <- glm(formula = coll ~ log(egk) + log(tvol) + I(log(tvol)^2) + log(tspd), family=binomial(link = "cloglog"), data = bw.model.data)  #Fit regression model
summary(bw.glm)  #Examine fit of regression model
dev(bw.glm)  #Report reduction in deviance

wc.glm <- glm(formula = coll ~ log(egk) + log(tvol) + I(log(tvol)^2) + log(tspd), family=binomial(link = "cloglog"), data = wc.model.data)  #Fit regression model
summary(wc.glm)  #Examine fit of regression model
dev(wc.glm)  #Report reduction in deviance

cb.glm <- glm(formula = coll ~ log(egk) + log(tvol) + I(log(tvol)^2) + log(tspd), family=binomial(link = "cloglog"), data = cb.model.data)  #Fit regression model
summary(cb.glm)  #Examine fit of regression model
dev(cb.glm)  #Report reduction in deviance

bwc.glm <- glm(formula = coll ~ log(egk) + log(tvol) + I(log(tvol)^2) + log(tspd), family=binomial(link = "cloglog"), data = bwc.model.data)  #Fit regression model
summary(bwc.glm)  #Examine fit of regression model
dev(bwc.glm)  #Report reduction in deviance

#Make predictions
b.preds <- as.data.table(cbind("uid"=b.model.data$uid,"collrisk"=predict(b.glm, type="response")))
w.preds <- as.data.table(cbind("uid"=w.model.data$uid,"collrisk"=predict(w.glm, type="response")))
c.preds <- as.data.table(cbind("uid"=c.model.data$uid,"collrisk"=predict(c.glm, type="response")))
bw.preds <- as.data.table(cbind("uid"=bw.model.data$uid,"collrisk"=predict(bw.glm, type="response")))
wc.preds <- as.data.table(cbind("uid"=wc.model.data$uid,"collrisk"=predict(wc.glm, type="response")))
cb.preds <- as.data.table(cbind("uid"=cb.model.data$uid,"collrisk"=predict(cb.glm, type="response")))
bwc.preds <- as.data.table(cbind("uid"=bwc.model.data$uid,"collrisk"=predict(bwc.glm, type="response")))

#Validate models
val.b <- summary(glm(b.model.data$coll ~ predict(coll.glm, b.model.data, type="link"), family = binomial(link = "cloglog")))
val.w <- summary(glm(w.model.data$coll ~ predict(coll.glm, w.model.data, type="link"), family = binomial(link = "cloglog")))
val.c <- summary(glm(c.model.data$coll ~ predict(coll.glm, c.model.data, type="link"), family = binomial(link = "cloglog")))


b.val.w <- summary(glm(w.model.data$coll ~ predict(b.glm, w.model.data, type="link"), family = binomial(link = "cloglog")))
b.val.c <- summary(glm(c.model.data$coll ~ predict(b.glm, c.model.data, type="link"), family = binomial(link = "cloglog")))

w.val.b <- summary(glm(b.model.data$coll ~ predict(w.glm, b.model.data, type="link"), family = binomial(link = "cloglog")))
w.val.c <- summary(glm(c.model.data$coll ~ predict(w.glm, c.model.data, type="link"), family = binomial(link = "cloglog")))

c.val.b <- summary(glm(b.model.data$coll ~ predict(c.glm, b.model.data, type="link"), family = binomial(link = "cloglog")))
c.val.w <- summary(glm(w.model.data$coll ~ predict(c.glm, w.model.data, type="link"), family = binomial(link = "cloglog")))


bw.val.c <- summary(glm(c.model.data$coll ~ predict(bw.glm, c.model.data, type="link"), family = binomial(link = "cloglog")))

wc.val.b <- summary(glm(b.model.data$coll ~ predict(wc.glm, b.model.data, type="link"), family = binomial(link = "cloglog")))

cb.val.w <- summary(glm(w.model.data$coll ~ predict(cb.glm, w.model.data, type="link"), family = binomial(link = "cloglog")))