require(ggplot2)
require(plyr)
require(plotROC)
require(reshape)
require(foreach)
require(xtable)

load(file="data/vic_coll_model_data")
load(file="data/vic_coll_glm")
load("data/all_data")
load(file="output/glms")
load(file="output/preds")
load(file="output/vals")
load(file="output/glm_sums_iag")

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
  round(((model$null.deviance - model$deviance)/model$null.deviance)*100,2)
}

"invcloglog" <- function (x) {1-exp(-exp(x))}

"round_df" <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[,nums] <- round(df[,nums], digits = digits)
  
  (df)
}

"range0_1" <- function(x){(x-min(x))/(max(x)-min(x))}

data.id <- factor(c("o","ob","ow","oc","obw","owc","ocb","obwc"), levels=c("o","ob","ow","oc","obw","owc","ocb","obwc"))

#create dataframe for mean predicted rates
#preds.m <- data.frame(data.id,"collrisk_m"=apply(preds[,-1],2,mean),"collrisk_sd"=apply(preds[,-1],2,sd),"collrisk_min"=apply(preds[,-1],2,min),"collrisk_max"=apply(preds[,-1],2,max))
#preds.m$total.coll <- as.numeric(total.coll)

#preds.m2 <- melt(preds[sample(nrow(preds), 100), ], id.vars=c("uid"))

#plot predictions with varying combinations of data
# png('figs/preds.png', pointsize = 6, res=300, width = 900, height = 900, bg='transparent')
# ggplot() +
#   geom_point(data=preds.m, aes(x=data.id, y=collrisk_m)) +
#   #geom_pointrange(data=preds.m, aes(x=data.id, y=collrisk_m, ymin=collrisk_m-collrisk_sd, ymax=collrisk_m+collrisk_sd)) +
#   geom_text(data=preds.m, aes(x=data.id, y=collrisk_m, label=total.coll),hjust=0.5, vjust=-1, size = 2.0, inherit.aes=FALSE) +
#   #geom_segment(aes(x = 0, y = 0, xend = .04, yend = .04), linetype=2, size=0.1, inherit.aes=FALSE) +
#   #coord_flip() +
#   scale_y_continuous(breaks=seq(0.0065,0.0095,by=.0005), expand = c(0, 0), lim=c(0.0065,0.0095)) +
#   ylab("Predicted mean rate (across entire road network)") +
#   xlab("Data combinations used for modelling") +
#   theme_bw() +
#   theme(plot.margin=unit(c(.5,0,.1,.1),"cm")) +
#   theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
#   theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
#   theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
#   theme(text = element_text(size = 8))
# dev.off()

#create table of model fits for original collision datasets
mod.sums <- data.frame(c("Wildlife Victoria","","","","","City of Bendigo","","","","","Western District","","","","","Crashstats","","","",""),
                             rep(c("Intercept","EGK","TVOL","TVOL2","TSPD"),4),
                             c(coef(coll.glm),coef(b.glm),coef(w.glm),coef(c.glm)),
                             c(as.numeric(summary(o.glm)$coefficients[, 2]),
                               as.numeric(summary(b.glm)$coefficients[, 2]),
                               as.numeric(summary(w.glm)$coefficients[, 2]),
                               as.numeric(summary(c.glm)$coefficients[, 2])
                               ),
                             c(as.numeric(summary(o.glm)$coefficients[, 3]),
                               as.numeric(summary(b.glm)$coefficients[, 3]),
                               as.numeric(summary(w.glm)$coefficients[, 3]),
                               as.numeric(summary(c.glm)$coefficients[, 3])
                               ),
                             c(as.numeric(summary(o.glm)$coefficients[, 4]),
                               as.numeric(summary(b.glm)$coefficients[, 4]),
                               as.numeric(summary(w.glm)$coefficients[, 4]),
                               as.numeric(summary(c.glm)$coefficients[, 4])
                               ),
                             c(dev(o.glm),"","","","",dev(b.glm),"","","","",dev(w.glm),"","","","",dev(c.glm),"","","","")
)

colnames(mod.sums) <- c("Dataset","Variable","Coefficient","Standard Error","Z-value","Pr(Z)","Deviance Explained")

print(data.frame(lapply(mod.sums, function(y) if(is.numeric(y)) round(y, 3) else y)))

latex.data <- data.frame(lapply(mod.sums, function(y) if(is.numeric(y)) round(y, 4) else y))
latex.data$Pr.Z. <- lapply(latex.data$Pr.Z., function(y) if(y<=.0001) '$<$.0001*' else y)
print(xtable(latex.data), include.rownames=FALSE, sanitize.text.function=function(x){x}, floating=FALSE)

write.csv(mod.sums, file="output/mod_sums.csv", row.names=FALSE)

#plot results from original glm models using only validation datasets
occ <- foreach(i = c("o","b","w","c"), .combine=rbind) %do% {
  model <- get(paste0(i,".glm"))
  occ.range <- seq(0,1,.001)[-c(1,length(seq(0,1,.001)))]
  occ.fit <- predict.glm(model,data.frame(egk=occ.range,tvol=mean(model$data$tvol),tspd=mean(model$data$tspd)),type="response",se.fit=TRUE)
  data.frame(x=occ.range,y=range0_1(occ.fit[["fit"]]),ymin=range0_1(occ.fit[["fit"]]-1.96*occ.fit[["se.fit"]]),ymax=range0_1(occ.fit[["fit"]]+1.96*occ.fit[["se.fit"]]),id=i)
}
occ$id <- substring(paste0(occ$id,"         "),1,15)
occ$id <- factor(occ$id,levels=c("o       ","b              ","w              ","c              "))


tiff('figs/occ.tif', pointsize = 10, compression = "lzw", res=300, width = 900, height = 1000)
ggplot(occ, aes(x=x,y=y,ymin=ymin,ymax=ymax,group=id)) +
  geom_line(size=0.8, aes(colour=id, linetype=id)) +
  #geom_ribbon(alpha=0.3) +
  ylab("RELATIVE COLLISION RATE (RESCALED)") +
  xlab("LIKELIHOOD OF SPECIES OCCURRENCE") +
  theme_bw() +
  theme(legend.position="top", legend.title = element_blank(), legend.margin=margin(0,0,0,0), legend.box.margin=margin(5,-10,-10,0)) +
  theme(plot.margin=unit(c(-.3,.5,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.12)) +
  #scale_y_continuous(breaks=seq(0,.12,by=.02), expand = c(0, 0), lim=c(0,.12)) +
  scale_x_continuous(breaks=seq(0,1,by=.1), expand = c(0, 0), lim=c(0,1))
dev.off()


tvol <- foreach(i = c("o","b","w","c"), .combine=rbind) %do% {
  model <- get(paste0(i,".glm"))
  tvol.range <- seq(0,40000,20)[-c(1,length(seq(0,40000,20)))]
  tvol.fit <- predict.glm(model,data.frame(egk=mean(model$data$egk),tvol=tvol.range,tspd=mean(model$data$tspd)),type="response",se.fit=TRUE)
  data.frame(x=tvol.range,y=range0_1(tvol.fit[["fit"]]),ymin=range0_1(tvol.fit[["fit"]]-1.96*tvol.fit[["se.fit"]]),ymax=range0_1(tvol.fit[["fit"]]+1.96*tvol.fit[["se.fit"]]),id=i)
}
tvol$id <- substring(paste0(tvol$id,"                 "),1,15)
tvol$id <- factor(tvol$id,levels=c("o              ","b              ","w              ","c              "))
  
tiff('figs/tvol.tif', pointsize = 10, compression = "lzw", res=300, width = 900, height = 1000)
ggplot(tvol, aes(x=x/1000,y=y,ymin=ymin,ymax=ymax,group=id)) +
  geom_line(size=0.3, aes(colour=id, linetype=id)) +
  #geom_ribbon(alpha=0.3) +
  ylab("Likelihood of Collision") +
  xlab("Traffic Volume (1000 vehicles/day)") +
  theme_bw() +
  theme(legend.position="top", legend.title = element_blank(), legend.margin=margin(0,0,0,0), legend.box.margin=margin(5,-10,-10,0)) +
  theme(plot.margin=unit(c(-.3,.5,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  scale_x_continuous(breaks=seq(0,40,by=5), expand = c(0, 0), lim=c(0,40))
dev.off()

tspd <- foreach(i = c("o","b","w","c"), .combine=rbind) %do% {
  model <- get(paste0(i,".glm"))
  tspd.range <- seq(40,110,.1)[-c(1,length(seq(40,110,.1)))]
  tspd.fit <- predict.glm(model,data.frame(egk=mean(model$data$egk),tvol=mean(model$data$tvol),tspd=tspd.range),type="response",se.fit=TRUE)
  data.frame(x=tspd.range,y=range0_1(tspd.fit[["fit"]]),ymin=range0_1(tspd.fit[["fit"]]-1.96*tspd.fit[["se.fit"]]),ymax=range0_1(tspd.fit[["fit"]]+1.96*tspd.fit[["se.fit"]]),id=i)
}
tspd$id <- substring(paste0(tspd$id,"                 "),1,15)
tspd$id <- factor(tspd$id,levels=c("o              ","b              ","w              ","c              "))
  
tiff('figs/tspd.tif', pointsize = 10, compression = "lzw", res=300, width = 900, height = 1000)
ggplot(tspd, aes(x=x,y=y,ymin=ymin,ymax=ymax,group=id)) +
  geom_line(size=0.3, aes(colour=id, linetype=id)) +
  #geom_ribbon(alpha=0.3) +
  ylab("Likelihood of Collision") +
  xlab("Traffic Speed (km/hour)") +
  theme_bw() +
  theme(legend.position="top", legend.title = element_blank(), legend.margin=margin(0,0,0,0), legend.box.margin=margin(5,-10,-10,0)) +
  theme(plot.margin=unit(c(-.3,.5,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  scale_x_continuous(breaks=seq(40,110,by=10), expand = c(0, 0), lim=c(40,110))
dev.off()


#create dataframe for calibration metrics
val.df <- data.frame("id"=rep(NA,12),"coef"=rep(NA,12),"coef_err"=rep(NA,12))
val.df[1,] <- c("o-b",coef(o.val.b)[2,1],coef(o.val.b)[2,2])
val.df[2,] <- c("o-w",coef(o.val.w)[2,1],coef(o.val.w)[2,2])
val.df[3,] <- c("o-c",coef(o.val.c)[2,1],coef(o.val.c)[2,2])
val.df[4,] <- c("ob-w",coef(ob.val.w)[2,1],coef(ob.val.w)[2,2])
val.df[5,] <- c("ob-c",coef(ob.val.c)[2,1],coef(ob.val.c)[2,2])
val.df[6,] <- c("ow-c",coef(ow.val.c)[2,1],coef(ow.val.c)[2,2])
val.df[7,] <- c("ow-b",coef(ow.val.b)[2,1],coef(ow.val.b)[2,2])
val.df[8,] <- c("oc-b",coef(oc.val.b)[2,1],coef(oc.val.b)[2,2])
val.df[9,] <- c("oc-w",coef(oc.val.w)[2,1],coef(oc.val.w)[2,2])
val.df[10,] <- c("obw-c",coef(obw.val.c)[2,1],coef(obw.val.c)[2,2])
val.df[11,] <- c("owc-b",coef(owc.val.b)[2,1],coef(owc.val.b)[2,2])
val.df[12,] <- c("ocb-w",coef(ocb.val.w)[2,1],coef(ocb.val.w)[2,2])
val.df$id <- factor(val.df$id, levels=c("o-b","o-w","o-c","ob-w","ob-c","ow-c","ow-b","oc-b","oc-w","obw-c","owc-b","ocb-w"))
val.df$coef <- as.numeric(val.df$coef)
val.df$coef_err <- as.numeric(val.df$coef_err)

#plot calibration with varying combinations of data
png('figs/calib.png', pointsize = 6, res=300, width = 1700, height = 900, bg='transparent')
ggplot() +
  #geom_smooth(data=plot.glm, aes(y=y,x=x), formula=y~log(x), method=glm, size = 0.2, colour='black', inherit.aes=FALSE) +
  #geom_line(data=val.df, aes(y=,x=)) +
  geom_pointrange(data=val.df, aes(x=id, y=coef, ymin=coef-coef_err, ymax=coef+coef_err), size=0.1) +
  #geom_text(data=plot.info, aes(x=median_p, y=prop_coll, label=count),hjust=-0.1, vjust=-1, size = 2.0, inherit.aes=FALSE) +
  geom_segment(aes(x = 0, y = 1, xend = Inf, yend = 1), linetype=2, size=0.1) +
  #geom_segment(aes(x = 0, y = mean(val.df[1:3,2]), xend = Inf, yend = mean(val.df[1:3,2])), linetype=2, size=0.1) +
  #coord_flip() +
  scale_y_continuous(breaks=seq(0.6,1.2,by=.1), expand = c(0, 0), lim=c(0.6,1.2)) +
  ylab("Calibration coefficient") +
  xlab("Data combinations used for modelling & validation") +
  theme_bw() +
  theme(plot.margin=unit(c(.5,0,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 10))
dev.off()

b.data <- bendigo.data
w.data <- western.data
c.data <- crashstats.data

yp_bins <- foreach(i = val.df$id, .combine=rbind) %do% {
  y <- eval(parse(text=paste0(strsplit(as.character(i), "-")[[1]][2],".data$coll")))
  #p <- predict(ob.glm, eval(parse(text=paste0(sub('-', '', i),".glm$data"))), type="response")
  p <- predict(get(paste0(strsplit(as.character(i), "-")[[1]][1],".glm")), eval(parse(text=paste0(strsplit(as.character(i), "-")[[1]][2],".data"))), type="response")
  
  p.b <- .bincode(p,seq(min(p),max(p),(max(p)-min(p))/10),include.lowest = TRUE)
  yp <- data.frame(y,p,p.b)
  calib <- glm(y~log(p), family=binomial(link=cloglog), data=yp)
  cbind(yp, "coef1"=calib$coefficients[1], "coef2"=calib$coefficients[2], "id"=as.character(i))
}

plotPal <- c("#77B9FF",
             "#598BBF",
             "#3B5C7F",
             "#BF544B",
             "#7F3832",
             "#9ABF78",
             "#667F50",
             "#BFAB64",
             "#7F7243",
             "#BF7EBD",
             "#FFA8FC",
             "#7F547E")

png('figs/calib2.png', pointsize = 6, res=300, width = 1300, height = 1000, bg='transparent')
ggplot() +
  #geom_smooth(data=plot.glm, aes(y=y,x=x), formula=y~log(x), method=glm, size = 0.2, colour='black', inherit.aes=FALSE) +
  geom_line(data=yp_bins, aes(y=invcloglog(coef1+coef2*log(p)),x=p, colour=id)) +
  #geom_pointrange(data=plot.info, aes(x=median_p, y=prop_coll, ymin=prop_lo, ymax=prop_hi), size = 0.2, inherit.aes=FALSE) +
  #geom_text(data=plot.info, aes(x=median_p, y=prop_coll, label=count),hjust=-0.1, vjust=-1, size = 2.0, inherit.aes=FALSE) +
  #geom_segment(aes(x = 0, y = 0, xend = 0.04, yend = 0.04), linetype=2, size=0.1, inherit.aes=FALSE) +
  #coord_flip() +
  ylab("Observed Rate (proportion in one year)") +
  xlab("Predicted Rate (proportion in one year)") +
  theme_bw() +
  scale_colour_manual(values=plotPal) +
  theme(plot.margin=unit(c(.5,0,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 10))
dev.off()

#create dataframe for iag calibration metrics
val.iag.df <- data.frame("id"=data.id,"coef"=rep(NA,8),"coef_err"=rep(NA,8),"dev"=val.iag.dev)
val.iag.df[1,2:3] <- c(coef(o.val.iag)[2,1],coef(o.val.iag)[2,2])
val.iag.df[2,2:3] <- c(coef(ob.val.iag)[2,1],coef(ob.val.iag)[2,2])
val.iag.df[3,2:3] <- c(coef(ow.val.iag)[2,1],coef(ow.val.iag)[2,2])
val.iag.df[4,2:3] <- c(coef(oc.val.iag)[2,1],coef(oc.val.iag)[2,2])
val.iag.df[5,2:3] <- c(coef(obw.val.iag)[2,1],coef(obw.val.iag)[2,2])
val.iag.df[6,2:3] <- c(coef(owc.val.iag)[2,1],coef(owc.val.iag)[2,2])
val.iag.df[7,2:3] <- c(coef(ocb.val.iag)[2,1],coef(ocb.val.iag)[2,2])
val.iag.df[8,2:3] <- c(coef(obwc.val.iag)[2,1],coef(obwc.val.iag)[2,2])
val.iag.df$coef <- as.numeric(val.iag.df$coef)
val.iag.df$coef_err <- as.numeric(val.iag.df$coef_err)
val.iag.df$dev <- as.numeric(val.iag.df$dev)
#val.iag.df$id <- factor(val.iag.df$id, levels=unlist(lapply(data.id, function(x) paste0(x,"-iag"))))

#plot iag calibration and performance with varying combinations of data
png('figs/calib_iag.png', pointsize = 6, res=300, width = 1700, height = 900, bg='transparent')
ggplot() +
  #geom_smooth(data=plot.glm, aes(y=y,x=x), formula=y~log(x), method=glm, size = 0.2, colour='black', inherit.aes=FALSE) +
  #geom_line(data=val.df, aes(y=,x=)) +
  geom_point(data=val.iag.df, aes(x=id, y=coef), size=1) +
  #geom_text(data=plot.info, aes(x=median_p, y=prop_coll, label=count),hjust=-0.1, vjust=-1, size = 2.0, inherit.aes=FALSE) +
  #geom_segment(aes(x = 0, y = 1, xend = Inf, yend = 1), linetype=2, size=0.1) +
  #geom_segment(aes(x = 0, y = mean(val.df[1:3,2]), xend = Inf, yend = mean(val.df[1:3,2])), linetype=2, size=0.1) +
  #coord_flip() +
  ylab("Calibration coefficient") +
  xlab("Data combinations used for modelling") +
  theme_bw() +
  theme(plot.margin=unit(c(.5,0,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 10))
dev.off()

png('figs/dev_iag.png', pointsize = 6, res=300, width = 1700, height = 900, bg='transparent')
ggplot() +
  #geom_smooth(data=plot.glm, aes(y=y,x=x), formula=y~log(x), method=glm, size = 0.2, colour='black', inherit.aes=FALSE) +
  #geom_line(data=val.df, aes(y=,x=)) +
  geom_point(data=val.iag.df, aes(x=id, y=dev), size=1) +
  #geom_text(data=plot.info, aes(x=median_p, y=prop_coll, label=count),hjust=-0.1, vjust=-1, size = 2.0, inherit.aes=FALSE) +
  #geom_segment(aes(x = 0, y = 1, xend = Inf, yend = 1), linetype=2, size=0.1) +
  #geom_segment(aes(x = 0, y = mean(val.df[1:3,2]), xend = Inf, yend = mean(val.df[1:3,2])), linetype=2, size=0.1) +
  #coord_flip() +
  ylab("Percent reduction in deviance") +
  xlab("Data combinations used for modelling") +
  theme_bw() +
  theme(plot.margin=unit(c(.5,0,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 10))
dev.off()

#######################################################################################
# 
# y <- coll.glm$data$y
# p <- predict(coll.glm, coll.glm$data, type="response")
# 
# p.b <- .bincode(p,seq(min(p),max(p),(max(p)-min(p))/10),include.lowest = TRUE)
# 
# yp_bins <- data.frame(y,p,p.b)
# 
# 
# plot.info <- ddply(yp_bins, ~p.b, summarise,
#                    count=length(y),
#                    prop_coll=sum(y)/length(y),
#                    prop_nocoll=(length(y)-sum(y))/length(y),
#                    prop_lo=binom.test(sum(y), length(y), 0.5)$conf.int[1],
#                    prop_hi=binom.test(sum(y), length(y), 0.5)$conf.int[2],
#                    median_p=median(p)
# )
# 
# calib <- glm(y~log(p), family=binomial(link=cloglog), data=yp_bins)
# roc <- roc(y,p)
# perform.glm <- rbind(calib_int=calib$coefficients[1], calib_slope=calib$coefficients[2], roc)
# colnames(perform.glm) <- "0.0"
# 
# png('figs/calib.png', pointsize = 6, res=300, width = 900, height = 900, bg='transparent')
# ggplot() +
#   #geom_smooth(data=plot.glm, aes(y=y,x=x), formula=y~log(x), method=glm, size = 0.2, colour='black', inherit.aes=FALSE) +
#   geom_line(data=yp_bins, aes(y=invcloglog(calib$coefficients[1]+calib$coefficients[2]*log(p)),x=p)) +
#   geom_pointrange(data=plot.info, aes(x=median_p, y=prop_coll, ymin=prop_lo, ymax=prop_hi), size = 0.2, inherit.aes=FALSE) +
#   geom_text(data=plot.info, aes(x=median_p, y=prop_coll, label=count),hjust=-0.1, vjust=-1, size = 2.0, inherit.aes=FALSE) +
#   geom_segment(aes(x = 0, y = 0, xend = .04, yend = .04), linetype=2, size=0.1, inherit.aes=FALSE) +
#   #coord_flip() +
#   ylab("Observed Rate (proportion in one year)") +
#   xlab("Predicted Rate (proportion in one year)") +
#   theme_bw() +
#   theme(plot.margin=unit(c(.5,0,.1,.1),"cm")) +
#   theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
#   theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
#   theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
#   theme(text = element_text(size = 10))
# dev.off()

#plot results from original glm models using only validation datasets for completion talk
occ <- foreach(i = c("o","b","w","c"), .combine=rbind) %do% {
  model <- get(paste0(i,".glm"))
  occ.range <- seq(0,1,.001)[-c(1,length(seq(0,1,.001)))]
  occ.fit <- predict.glm(model,data.frame(egk=occ.range,tvol=mean(model$data$tvol),tspd=mean(model$data$tspd)),type="response",se.fit=TRUE)
  data.frame(x=occ.range,y=range0_1(occ.fit[["fit"]]),ymin=range0_1(occ.fit[["fit"]]-1.96*occ.fit[["se.fit"]]),ymax=range0_1(occ.fit[["fit"]]+1.96*occ.fit[["se.fit"]]),id=i)
}
occ$id <- paste0(occ$id,"         ")
occ$id <- factor(occ$id,levels=occ$id)

png('/home/casey/Research/Projects/PhD_Thesis/completion_talk/graphics/occ_kv.png', pointsize = 16, res=300, width = 1000, height = 900)
ggplot(occ, aes(x=x,y=y,ymin=ymin,ymax=ymax,group=id)) +
  geom_line(size=0.8, aes(colour=id, linetype=id)) +
  ylab("RELATIVE COLLISION RATE (RESCALED)") +
  xlab("LIKELIHOOD OF SPECIES OCCURRENCE") +
  theme_bw() +
  theme(legend.position="top", legend.title = element_blank(), legend.margin=margin(0,0,0,0), legend.box.margin=margin(5,-10,-10,0)) +
  theme(plot.margin=unit(c(-.3,.5,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.12)) +
  theme(text = element_text(size = 8)) +
  scale_x_continuous(breaks=seq(0,1,by=.1), expand = c(0, 0), lim=c(0,1))
dev.off()


tvol <- foreach(i = c("o","b","w","c"), .combine=rbind) %do% {
  model <- get(paste0(i,".glm"))
  tvol.range <- seq(0,40000,20)[-c(1,length(seq(0,40000,20)))]
  tvol.fit <- predict.glm(model,data.frame(egk=mean(model$data$egk),tvol=tvol.range,tspd=mean(model$data$tspd)),type="response",se.fit=TRUE)
  data.frame(x=tvol.range,y=range0_1(tvol.fit[["fit"]]),ymin=range0_1(tvol.fit[["fit"]]-1.96*tvol.fit[["se.fit"]]),ymax=range0_1(tvol.fit[["fit"]]+1.96*tvol.fit[["se.fit"]]),id=i)
}
tvol$id <- paste0(tvol$id,"         ")
tvol$id <- factor(tvol$id,levels=tvol$id)

png('/home/casey/Research/Projects/PhD_Thesis/completion_talk/graphics/tvol_kv.png', pointsize = 16, res=300, width = 1000, height = 900)
ggplot(tvol, aes(x=x/1000,y=y,ymin=ymin,ymax=ymax,group=id)) +
  geom_line(size=0.8, aes(colour=id, linetype=id)) +
  #geom_ribbon(alpha=0.3) +
  ylab("") +
  xlab("TRAFFIC VOLUME (1000 VEHICLES/DAY)") +
  theme_bw() +
  theme(legend.position="top", legend.title = element_blank(), legend.margin=margin(0,0,0,0), legend.box.margin=margin(5,-10,-10,0)) +
  theme(plot.margin=unit(c(-.3,.5,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 8)) +
  scale_x_continuous(breaks=seq(0,40,by=5), expand = c(0, 0), lim=c(0,40))
dev.off()

tspd <- foreach(i = c("o","b","w","c"), .combine=rbind) %do% {
  model <- get(paste0(i,".glm"))
  tspd.range <- seq(40,110,.1)[-c(1,length(seq(40,110,.1)))]
  tspd.fit <- predict.glm(model,data.frame(egk=mean(model$data$egk),tvol=mean(model$data$tvol),tspd=tspd.range),type="response",se.fit=TRUE)
  data.frame(x=tspd.range,y=range0_1(tspd.fit[["fit"]]),ymin=range0_1(tspd.fit[["fit"]]-1.96*tspd.fit[["se.fit"]]),ymax=range0_1(tspd.fit[["fit"]]+1.96*tspd.fit[["se.fit"]]),id=i)
}
tspd$id <- paste0(tspd$id,"         ")
tspd$id <- factor(tspd$id,levels=tspd$id)

png('/home/casey/Research/Projects/PhD_Thesis/completion_talk/graphics/tspd_kv.png', pointsize = 16, res=300, width = 1000, height = 900)
ggplot(tspd, aes(x=x,y=y,ymin=ymin,ymax=ymax,group=id)) +
  geom_line(size=0.8, aes(colour=id, linetype=id)) +
  #geom_ribbon(alpha=0.3) +
  ylab("") +
  xlab("TRAFFIC SPEED (KM/HOUR)") +
  theme_bw() +
  theme(legend.position="top", legend.title = element_blank(), legend.margin=margin(0,0,0,0), legend.box.margin=margin(5,-10,-10,0)) +
  theme(plot.margin=unit(c(-.3,.5,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 8)) +
  scale_x_continuous(breaks=seq(40,110,by=10), expand = c(0, 0), lim=c(40,110))
dev.off()

png('/home/casey/Research/Projects/PhD_Thesis/completion_talk/graphics/calib_iag.png', pointsize = 16, res=300, width = 1000, height = 900)
ggplot() +
  #geom_smooth(data=plot.glm, aes(y=y,x=x), formula=y~log(x), method=glm, size = 0.2, colour='black', inherit.aes=FALSE) +
  #geom_line(data=val.df, aes(y=,x=)) +
  geom_point(data=val.iag.df, aes(x=id, y=coef), size=1) +
  #geom_text(data=plot.info, aes(x=median_p, y=prop_coll, label=count),hjust=-0.1, vjust=-1, size = 2.0, inherit.aes=FALSE) +
  #geom_segment(aes(x = 0, y = 1, xend = Inf, yend = 1), linetype=2, size=0.1) +
  #geom_segment(aes(x = 0, y = mean(val.df[1:3,2]), xend = Inf, yend = mean(val.df[1:3,2])), linetype=2, size=0.1) +
  #coord_flip() +
  ylab("CALIBRATION COEFFICIENT") +
  xlab("DATA COMBINATIONS USED FOR MODELLING") +
  theme_bw() +
  theme(plot.margin=unit(c(.5,0,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 8))
dev.off()

png('/home/casey/Research/Projects/PhD_Thesis/completion_talk/graphics/dev_iag.png', pointsize = 16, res=300, width = 1000, height = 900)
ggplot() +
  #geom_smooth(data=plot.glm, aes(y=y,x=x), formula=y~log(x), method=glm, size = 0.2, colour='black', inherit.aes=FALSE) +
  #geom_line(data=val.df, aes(y=,x=)) +
  geom_point(data=val.iag.df, aes(x=id, y=dev), size=1) +
  #geom_text(data=plot.info, aes(x=median_p, y=prop_coll, label=count),hjust=-0.1, vjust=-1, size = 2.0, inherit.aes=FALSE) +
  #geom_segment(aes(x = 0, y = 1, xend = Inf, yend = 1), linetype=2, size=0.1) +
  #geom_segment(aes(x = 0, y = mean(val.df[1:3,2]), xend = Inf, yend = mean(val.df[1:3,2])), linetype=2, size=0.1) +
  #coord_flip() +
  ylab("PERCENT REDUCTION IN DEVIANCE") +
  xlab("DATA COMBINATIONS USED FOR MODELLING") +
  theme_bw() +
  theme(plot.margin=unit(c(.5,0,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 8))
dev.off()