require(ggplot2)
require(plyr)
require(plotROC)
require(reshape)
require(foreach)

load(file="data/vic_coll_model_data")
load(file="data/vic_coll_glm")
load("data/all_data")
load(file="output/glms")
load(file="output/preds")
load(file="output/vals")
load(file="output/glm_sums_iag")

roc <- function (obsdat, preddat){
  if (length(obsdat) != length(preddat)) 
    stop("obs and preds must be equal lengths")
  n.x <- length(obsdat[obsdat == 0])
  n.y <- length(obsdat[obsdat == 1])
  xy <- c(preddat[obsdat == 0], preddat[obsdat == 1])
  rnk <- rank(xy)
  roc <- ((n.x * n.y) + ((n.x * (n.x + 1))/2) - sum(rnk[1:n.x]))/(n.x * n.y)
  return(round(roc, 4))
}

invcloglog <- function (x) {1-exp(-exp(x))}

#create dataframe for mean predicted rates
data.id <- factor(c("o","ob","ow","oc","obw","owc","ocb","obwc"), levels=c("o","ob","ow","oc","obw","owc","ocb","obwc"))
preds.m <- data.frame(data.id,"collrisk_m"=apply(preds[,-1,with=FALSE],2,mean),"collrisk_sd"=apply(preds[,-1,with=FALSE],2,sd),"collrisk_min"=apply(preds[,-1,with=FALSE],2,min),"collrisk_max"=apply(preds[,-1,with=FALSE],2,max))
preds.m$total.coll <- as.numeric(total.coll)

#preds.m2 <- melt(preds[sample(nrow(preds), 100), ], id.vars=c("uid"))

#plot predictions with varying combinations of data
png('figs/preds.png', pointsize = 6, res=300, width = 900, height = 900, bg='transparent')
ggplot() +
  geom_point(data=preds.m, aes(x=data.id, y=collrisk_m)) +
  #geom_pointrange(data=preds.m, aes(x=data.id, y=collrisk_m, ymin=collrisk_m-collrisk_sd, ymax=collrisk_m+collrisk_sd)) +
  geom_text(data=preds.m, aes(x=data.id, y=collrisk_m, label=total.coll),hjust=0.5, vjust=-1, size = 2.0, inherit.aes=FALSE) +
  #geom_segment(aes(x = 0, y = 0, xend = .04, yend = .04), linetype=2, size=0.1, inherit.aes=FALSE) +
  #coord_flip() +
  scale_y_continuous(breaks=seq(0.0065,0.0095,by=.0005), expand = c(0, 0), lim=c(0.0065,0.0095)) +
  ylab("Predicted mean rate (across entire road network)") +
  xlab("Data combinations used for modelling") +
  theme_bw() +
  theme(plot.margin=unit(c(.5,0,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 8))
dev.off()

# ggplot() +
#   geom_line(data=preds.m2, aes(y=value,x=variable,group=uid)) +
#   #geom_pointrange(data=plot.info, aes(x=median_p, y=prop_coll, ymin=prop_lo, ymax=prop_hi), size = 0.2, inherit.aes=FALSE) +
#   #geom_text(data=plot.info, aes(x=median_p, y=prop_coll, label=count),hjust=-0.1, vjust=-1, size = 2.0, inherit.aes=FALSE) +
#   #geom_segment(aes(x = 0, y = 0, xend = .04, yend = .04), linetype=2, size=0.1, inherit.aes=FALSE) +
#   #coord_flip() +
#   ylab("Observed Rate (proportion in one year)") +
#   xlab("Predicted Rate (proportion in one year)") +
#   theme_bw() +
#   theme(plot.margin=unit(c(.5,0,.1,.1),"cm")) +
#   theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
#   theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
#   theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
#   theme(text = element_text(size = 10))

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
png('figs/calib.png', pointsize = 6, res=300, width = 1500, height = 900, bg='transparent')
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
  theme(text = element_text(size = 8))
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

png('figs/calib2.png', pointsize = 6, res=300, width = 1200, height = 900, bg='transparent')
ggplot() +
  #geom_smooth(data=plot.glm, aes(y=y,x=x), formula=y~log(x), method=glm, size = 0.2, colour='black', inherit.aes=FALSE) +
  geom_line(data=yp_bins, aes(y=invcloglog(coef1+coef2*log(p)),x=p, colour=id)) +
  #geom_pointrange(data=plot.info, aes(x=median_p, y=prop_coll, ymin=prop_lo, ymax=prop_hi), size = 0.2, inherit.aes=FALSE) +
  #geom_text(data=plot.info, aes(x=median_p, y=prop_coll, label=count),hjust=-0.1, vjust=-1, size = 2.0, inherit.aes=FALSE) +
  geom_segment(aes(x = 0, y = 0, xend = 0.35, yend = 0.35), linetype=2, size=0.1, inherit.aes=FALSE) +
  #coord_flip() +
  ylab("Observed Rate (proportion in one year)") +
  xlab("Predicted Rate (proportion in one year)") +
  theme_bw() +
  theme(plot.margin=unit(c(.5,0,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 10))
dev.off()

#create dataframe for iag calibration metrics
val.iag.df <- data.frame("id"=unlist(lapply(data.id, function(x) paste0(x,"-iag"))),"coef"=rep(NA,8),"coef_err"=rep(NA,8),"dev"=val.iag.dev)
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
val.iag.df$id <- factor(val.iag.df$id, levels=unlist(lapply(data.id, function(x) paste0(x,"-iag"))))

#plot iag calibration and performance with varying combinations of data
png('figs/calib_iag.png', pointsize = 6, res=300, width = 1500, height = 900, bg='transparent')
ggplot() +
  #geom_smooth(data=plot.glm, aes(y=y,x=x), formula=y~log(x), method=glm, size = 0.2, colour='black', inherit.aes=FALSE) +
  #geom_line(data=val.df, aes(y=,x=)) +
  geom_pointrange(data=val.iag.df, aes(x=id, y=coef, ymin=coef-coef_err, ymax=coef+coef_err), size=0.1) +
  #geom_text(data=plot.info, aes(x=median_p, y=prop_coll, label=count),hjust=-0.1, vjust=-1, size = 2.0, inherit.aes=FALSE) +
  #geom_segment(aes(x = 0, y = 1, xend = Inf, yend = 1), linetype=2, size=0.1) +
  #geom_segment(aes(x = 0, y = mean(val.df[1:3,2]), xend = Inf, yend = mean(val.df[1:3,2])), linetype=2, size=0.1) +
  #coord_flip() +
  ylab("Calibration coefficient") +
  xlab("Data combinations used for validation") +
  theme_bw() +
  theme(plot.margin=unit(c(.5,0,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 8))
dev.off()

png('figs/dev_iag.png', pointsize = 6, res=300, width = 1500, height = 900, bg='transparent')
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
  theme(text = element_text(size = 8))
dev.off()

#######################################################################################

y <- coll.glm$data$y
p <- predict(coll.glm, coll.glm$data, type="response")

p.b <- .bincode(p,seq(min(p),max(p),(max(p)-min(p))/10),include.lowest = TRUE)

yp_bins <- data.frame(y,p,p.b)


plot.info <- ddply(yp_bins, ~p.b, summarise,
                   count=length(y),
                   prop_coll=sum(y)/length(y),
                   prop_nocoll=(length(y)-sum(y))/length(y),
                   prop_lo=binom.test(sum(y), length(y), 0.5)$conf.int[1],
                   prop_hi=binom.test(sum(y), length(y), 0.5)$conf.int[2],
                   median_p=median(p)
)

calib <- glm(y~log(p), family=binomial(link=cloglog), data=yp_bins)
roc <- roc(y,p)
perform.glm <- rbind(calib_int=calib$coefficients[1], calib_slope=calib$coefficients[2], roc)
colnames(perform.glm) <- "0.0"

png('figs/calib.png', pointsize = 6, res=300, width = 900, height = 900, bg='transparent')
ggplot() +
  #geom_smooth(data=plot.glm, aes(y=y,x=x), formula=y~log(x), method=glm, size = 0.2, colour='black', inherit.aes=FALSE) +
  geom_line(data=yp_bins, aes(y=invcloglog(calib$coefficients[1]+calib$coefficients[2]*log(p)),x=p)) +
  geom_pointrange(data=plot.info, aes(x=median_p, y=prop_coll, ymin=prop_lo, ymax=prop_hi), size = 0.2, inherit.aes=FALSE) +
  geom_text(data=plot.info, aes(x=median_p, y=prop_coll, label=count),hjust=-0.1, vjust=-1, size = 2.0, inherit.aes=FALSE) +
  geom_segment(aes(x = 0, y = 0, xend = .04, yend = .04), linetype=2, size=0.1, inherit.aes=FALSE) +
  #coord_flip() +
  ylab("Observed Rate (proportion in one year)") +
  xlab("Predicted Rate (proportion in one year)") +
  theme_bw() +
  theme(plot.margin=unit(c(.5,0,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 10))
dev.off()
