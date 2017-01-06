require(ggplot2)
require(plyr)
require(plotROC)
require(reshape)

load(file="data/vic_coll_model_data")
load(file="data/vic_coll_glm")
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
data.id <- factor(c("o","b","w","c","bw","wc","cb","bwc"), levels=c("o","b","w","c","bw","wc","cb","bwc"))
preds.m <- data.frame(data.id,"collrisk_m"=apply(preds[,-1],2,mean),"collrisk_sd"=apply(preds[,-1],2,sd),"collrisk_min"=apply(preds[,-1],2,min),"collrisk_max"=apply(preds[,-1],2,max))

#preds.m2 <- melt(preds[sample(nrow(preds), 100), ], id.vars=c("uid"))

#plot predictions with varying combinations of data
png('figs/preds.png', pointsize = 6, res=300, width = 900, height = 900, bg='transparent')
ggplot() +
  geom_point(data=preds.m, aes(x=data.id, y=collrisk_m)) +
  #geom_pointrange(data=preds.m, aes(x=data.id, y=collrisk_m, ymin=collrisk_m-collrisk_sd, ymax=collrisk_m+collrisk_sd)) +
  #geom_text(data=plot.info, aes(x=median_p, y=prop_coll, label=count),hjust=-0.1, vjust=-1, size = 2.0, inherit.aes=FALSE) +
  #geom_segment(aes(x = 0, y = 0, xend = .04, yend = .04), linetype=2, size=0.1, inherit.aes=FALSE) +
  #coord_flip() +
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
val.df[1,] <- c("o.val.b",coef(val.b)[2,1],coef(val.b)[2,2])
val.df[2,] <- c("o.val.w",coef(val.w)[2,1],coef(val.w)[2,2])
val.df[3,] <- c("o.val.c",coef(val.c)[2,1],coef(val.c)[2,2])
val.df[4,] <- c("b.val.w",coef(b.val.w)[2,1],coef(b.val.w)[2,2])
val.df[5,] <- c("b.val.c",coef(b.val.c)[2,1],coef(b.val.c)[2,2])
val.df[6,] <- c("w.val.c",coef(w.val.c)[2,1],coef(w.val.c)[2,2])
val.df[7,] <- c("w.val.b",coef(w.val.b)[2,1],coef(w.val.b)[2,2])
val.df[8,] <- c("c.val.b",coef(c.val.b)[2,1],coef(c.val.b)[2,2])
val.df[9,] <- c("c.val.w",coef(c.val.w)[2,1],coef(c.val.w)[2,2])
val.df[10,] <- c("bw.val.c",coef(bw.val.c)[2,1],coef(bw.val.c)[2,2])
val.df[11,] <- c("wc.val.b",coef(wc.val.b)[2,1],coef(wc.val.b)[2,2])
val.df[12,] <- c("cb.val.w",coef(cb.val.w)[2,1],coef(cb.val.w)[2,2])
val.df$id <- factor(val.df$id, levels=c("o.val.b","o.val.w","o.val.c","b.val.w","b.val.c","w.val.c","w.val.b","c.val.b","c.val.w","bw.val.c","wc.val.b","cb.val.w"))
val.df$coef <- as.numeric(val.df$coef)
val.df$coef_err <- as.numeric(val.df$coef_err)

#plot calibration with varying combinations of data
png('figs/calib.png', pointsize = 6, res=300, width = 900, height = 900, bg='transparent')
ggplot() +
  #geom_smooth(data=plot.glm, aes(y=y,x=x), formula=y~log(x), method=glm, size = 0.2, colour='black', inherit.aes=FALSE) +
  #geom_line(data=val.df, aes(y=,x=)) +
  geom_pointrange(data=val.df, aes(x=id, y=coef, ymin=coef-coef_err, ymax=coef+coef_err), size=0.1) +
  #geom_text(data=plot.info, aes(x=median_p, y=prop_coll, label=count),hjust=-0.1, vjust=-1, size = 2.0, inherit.aes=FALSE) +
  geom_segment(aes(x = 0, y = 1, xend = Inf, yend = 1), linetype=2, size=0.1) +
  #geom_segment(aes(x = 0, y = mean(val.df[1:3,2]), xend = Inf, yend = mean(val.df[1:3,2])), linetype=2, size=0.1) +
  #coord_flip() +
  ylab("Calibration coefficient") +
  xlab("Data combinations used for modelling & validation") +
  theme_bw() +
  theme(plot.margin=unit(c(.5,0,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 8))
dev.off()


#create dataframe for iag calibration metrics
val.iag.df <- data.frame("id"=data.id,"coef"=rep(NA,8),"coef_err"=rep(NA,8),"dev"=val.iag.dev)
val.iag.df[1,2:3] <- c(coef(val.iag)[2,1],coef(val.iag)[2,2])
val.iag.df[2,2:3] <- c(coef(val.iag.b)[2,1],coef(val.iag.b)[2,2])
val.iag.df[3,2:3] <- c(coef(val.iag.w)[2,1],coef(val.iag.w)[2,2])
val.iag.df[4,2:3] <- c(coef(val.iag.c)[2,1],coef(val.iag.c)[2,2])
val.iag.df[5,2:3] <- c(coef(val.iag.bw)[2,1],coef(val.iag.bw)[2,2])
val.iag.df[6,2:3] <- c(coef(val.iag.wc)[2,1],coef(val.iag.wc)[2,2])
val.iag.df[7,2:3] <- c(coef(val.iag.cb)[2,1],coef(val.iag.cb)[2,2])
val.iag.df[8,2:3] <- c(coef(val.iag.bwc)[2,1],coef(val.iag.bwc)[2,2])
val.iag.df$coef <- as.numeric(val.iag.df$coef)
val.iag.df$coef_err <- as.numeric(val.iag.df$coef_err)
val.iag.df$dev <- as.numeric(val.iag.df$dev)


#plot iag calibration and performance with varying combinations of data
png('figs/calib_iag.png', pointsize = 6, res=300, width = 900, height = 900, bg='transparent')
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

png('figs/dev_iag.png', pointsize = 6, res=300, width = 900, height = 900, bg='transparent')
ggplot() +
  #geom_smooth(data=plot.glm, aes(y=y,x=x), formula=y~log(x), method=glm, size = 0.2, colour='black', inherit.aes=FALSE) +
  #geom_line(data=val.df, aes(y=,x=)) +
  geom_point(data=val.iag.df, aes(x=id, y=dev), size=0.1) +
  #geom_text(data=plot.info, aes(x=median_p, y=prop_coll, label=count),hjust=-0.1, vjust=-1, size = 2.0, inherit.aes=FALSE) +
  #geom_segment(aes(x = 0, y = 1, xend = Inf, yend = 1), linetype=2, size=0.1) +
  #geom_segment(aes(x = 0, y = mean(val.df[1:3,2]), xend = Inf, yend = mean(val.df[1:3,2])), linetype=2, size=0.1) +
  #coord_flip() +
  ylab("Percent reduction in deviance") +
  xlab("Data combinations used for validation") +
  theme_bw() +
  theme(plot.margin=unit(c(.5,0,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 8))
dev.off()

