require(ggplot2)
require(plyr)
require(plotROC)

load(file="data/vic_model_data")
load(file="data/vic_coll_glm")
load(file="output/glms")
load(file="output/preds")

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

ggplot(yp_bins, aes(x=p)) + geom_density(aes(group=y, colour=y, fill=y), alpha=0.3)

png('figs/roc.png', pointsize = 6, res=300, width = 900, height = 900, bg='transparent')
ggplot() +
  stat_roc(data=yp_bins, aes(d=y,m=p), n.cuts = 0, size=0.5) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linetype=2, size=0.1, inherit.aes=FALSE) +
  ylab("True Positive Fraction") +
  xlab("False Positive Fraction") +
  theme_bw() +
  theme(plot.margin=unit(c(.5,0,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 10))
dev.off()

perform.glm.1000 <- cbind("full_model"=signif(perform.glm, digits=4),
                          "cv_model_mean"=signif(apply(perform.glm.cv,1,mean), digits=4),
                          "cv_model_sd"=signif(apply(perform.glm.cv,1,sd), digits=4),
                          "cv_model_rlo"=signif(apply(perform.glm.cv,1,range), digits=4)[1,],
                          "cv_model_rhi"=signif(apply(perform.glm.cv,1,range), digits=4)[2,]
)

#write.csv(perform.glm.1000,"output/perform_glm_1000.csv",row.names=FALSE)


metrics <- factor(c("Intercept","Slope","ROC"),
                  levels=rev(c("Intercept","Slope","ROC")))

cv_plot <- transform(data.frame(x = metrics, y = perform.glm.1000[, 2], y_orig = perform.glm.1000[, 1]),
                  ylo=perform.glm.1000[, 2] - 2*perform.glm.1000[, 3],
                  yhi=perform.glm.1000[, 2] + 2*perform.glm.1000[, 3]
                  )

png('figs/validate.png', pointsize = 6, res=300, width = 900, height = 900, bg='transparent')
p <- ggplot() +
  geom_pointrange(data=cv_plot, aes(x=x, y=y, ymin=ylo, ymax=yhi), size = 0.1) +
  #geom_hline(yintercept = 0, linetype=2, size=0.2) +
  coord_flip() +
  ylab("Value") +
  xlab("Test Metric") +
  theme_bw() +
  theme(plot.margin=unit(c(.5,0,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 10))
p + geom_point(data=cv_plot, aes(x=x, y=y_orig), size = 2, shape=1, inherit.aes=FALSE) +
  geom_segment(aes(x = 2.5, y = 0, xend = 3.5, yend = 0), linetype=2, size=0.1) +
  geom_segment(aes(x = 0.5, y = 1, xend = 2.5, yend = 1), linetype=2, size=0.1)
dev.off()
