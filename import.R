library(ggplot2)

raw <- read.csv2("calib_values.txt")

thumb <- raw[ ,1:2]
hand <- raw[ ,3:4]

names(thumb) <- c("x","y")
names(hand) <- c("x","y")

hand$y[36] <- NA

###
### THUMB
###
ggplot(thumb, aes(x, y)) + geom_point() + geom_smooth(se=FALSE, span=.5)

thumb.alpha0 <- -18
thumb.beta0 <- 1024
##thumb.nls <- nls( y ~ alpha/(alpha+x), data=thumb, start=list(alpha=-840), trace=TRUE)
##thumb.nls <- nls( y ~ alpha/(x+beta), data=thumb, start=list(alpha=-1000,beta=-840), trace=TRUE)
thumb.nls <- nls( y ~ x*alpha/(x-beta), data=thumb, start=list(alpha=thumb.alpha0,beta=thumb.beta0), trace=TRUE)
thumb.nls

thumb.loess <- loess(y~x, data=thumb, span=.5)


thumb.pred <- merge(data.frame( x=0:1023), thumb, all.x=TRUE) 
thumb.pred$y.pred <- predict(thumb.loess, newdata=thumb.pred)
ggplot(thumb.pred, aes(x=x)) + geom_point(aes(y=y)) + geom_line(aes(y=y.pred)) + scale_x_continuous(lim=c(0,817)) + scale_y_continuous(lim=c(0,100))
##problems: 1) asymptote not at 1023, but around 842, 2) possible instrument sticking?, 3) and/or function a bit different
dev.new()
ggplot(thumb.pred, aes(x=x, y=(y-y.pred))) + geom_point() + geom_smooth(se=FALSE) #show residuals


write.csv(thumb.pred[1:817, ], "andreas-thumb-smoothed.csv", row.names=FALSE)


###
###HAND
###
ggplot(hand,aes(x,y)) + geom_point() + geom_smooth(se=FALSE,span=.5)

hand.alpha0 <- -5
hand.beta0 <- 1023
hand.nls <- nls( y ~ x*alpha/(x-beta), data=hand, start=list(alpha=hand.alpha0,beta=hand.beta0), trace=TRUE)
hand.nls


hand.loess <- loess(y~x, data=hand, span=.5)

hand.pred <- merge(data.frame( x=0:1023), hand, all.x=TRUE) 
hand.pred$y.pred <- predict(hand.loess, newdata=hand.pred)
ggplot(hand.pred, aes(x=x)) + geom_point(aes(y=y)) + geom_line(aes(y=y.pred)) + scale_x_continuous(lim=c(0,1023)) + scale_y_continuous(lim=c(0,100))

dev.new()
ggplot(hand.pred, aes(x=x, y=(y-y.pred))) + geom_point() + geom_smooth(se=FALS
E) #show residuals




write.csv(hand.pred[1:941, ], "andreas-sideofhand-smoothed.csv", row.names=FALSE)
