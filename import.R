library(ggplot2)

raw <- read.csv2("calib_values.txt")

thumb <- raw[ ,1:2]
hand <- raw[ ,3:4]

names(thumb) <- c("x","y")
names(hand) <- c("x","y")

###
### THUMB
###
ggplot(thumb) + geom_point(aes(x, y))

thumb.alpha0 <- -18
thumb.beta0 <- 1024
thumb.nls <- nls( y ~ x*alpha/(x-beta), data=thumb, start=list(alpha=thumb.alpha0,beta=thumb.beta0), trace=TRUE)
thumb.nls


test.df <- merge(data.frame( x=0:1023), thumb, all.x=TRUE) 
test.df$y.pred <- predict(thumb.nls, newdata=test.df)
ggplot(test.df, aes(x=x)) + geom_point(aes(y=y)) + geom_line(aes(y=y.pred)) + scale_x_continuous(lim=c(0,817)) + scale_y_continuous(lim=c(0,100))
##problems: 1) asymptote not at 1023, but around 842, 2) possible instrument sticking?, 3) and function a bit different


###
###HAND
###


ggplot(hand) + geom_point(aes(x,y))

hand.alpha0 <- -5
hand.beta0 <- 1023
hand.nls <- nls( y ~ x*alpha/(x-beta), data=hand, start=list(alpha=hand.alpha0,beta=hand.beta0), trace=TRUE)
hand.nls


test.df <- merge(data.frame( x=0:1023), hand, all.x=TRUE) 
test.df$y.pred <- predict(hand.nls, newdata=test.df)
ggplot(test.df, aes(x=x)) + geom_point(aes(y=y)) + geom_line(aes(y=y.pred)) + scale_x_continuous(lim=c(0,1023)) + scale_y_continuous(lim=c(0,100))



