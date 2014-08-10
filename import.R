library(ggplot2)

raw <- read.csv2("calib_values.txt")

thumb <- raw[ ,1:2]
hand <- raw[ ,3:4]

names(thumb) <- c("x","y")
names(hand) <- c("x","y")

ggplot(thumb) + geom_point(aes(x, y))


alpha0 <- -18
beta0 <- 1024
thumb.nls <- nls( y ~ x*alpha/(x-beta), data=thumb, start=list(alpha=alpha0,beta=beta0), trace=TRUE)
thumb.nls

test.df <- merge(data.frame( x=0:1023), thumb, all.x=TRUE) 
test.df$y.pred <- predict(thumb.nls, newdata=test.df)
ggplot(test.df, aes(x=x)) + geom_point(aes(y=y)) + geom_line(aes(y=y.pred)) + scale_x_continuous(lim=c(0,817)) + scale_y_continuous(lim=c(0,100))









