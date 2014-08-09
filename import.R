library(ggplot2)

raw <- read.csv2("calib_values.txt")

thumb <- raw[ ,1:2]
hand <- raw[ ,3:4]

ggplot(thumb) + geom_point(aes(thumbvalue, thumbcalib))


