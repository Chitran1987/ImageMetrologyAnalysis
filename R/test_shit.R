rm(list=ls())
library(StatsChitran)
library(ImageMetrologyAnalysis)
X <- seq(-10, 10, 0.1)
Y <- gauss(X, sig = 2, probability = T)
Y <- Y + rnorm(length(X), sd=0.01)
plot(X, Y)

#add bg
Y <- Y + 0.01*X
plot(X, Y)
M <- lin.bg.sub.1D(dat = data.frame(X, Y), win = data.frame(A=c(-10,4.5), B = c(-7.5, 10)), min.zero = T  )
