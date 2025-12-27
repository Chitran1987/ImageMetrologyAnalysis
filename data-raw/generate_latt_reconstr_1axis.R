rm(list=ls())
#Create two inter-penetrating lattice datasets
#call relevant libraries
library(StatsChitran)
library(ImageMetrologyAnalysis)
X <- seq(-10, 10, by=0.05)
Y <- seq(-10, 10, by=0.05)
sq_latt <- sq.latt(X, Y, R.latt = 1.0, A=1.0, sig=0.2)
rec_latt <- rect.latt(X, Y, R.latt.x = 0.7, R.latt.y = 1.0, A = 1.0, sig = 0.2)
#Create the sigmoid matrixes

#The square lattice modulation
vec <- c(-8, -4, -2, 2, 4, 8)
vec_ind <- c(1, 3, 5)
M_sq <- matrix(data = 0, nrow = dim(sq_latt)[1], ncol = dim(sq_latt)[2])
for (i in vec_ind) {
  M_sq <- M_sq + sigmoid.2D.plat(X, Y, k=9, x.left = vec[i], x.right = vec[i+1], y.left = -11, y.right = 11, pl=F)
}
plot2D.mat(X, Y, Z=M_sq)

#The rec lattice modulation
vec <- c(-10, -8, -4, -2, 2, 4, 8, 10)
vec_ind <- c(1, 3, 5, 7)
M_rec <- matrix(data = 0, nrow = dim(rec_latt)[1], ncol = dim(rec_latt)[2])
for (i in vec_ind) {
  M_rec <- M_rec + sigmoid.2D.plat(X, Y, k = 9, x.left = vec[i], x.right = vec[i+1], y.left = -11, y.right = 11, pl=F)
}
plot2D.mat(X, Y, Z=M_rec)

#Correct the lattices
#Square lattice modulated
sq_latt[,,1] <- sq_latt[,,1]*M_sq
plot2D.arr(sq_latt)

#Rec lattice modulated
rec_latt[,,1] <- rec_latt[,,1]*M_rec
plot2D.arr(rec_latt)

#Build the final lattice
latt <- array(data = 0, dim = dim(sq_latt))
latt[,,2:3] <- sq_latt[,,2:3]
latt[,,1] <- sq_latt[,,1]+rec_latt[,,1]
plot2D.arr(latt)
latt_reconstr_1axis <- latt
