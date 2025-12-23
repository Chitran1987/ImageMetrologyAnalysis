#Write a function for fft_2D_map()
fft_2D_map <- function(img.tens, DelX, DelY, k1st, k0, pl=T){
  library(StatsChitran)
  #########error checking#############################
  #
  #
  #
  #
  #
  #
  #########error checking#############################
  #Define the return tensor
  ret.tens <- rep(0, dim(img.tens)[1]*dim(img.tens)[2]*dim(img.tens)[3])
  ret.tens <- array(data = ret.tens, dim = dim(img.tens))
  m <- dim(img.tens)[1]
  n <- dim(img.tens)[2]
  n_spots <- dim(k1st)[1]
  #Define the storage modes
  storage.mode(img.tens) <- 'double'
  storage.mode(m) <- 'integer'
  storage.mode(n) <- 'integer'
  storage.mode(ret.tens) <- 'double'
  storage.mode(DelX) <- 'double'
  storage.mode(DelY) <- 'double'
  storage.mode(k1st) <- 'double'
  storage.mode(n_spots) <- 'integer'
  storage.mode(k0) <- 'double'
  'Call the fft_2D_map() function'
  fortran.res.list <- .C('fft_2D_map_c', img_tens = img.tens, res_tens = ret.tens , m = m, n=n, Xspan=DelX, Yspan=DelY, k1st=k1st, k0=k0)
  ret.val <- fortran.res.list$res_tens
  if(pl){
    plot2D.arr(arr=ret.val)
  }
  return(ret.val)
}
