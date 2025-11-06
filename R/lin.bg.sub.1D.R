###Write a function for linear background substraction in 1D
lin.bg.sub.1D <- function(dat, win, min.zero){
  ##error checking
  #
  #
  #
  #
  #ensure that min.zero is a boolean
  #
  #
  #error checking
  dat <- as.matrix(dat)
  win <- as.matrix(win)
  min.zero <- as.integer(min.zero)
  storage.mode(dat) <- "double"
  storage.mode(win) <- "double"
  storage.mode(min.zero) <- 'integer'
  dat.m = dim(dat)[1]
  dat.n = dim(dat)[2]
  win.m = dim(win)[1]
  win.n = dim(win)[2]
  #compute sizes for res_m and res_n
  #don't need to compute they have the exact same size as the input matrix dat
  res <- .C('lin_bg_sub_1D_c', dat=dat, dat_m=dat.m, dat_n=dat.n, win=win, win_m=win.m, win_n=win.n, res=dat, min_zero=min.zero)
  res <- as.data.frame(res$res)
  names(res) <- c('X', 'Y')
  return(res)
}
