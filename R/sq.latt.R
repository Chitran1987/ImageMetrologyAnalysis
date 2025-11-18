sq.latt <- function(X, Y, R.latt, A, sig, pl = T){
  #X, Y, mx, my, R_latt, A, sig, tens ---------> These are the inputs that the fortran subroutine needs
  #Call the libraries needed
  library(StatsChitran)
  #Define the minimum inputs needed for the fortran subroutine
  m.x <- length(X)
  m.y <- length(Y)
  tens <- array(data = 0, dim = c(m.y, m.x, 3))
  #Define the storage modes for passing to fortran
  storage.mode('X') <- 'double'
  storage.mode('Y') <- 'double'
  storage.mode('R.latt') <- 'double'
  storage.mode('A') <- 'double'
  storage.mode('sig') <- 'double'
  storage.mode('tens') <- 'double'
  storage.mode('m.x') <- 'integer'
  storage.mode('m.y') <- 'integer'
  res.list <- .C('square_latt_sb_c', X = X, Y = Y, mx = m.x, my = m.y, R_latt = R.latt, A = A, sig = sig, tens = tens)
  ret.tens <- res.list$tens
  if(pl){
    plot2D.arr(ret.tens)
  }
  return(ret.tens)

}
