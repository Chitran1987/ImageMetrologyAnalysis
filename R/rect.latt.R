rect.latt <- function(X, Y, R.latt.x, R.latt.y, A, sig, pl = T){
  #X, Y, mx, my, R_latt_x, R_latt_y, A, sig, tens ---------> These are the inputs that the fortran subroutine needs
  #Call the libraries needed
  library(StatsChitran)
  #Define the minimum inputs needed for the fortran subroutine
  m.x <- length(X)
  m.y <- length(Y)
  tens <- array(data = 0, dim = c(m.y, m.x, 3))
  #Define the storage modes for passing to fortran
  storage.mode(X) <- 'double'
  storage.mode(Y) <- 'double'
  storage.mode(R.latt.x) <- 'double'
  storage.mode(R.latt.y) <- 'double'
  storage.mode(A) <- 'double'
  storage.mode(sig) <- 'double'
  storage.mode(tens) <- 'double'
  storage.mode(m.x) <- 'integer'
  storage.mode(m.y) <- 'integer'
  res.list <- .C('rect_latt_sb_c', X = X, Y = Y, mx = m.x, my = m.y, R_latt_x = R.latt.x, R_latt_y = R.latt.y, A = A, sig = sig, tens = tens)
  ret.tens <- res.list$tens
  if(pl){
    plot2D.arr(ret.tens)
  }
  return(ret.tens)

}
