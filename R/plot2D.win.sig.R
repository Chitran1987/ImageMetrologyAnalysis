plot2D.win.sig <- function(tens, center, k, Xspan, Yspan, pl = T){
  library(StatsChitran)
  #error checks
  if(!is.tensor.rank3(tens)){
    stop('argument should be a rank-3 image tensor')
  }
  if( (length(center) != 2 ) || (!is.numeric(center))){
    stop('center should be a numeric vector of length = 2')
  }
  if( (!is.numeric(k)) || (!is.numeric(Xspan)) || (!is.numeric(Yspan))){
    stop('arguments k, Xspan and Yspan should all be of numeric type')
  }
  if( (length(k) != 1) || (length(Xspan) != 1) || (length(Yspan) != 1)){
    stop('Arguments k, Xspan and Yspan all need to be scalars')
  }
  #error checks
  #Define m and n
  m <- dim(tens)[1]
  n <- dim(tens)[2]

  #Define res.tens
  res.tens <- tens

  #Define the storage modes
  storage.mode(m) <- 'integer'
  storage.mode(n) <- 'integer'
  storage.mode(res.tens) <- 'double'
  storage.mode(tens) <- 'double'
  storage.mode(center) <- 'double'
  storage.mode(k) <- 'double'
  storage.mode(Xspan) <- 'double'
  storage.mode(Yspan) <- 'double'

  #Call the function
  for.res <- .C('window_sigmoid_c',tens = tens, res_tens = res.tens, m = m, n = n, cent = center, k = k, Xspan = Xspan, Yspan = Yspan)
  Z <- for.res$res_tens
  if(pl){
    plot2D.arr(Z)
    return(Z)
  }else{
    return(Z)
  }
}
