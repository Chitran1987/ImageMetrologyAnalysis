lin.prof.v <- function(tens, v.val, pl=T){
  library(StatsChitran)
  if(!is.tensor.rank3(tens)){
    stop('agument tens has to be of type tensor.rank3')
  }
  if( !is.numeric(v.val) || length(v.val) != 1 ){
    stop('v.val should be a numeric scalar')
  }
  if(!is.logical(pl) || length(pl)!= 1){
    stop('pl should be a boolean bit')
  }
  if( v.val<min(tens[1,,2]) || v.val>max(tens[1,,2]) ){
    stop('h.val is not within the range of Y-values of the tens tensor')
  }
  res_mat <- matrix(data = 0.0, nrow = dim(tens)[1], ncol = 2)
  v <- dim(tens)
  storage.mode(tens) <- 'double'
  storage.mode(v.val) <- 'double'
  storage.mode(res_mat) <- 'double'
  storage.mode(v) <- 'integer'
  m <- v[1]
  n <- v[2]
  p <- v[3]
  res <- .C("lin_prof_v_c", Tens = tens, m=m, n=n, p=p, v_val=v.val, res=res_mat)
  res_df <- as.data.frame(res$res)
  names(res_df) <- c('dist', 'magnitude')
  if(pl){
    h_dummy <- tens[1,,2] #The vector to figure out which value v.val is closest to
    h_dummy <- abs(h_dummy - v.val)
    ind <- which.min(v_dummy)
    tens[,ind,1] <- 1.2*max(tens[,,1])
    plot2D.arr(tens)
  }
  return(res_df)
}
