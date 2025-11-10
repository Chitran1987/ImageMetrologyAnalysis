lin.prof.h <- function(tens, h.val, pl=T){
  library(StatsChitran)
  if(!is.tensor.rank3(tens)){
    stop('agument tens has to be of type tensor.rank3')
  }
  if( !is.numeric(h.val) || length(h.val) != 1 ){
    stop('h.val should be a numeric scalar')
  }
  if(!is.logical(pl) || length(pl)!= 1){
    stop('pl should be a boolean bit')
  }
  if( h.val<min(tens[,1,3]) || h.val>max(tens[,1,3]) ){
    stop('h.val is not within the range of Y-values of the tens tensor')
  }
  res_mat <- matrix(data = 0.0, nrow = dim(tens)[2], ncol = 2)
  v <- dim(tens)
  storage.mode(tens) <- 'double'
  storage.mode(h.val) <- 'double'
  storage.mode(res_mat) <- 'double'
  storage.mode(v) <- 'integer'
  m <- v[1]
  n <- v[2]
  p <- v[3]
  res <- .C("lin_prof_h_c", Tens = tens, m=m, n=n, p=p, h_val=h.val, res=res_mat)
  res_df <- as.data.frame(res$res)
  names(res_df) <- c('dist', 'magnitude')
  if(pl){
    v_dummy <- tens[,1,3] #The vector to figure out which value h.val is closest to
    v_dummy <- abs(v_dummy - h.val)
    ind <- which.min(v_dummy)
    tens[ind,,1] <- 1.2*max(tens[,,1])
    plot2D.arr(tens)
  }
  return(res_df)

}
