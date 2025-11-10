lin.prof.h <- function(tens, h.val){
  if(!is.tensor.rank3(tens)){
    stop('agument tens has to be of type tensor.rank3')
  }
  if( !is.numeric(h.val) || length(h.val) != 1 ){
    stop('h.val should be a numeric scalar')
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
  #res <- as.data.frame(res$res.mat)
  #names(res) <- c('dist', 'magnitude')
  return(res$res)

}
