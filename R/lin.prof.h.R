lin.prof.h <- function(tens, h.val){
  if(!is.tensor.rank3(tens)){
    stop('agument tens has to be of type tensor.rank3')
  }
  if( !is.numeric(h.val) || length(h.val) != 1 ){
    stop('h.val should be a numeric scalar')
  }
  storage.mode(tens) <- 'double'
  storage.mode(h.val) <- 'double'
  res.mat <- matrix(data = 0, nrow = dim(tens)[2], ncol = 2)
  res <- .C("lin_prof_h_c", tens, dim(tens)[1], dim(tens)[2], dim(tens)[3], h.val, res.mat)
  res <- as.data.frame(res$res.mat)
  names(res) = c('dist', 'magnitude')
  return(res)

}
