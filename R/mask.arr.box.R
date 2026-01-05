mask.arr.box <- function(tens, box.vec, pl = T){
  library(StatsChitran)
  ######### error check ##########################
  ######### error check ##########################

  if(is.tensor.rank3(tens) == F){
    stop('tens needs to be a rank 3 tensor of the (data, X, Y) form')
  }
  if( (length(box.vec) != 4)  || (is.numeric(box.vec) == F) ){
    stop('box.vec needs to be a numeric vector of length 4')
  }
  #Define the result tensor
  res.tens <- tens
  #Define m and n
  m <- dim(tens)[1]
  n <- dim(tens)[2]
  #Define the storage modes
  storage.mode(tens) <- 'double'
  storage.mode(res.tens) <- 'double'
  storage.mode(box.vec) <- 'double'
  storage.mode(m) <- 'integer'
  storage.mode(n) <- 'integer'
  #Call the return list
  ret.list <- .C('mask_box_c', tens = tens, res_tens = res.tens, m=m, n=n, box_vec=box.vec)
  Z <- ret.list$res_tens
  if(pl){
    plot2D.arr(Z)
    return(Z)
  }else{
    return(Z)
  }
}
