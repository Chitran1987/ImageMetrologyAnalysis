fft_2D <- function(tens, sampling.del = 0.1, pl = 'none'){
  ###Call the relevant libraries
  library(StatsChitran)
  ###Error handling
  #Is tens a rank 3 tensor
  if(!is.tensor.rank3(tens)){
    stop('tens has to be a tensor of rank = 3')
  }
  #does pl belong to either none, amp or phase
  if( pl!='none' && pl!='amp' && pl!='phase'){
    stop("The pl argument should either be a string equalling 'amp', 'phase' or 'none'")
  }

  #Define the return Tensor and the other arguments
  ret.tens <- rep(0.0, dim(tens)[1]*dim(tens)[2]*4)
  ret.tens <- array(data = ret.tens, dim = c(dim(tens)[1], dim(tens)[2], 4))
  m <- dim(tens)[1]
  n <- dim(tens)[2]
  p <- dim(tens)[3]
  wm <- dim(ret.tens)[1]
  wn <- dim(ret.tens)[2]
  wp <- dim(ret.tens)[3]
  #Define and abstract the storage modes
  storage.mode(tens) <- 'double'
  storage.mode(ret.tens) <- 'double'
  storage.mode(sampling.del) <- 'double'
  storage.mode(m) <- 'integer'
  storage.mode(n) <- 'integer'
  storage.mode(p) <- 'integer'
  storage.mode(wm) <- 'integer'
  storage.mode(wn) <- 'integer'
  storage.mode(wp) <- 'integer'
  #Call the fft_2D_c function
  fortran.res.list <- .C('fft_2D_c', Tens = tens, m = m, n = n, p = p, Transform = ret.tens, wm = wm, wn = wn, wp = wp, sampling_del = sampling.del )
  #Define a list for return
  amp.tens <- fortran.res.list$Transform[,,c(1,3,4)]
  phase.tens <- fortran.res.list$Transform[,,c(2,3,4)]
  ret.list <- vector(mode='list', length = 2)
  ret.list[[1]] <- amp.tens
  ret.list[[2]] <- phase.tens

  #Check the plot criterion and return
  if(pl == 'none'){
    return(ret.list)
  }
  if(pl == 'amp'){
    plot2D.arr(ret.list[[1]])
    return(ret.list)
  }
  if(pl == 'phase'){
    plot2D.arr(ret.list[[2]])
    return(ret.list)
  }

}
