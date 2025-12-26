plot2D.boxes <- function(img.tens, box.mat, pl=T){
  #Call the relevant libraries
  library(StatsChitran)
  #Error Handling###################
  #
  #
  #
  #
  #Error Handling###################

  #Define the tensor dimensions
  m = dim(img.tens)[1]
  n = dim(img.tens)[2]
  n.boxes = dim(box.mat)[1]
  res.tens = array(data = 0, dim=dim(img.tens))
  #Define the storage modes
  storage.mode(img.tens) <- 'double'
  storage.mode(box.mat) <- 'double'
  storage.mode(res.tens) <- 'double'
  storage.mode(m) <- 'integer'
  storage.mode(n) <- 'integer'
  storage.mode(n.boxes) <- 'integer'

  #Call the function
  fortran.res <- .C('plot_boxes_c', img_tens = img.tens, m = m, n = n, box_mat = box.mat, n_boxes = n.boxes, res_tens = res.tens)
  #Return the result
  if(pl){
    plot2D.arr(fortran.res$res_tens)
    return(fortran.res$res_tens)
  }else{
    return(fortran.res$res_tens)
  }




}
