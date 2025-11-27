#Write a function that returns a rank 3 tensor plottable with plot2D() from StatsChitran
func2D <- function(f, X, Y){
  ###error checking###################################
  if(!is.function(f)){
    stop('Argument f needs to be a function')
  }
  if(length(formals(f)) != 2){
    stop('Argument f neds to be a function with exactly two arguments')
  }
  if(!identical(args, c("X", "Y"))){
    stop('f needs to be a function whose arguments are exactly called "X" and "Y" ')
  }
  if( !is.numeric(X) || (length(dim(X)) != 1) ){
    stop('X needs to be a numeric vector')
  }
  if( !is.numeric(Y) || (length(dim(Y)) != 1)){
    stop('Y needs to be a numeric vector')
  }
  ###eroor checking####################################
  XY <- grid_2(X, Y)
  func.mat <- f(X = XY[,,1], Y = XY[,,2])
  tens <- array(data=NA, dim = c(length(Y), length(X), 3))
  tens[,,1] <- func.mat
  tens[,,2:3] <- XY
  return(tens)
}
