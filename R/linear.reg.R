###Make function to implement lin_reg, now that lin_reg is callable
linear.reg <- function(df){
  if(!is.data.frame(df)){
    stop('df has to be a data frame')
  }
  if(dim(df)[2] < 2){
    stop('df should have more than one column')
  }
  if(dim(df)[1] <= 2){
    stop('df should have more than two rows or points')
  }
  df <- as.matrix(df)
  if(!is.numeric(df)){
    stop('df should be of type numeric')
  }
  storage.mode(df) <- "double"
  Cf <- vector(mode = 'numeric', length = dim(df)[2])
  storage.mode(Cf) <- 'double'
  res <- .C("lin_reg", dat = df, p = dim(df)[1], q = dim(df)[2], Coeff = Cf)
  return(res$Coeff)
}
