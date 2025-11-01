#' Estimates the co-efficients of the fitting line/plane/hyper-plane using linear regression
#'
#' @description This function takes in a non-singular square matrix and a vector of the same length as the no. of rows of the matrix \\cr
#' The no. of conditions/equations are equal to the no. of rows of matrix \code{A}
#'
#'
#' @param A Numeric matrix for the system of linear equations
#' @param b Numeric Vector having length equal to no. of equations
#'
#' @details
#' Solves the matrix equation as shown below. \cr
#' \deqn{ [A] [X] = [b]; \quad \implies \text{returns} \,\, [X] = [A]^{-1}[b]}
#' For example, the equations shown below
#' \deqn{
#' x + y = 3 \\\\
#' x - y = 1
#' }
#' Can be arranged as shown
#' \deqn{
#' \begin{pmatrix}
#' 1 & 1 \\
#' 1 & -1
#' \end{pmatrix}\cdot
#' \begin{pmatrix}
#' x \\
#' y
#' \end{pmatrix} =
#' \begin{pmatrix}
#' 3\\
#' 1
#' \end{pmatrix}
#' }
#' Where \eqn{[A]} and \eqn{[b]} are represented as shown below
#' \deqn{
#' [A] =
#' \begin{pmatrix}
#' 1 & 1\\
#' 1 & -1
#' \end{pmatrix}
#' \quad [b] =
#' \begin{pmatrix}
#' 3\\
#' 1
#' \end{pmatrix}
#' }
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' @return A numeric vector of the same length as \eqn{[b]}.
#'
#' @author
#' Chitran Ghosal <ghosal.chitran@gmail.com>
#'
#' @examples
#' A <- matrix(data = c(1,1,1, -1), nrow = 2, byrow = T)
#' b <- c(3,1)
#' solve_sym_auto(A,b)
#'
#' @export
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
  if(dim(df)[1] < dim(df)[2]){
    stop('No of data-points too low for the no. of variables involved')
  }
  storage.mode(df) <- "double"
  Cf <- vector(mode = 'numeric', length = dim(df)[2])
  storage.mode(Cf) <- 'double'
  res <- .C("lin_reg", dat = df, p = dim(df)[1], q = dim(df)[2], Coeff = Cf)
  return(res$Coeff)
}
