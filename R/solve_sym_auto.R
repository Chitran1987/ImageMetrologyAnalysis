#' Solves a system of Linear Equations
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
solve_sym_auto <- function(A, b) {
  if(!is.numeric(A) || !is.numeric(b)){
    stop('A and b need to be of type numeric')
  }
  if(dim(A)[1] != dim(A)[2]){
    stop('A needs to be a square matrix, please check dimensions')
  }
  if(dim(A)[1] != length(b)){
    stop('Sizes of A and b are not compatible')
  }
  storage.mode(A) <- "double"
  storage.mode(b) <- "double"
  # Try Cholesky (fast, if SPD). If it fails, fall back to LU.
  ch <- try(chol(A), silent = TRUE)
  if (!inherits(ch, "try-error")) {
    # SPD path: solve L^T L x = b
    return(backsolve(ch, forwardsolve(t(ch), b, upper.tri=FALSE), upper.tri=TRUE))
  } else {
    return(solve(A, b))  # LU fallback (dgesv under the hood)
  }
}
