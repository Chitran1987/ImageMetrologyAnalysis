#' Estimates the co-efficients of the fitting line/plane/hyper-plane using linear regression
#'
#' @description This function takes in a dataframe and returns the value of the coefficients of the regression
#'
#'@usage linear.reg(df)
#'
#' @param df Numeric dataframe needed for the regression analysis
#'
#' @details
#' The dataframe argument \code{df} should have \eqn{m} rows/observations/points and \eqn{n} columns.\cr
#' The first \eqn{n-1} columns are treated as the first \eqn{n-1} independent variables or \eqn{X}'s\cr
#' The last/\eqn{n}th column is the independent variable \eqn{Y}\cr
#' Hence the dataframe should be built as shown below\cr
#' \code{df = data.frame(X1, X2, X3, ... , Xn-1, Y)} where \code{Xk, Y} are vectors of equal length\cr
#' The fit model is assumed to be as shown below
#' \deqn{\displaystyle\hat{Y} = C_1 X_1 + C_2 X_2 + C_3 X_3 + \dots + C_{n-1}X_{n-1} + C_n + \epsilon \\\\
#' \implies \hat{Y} = \sum_{k=1}^{n-1}C_k X_k + C_n + \epsilon
#' }
#' Ofcourse, for a valid regression \eqn{m > n}\cr
#' When \eqn{m = n}, the problem reduces to a system of linear equations\cr
#' For solving a system of linear equations, please see \link{solve_sym_auto}\cr
#' The coefficients \eqn{C_1, C_2, \dots C_n} are returned in a vector format\cr
#' The methodology used is related to Ordinary Least Squares or OLS regression. Please see [StatsChitran::OLS.reg()]\cr
#'
#'
#'
#'
#'
#' @return A numeric vector of length \eqn{n} containing the coefficients \eqn{C_1} through \eqn{C_n}.\cr
#' The return vector is arranged as shown below
#' \deqn{\displaystyle
#' C =
#' \begin{pmatrix}
#' C_1\\
#' C_2\\
#' C_3\\
#' \cdot\\
#' \cdot\\
#' \cdot\\
#' C_{n-1}\\
#' C_n
#' \end{pmatrix}
#' }
#'
#' @author
#' Chitran Ghosal <ghosal.chitran@gmail.com>
#'
#' @examples
#'
#' ####First example
#' #Call the relevant libraries
#' library(ImageMetrologyAnalysis)
#' #Build the noisy dataset
#' X1 <- seq(-10, 10, by = 10^-5)
#' X2 <- X1^2
#' Y <- 3*X1 + 4*X2 + 9 + rnorm(n=length(X1), mean = 0, sd=1000) #add noise
#' dat <- data.frame(X1, X2, Y)
#' #Call the linear.reg() function
#' C <- linear.reg(dat)
#' #Check the co-efficients
#' C
#'
#' ####Second example
#' #Call the relevant libraries
#' library(StatsChitran)
#' #Build the noisy polynomial dataset
#' X <- seq(-10, 10, by = 10^-3)
#' Y <- 3*X^2 + 4*X + 7
#' Y <- Y + rnorm(n=length(X), mean = 0, sd = 5) #add noise
#' #The X^2 and the X can be treated as X1 and X2 here respectively
#' #regression of the polynomial co-efficients is in hindsight, a problem in linear regression
#' X1 <- X^2
#' df <- data.frame(X1, X, Y)
#' #Call the linear.reg() function
#' C <- linear.reg(df)
#' #plot the solution
#' plot(X, Y, col = rgb(0, 0, 0.25, 0.25), pch=19)
#' Y.pred <- C[1]*X1 + C[2]*X + C[3]
#' lines(X, Y.pred, col='red')
#'
#'
#'
#'
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
