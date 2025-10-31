# Solve A x = b. Works for vector or matrix b.
solve_sym_auto <- function(A, b) {
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
