#' Fisher's R to Z
#'
#' \code{r2z} Converts an r-value to a z-value using Fisher's r-to-z transformation.
#'
#' @param r the existing r-value (should be numeric, >-1, <1)
#' 
#' @return double
#' @examples
#' r2z(-.4) # -0.4236489
#' @export
r2z <- function(r) {
  stopifnot(r >= -1)
  stopifnot(r <= 1)
  .5*(log((1+r)/(1-r)))
}

#' Steiger's Z
#'
#' \code{steiger} Converts an r-value to a z-value using Fisher's r-to-z transformation.
#'
#' @param r12 the correlation between variables 1 and 2
#' @param r13 the correlation between variables 1 and 3
#' @param r23 the correlation between variables 2 and 3
#' @param n the number of observations
#' @param tails whether to report a one-tailed or two-tailed p-value
#' 
#' @return list
#' @examples
#' steiger(.4, .3, .2, 20) # z=0.3583792, p=0.7200596
#' @export
steiger <- function(r12, r13, r23, n, tails = 2) {
  stopifnot(tails %in% 1:2)
  stopifnot(r12 >= -1)
  stopifnot(r12 <= 1)
  stopifnot(r13 >= -1)
  stopifnot(r13 <= 1)
  stopifnot(r23 >= -1)
  stopifnot(r23 <= 1)
  stopifnot(n > 1)
  
  z12 <- r2z(r12)
  z13 <- r2z(r13)
  rm2 <- (r12*r12 + r13*r13)/2
  f <- (1 - r23) / (2*(1 - rm2))
  h <- (1 - (f*rm2)) / (1 - rm2)
  z <- (z12 - z13) * (sqrt(n - 3)) / sqrt(2 * (1 - r23) * h)
  p <- tails*stats::pnorm(-abs(z))
  
  list("z" = z, "p" = p)
}
