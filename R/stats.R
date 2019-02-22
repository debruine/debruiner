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
#' \code{steiger} Test for significant difference between two correlations on the same sample.
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


#' Steiger's Z for Data Frames or vectors
#'
#' \code{steiger} Test for significant difference between two correlations on the same sample.
#'
#' @param v1 the first vector or a dataframe where the first three columns are the vectors
#' @param v2 the second vector (ignored if v1 is a data frame)
#' @param v3 the third vector (ignored if v1 is a data frame)
#' @param alpha the number of observations
#' @param tails whether to report a one-tailed or two-tailed p-value
#' 
#' @return list
#' @examples
#' steiger_dat(iris)
#' @export
steiger_dat <- function(v1, v2 = NULL, v3 = NULL, alpha = 0.05, tails = 2) {
  varnames <- c("V1", "V2", "V3")
  if (is.data.frame(v1)) {
    dat <- v1
    v1 <- dat[, 1]
    v2 <- dat[, 2]
    v3 <- dat[, 3]
    varnames = names(dat)[1:3]
  }
  stopifnot(length(v1) == length(v2))
  stopifnot(length(v1) == length(v3))
  
  r12 <- stats::cor(v1, v2)
  r13 <- stats::cor(v1, v3)
  r23 <- stats::cor(v2, v3)
  n <- length(v1)
  
  stg1 <- steiger(r12, r13, r23, n, tails)
  stg2 <- steiger(r12, r23, r13, n, tails)
  stg3 <- steiger(r13, r23, r12, n, tails)
  
  sig1 <- ifelse(stg1$p < alpha, "", "not ")
  sig2 <- ifelse(stg2$p < alpha, "", "not ")
  sig3 <- ifelse(stg3$p < alpha, "", "not ")
  
  stg1$text <- paste0(varnames[1], " is correlated to ", varnames[2], 
                      " (r = ", round(r12, 3), ") ",
                      "and to ", varnames[3], " (r = ", round(r13, 3) , 
                      "). This difference is ", sig1, 
                      "significant (z = ", round(stg1$z, 3), ", ", format_p(stg1$p), 
                      ") ", tails, "-tailed with an alpha of ", alpha, ".")
  
  stg2$text <- paste0(varnames[2], " is correlated to ", varnames[1], 
                      " (r = ", round(r12, 3), ") ",
                      "and to ", varnames[3], " (r = ", round(r23, 3) , 
                      "). This difference is ", sig2, 
                      "significant (z = ", round(stg2$z, 3), ", ", format_p(stg2$p), 
                      ") ", tails, "-tailed with an alpha of ", alpha, ".")
  
  stg3$text <- paste0(varnames[3], " is correlated to ", varnames[1], 
                      " (r = ", round(r13, 3), ") ",
                      "and to ", varnames[2], " (r = ", round(r23, 3) , 
                      "). This difference is ", sig3, 
                      "significant (z = ", round(stg3$z, 3), ", ", format_p(stg3$p), 
                      ") ", tails, "-tailed with an alpha of ", alpha, ".")
  
  stg <- list(stg1, stg2, stg3)
  names(stg) <- varnames
  
  print(c(stg1$text, stg2$text, stg3$text))
  invisible(stg)
}


