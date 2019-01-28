#' APA-formatted p-values
#'
#' \code{format_p} Produces an APA-formatted string for reporting a p-value.
#'
#' @param p the existing p-value (should be numeric, >0, <1)
#' @param digits integer indicating the number of decimal places to be used for rounding and display (1:10)
#' @param minp the minimum pvalue under which the return value defaults to p < pmin
#' 
#' @return character
#' @examples
#' format_p(0.03007) # p = .030
#' format_p(0.34567, digits = 4) # p = .3457
#' format_p(0.005, digits = 2, minp = .01) # p < .01
#' @export

format_p <- function(p, digits = 3, minp = .001) {
  # sense checks
  p <- suppressWarnings( as.numeric(p) )
  if (is.na(p)) stop("p must be a number")
  if (p < 0) stop("p cannot be < 0")
  if (p > 1) stop("p cannot be > 1")
  
  minp <- suppressWarnings( as.numeric(minp) )
  if (is.na(minp)) stop("minp must be a number")
  if (minp < 0) stop("minp cannot be < 0")
  if (minp > 1) stop("minp cannot be > 1")
  
  if (!(digits %in% 1:10)) stop("digits must be an integer between 1 and 10")

  # handle p < .001 (or value of minp)
  if (p < minp) {
    p <- minp
    symbol <- "<"
  } else {
    symbol <- "="
  }
  
  ## format p-value
  roundp <- round(p, digits)
  stringp <-  formatC(roundp, format = "f", drop0trailing = FALSE, digits = digits)
  nozerop <- gsub("0.", ".", stringp, fixed = TRUE)
  
  paste("p", symbol, nozerop)
}