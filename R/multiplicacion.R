#' It multiply two values
#'
#' @param x,y
#'
#' @return A numeric output
#' @export
#'
#' @examples
multiply <- function(x = NULL, y = NULL) {
  if (!is.numeric(x) & is.numeric(y)) {stop("x and/or y must be numeric")}

  x*y

}


