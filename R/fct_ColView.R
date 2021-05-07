#' filter the view, while also including critical columns.
#'
#'@param x data table.
#'@param r reactive parameter with 'colView' as the column name.
#'@param other other column names critical to downstream operations
#'@return x
ColView <- function(x, r = r, other = c("ext")){
  colNames <- unique(c(r$colView,other))
  colNames <- names(x)[names(x) %in% colNames]
  return(x[,..colNames])
}