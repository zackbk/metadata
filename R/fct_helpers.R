toDateTime <- function(x) {
  a <- as.POSIXct(x = strptime( x, format = "%d/%m/%Y  %I:%M %p"))
  a[is.na(a)] <- as.POSIXct(x = strptime( x[is.na(a)], format = "%Y-%m-%d %H:%M:%S"))
  a
}
