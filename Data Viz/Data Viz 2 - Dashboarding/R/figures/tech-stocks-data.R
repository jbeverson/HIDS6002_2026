# dviz.supp::tech_stocks was saved with an old dplyr grouping that current dplyr
# rejects. Strip the stale grouping attributes and return a plain data frame.
tech_stocks_clean <- function() {
  x <- as.data.frame(dviz.supp::tech_stocks)
  attr(x, "groups") <- NULL
  x
}
