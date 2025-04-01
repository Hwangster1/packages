#' Column Mean Function
#'
#' @param df 
#'
#' @details
#' After inputting a dataframe it will output the mean of each column
#' @author Andrew Hwang
#' @returns The Mean of a column
#'
#' @examples
#' ## Data_Frame <- data.frame (column1 = c(100, 150, 120), column2 = c(60, 30, 45))
#' ## col_means(Data_Frame)
col_means <- function(df) {
  means <- numeric(ncol(df))
  for (i in seq_along(df)) {
    means[i] <- mean(df[[i]], na.rm = TRUE)
  }
  return(means)
}

#' Count NA Function
#'
#' This function counts the number of NA values in a given vector using a for-loop.
#'
#' @param vec A numeric or character vector.
#' 
#' @details
#' After inputting a vector, it will return the count of NA values.
#' 
#' @author Andrew Hwang
#' 
#' @return An integer representing the count of NA values in the vector.
#'
#' @examples
#' vec <- c(1, 2, NA, 4, NA, 6)
#' count_na(vec)
#' 
#' @export
count_na <- function(vec) {
  count <- 0
  for (i in seq_along(vec)) {
    if (is.na(vec[i])) {
      count <- count + 1
    }
  }
  return(count)
}