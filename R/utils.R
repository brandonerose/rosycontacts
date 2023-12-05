#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

pkg_name<-.packageName
pkg_version<-as.character(utils::packageVersion(.packageName))
pkg_date<-Sys.Date()

drop_nas<-function (x) {
  x[!sapply(x, is.na)]
}

grab_after_symbol<- function(vector,symbol,ifna=""){
  x<-stringr::str_extract(vector, paste0("(?<=",symbol,").*"))
  x[which(is.na(x))] <- ifna
  x
}

convert_to_phone_format <- function(numbers) {
  formatted_numbers <- character(length(numbers))  # Initialize an empty vector
  for (i in seq_along(numbers)) {
    current_number <- numbers[i]
    if (nchar(current_number) != 10 || !grepl("^\\d+$", current_number)) {
      stop("Input must be a string of 10 digits.")
    }
    first_part <- substr(current_number, 1, 3)
    second_part <- substr(current_number, 4, 6)
    remaining_part <- substr(current_number, 7, 10)
    formatted_numbers[i] <- paste(first_part, second_part, remaining_part, sep = "-")
  }
  return(formatted_numbers)
}
get_first_longer_than_10_digit_numbers <- function(input_vector) {
  pattern <- "\\b\\d{10,}\\b"  # Pattern for more than 10 digits
  result <- vector(mode = "character", length = length(input_vector))

  for (i in seq_along(input_vector)) {
    match <- regmatches(input_vector[i], regexec(pattern, input_vector[i]))[[1]]
    if (length(match) > 0) {
      result[i] <- match
    } else {
      result[i] <- NA_character_
    }
  }

  return(result)
}
numbers_only <- function(x) !grepl("\\D", x)

wrap_string_to_lines <- function(text, max_length,spacer="") {
  result_vector <- c()
  n <- nchar(text)
  start <- 1
  end <- min(start + max_length-1, n)
  chunk <- substr(text, start, end)
  stringr::str_length(chunk)
  result_vector <- c(result_vector, chunk)
  start <- end + 1
  while (start <= n) {
    end <- min(start + (max_length - stringr::str_length(spacer)-1), n)
    chunk <- paste(spacer, substr(text, start, end), sep = "")
    stringr::str_length(chunk)
    result_vector <- c(result_vector, chunk)
    start <- end + 1
  }
  return(result_vector)
}

clean_df_of_NA <- function(df){
  df %>% dplyr::mutate_all(~ ifelse(is.na(.), "", .))
}
