#' function to create comma separated list with strings in quotes
#' @param text Vector containing the text. Can be in list format as well.
#' @param sep The character or string you want to separate the single quoted strings. The default is a comma ','
#' @return shpaste() The function returns one string with the items in a vector or list quoted and separated by a separator
#' @export
#' @examples
#' ran_vect = runif(n = 10, min = 1, max = 100)
#' shpaste(ran_vect)
#' ran_list = lapply(ran_vect, function(x) x)
#' shpaste(ran_list)
#' shpaste(ran_list, '--')

shpaste <- function(text, sep) {
  # set default separator
  if (missing(sep)) {
    sep = ','
  }

  # convert to vector if a list
  # list are type 'vector' and 'list'
  # vectors are not type 'list'
  if (is.list(text)) {
    text = sapply(text, function(x) x)
  }
  temp_text = paste(shQuote(text), collapse = paste0(sep))
  return(temp_text)
}
