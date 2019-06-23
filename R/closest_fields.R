# Identify closest field in codelist
#'
#' Accepts a vector of unique values and returns an ordered data.frame or tibble
#' that shows the most closely matching fields in the codelist data.frame
#' @param x A vector of unique codes or names to check against the codelist data.frame
#' @aliases closest_field
#' @examples
#' # Most likely 2 character code fields
#' closest_field(c('CM','AL','PA','MK','HA','CO','ZI','KU','TH','FI'))
#' # Most likely country name fields
#' closest_field(c('Ivory Coast','Guinea','Iran','Russia','North Korea'))
#' @export
closest_field = function(x){
  if(length(x) > length(unique(x))) stop('x must be a vector of unique values')
  similarity = sapply(codelist, function(field){ 100 * sum(x %in% field) / length(x) })
  similarity = data.frame(field = names(similarity), similarity = as.vector(similarity), stringsAsFactors=FALSE)
  similarity = similarity[order(similarity$similarity, decreasing = TRUE),]
  if(suppressWarnings(suppressMessages(!require(tibble)))) return(similarity)
  return(as_tibble(similarity))
}
