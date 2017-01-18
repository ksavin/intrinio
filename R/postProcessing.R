#' Rbinds raw list data into tables
#'
#' @param x a list with raw data
#' @param id A vector or list with id columns. Use only together with \code{intrCallWrap}.
#' @noRd
#' @return returns data in specified format, determined with \code{\link{intrOptions}}
intrRbind <- function(x, id = NULL, outFormat = intrOptions()$outFormat){
  x <- rbindlist(x[!sapply(x, function(x) is.null(x) || length(x) == 0)], fill = TRUE)
  if (!is.null(id)) {
    id <- as.data.table(id)
    id[, intr_call_id := .I]
    x <- id[x, on = 'intr_call_id']
    x$intr_call_id <- NULL
  }
  return(i_convert(x, outFormat))
}

i_convert <- function(x, outFormat = intrOptions()$outFormat){
  switch(outFormat,
         data.frame = as.data.frame(x),
         data.table = x,
         stop('outFormat option can be either data.table or data.frame'))
}
