#' Rbinds raw list data into tables
#'
#' @param x a list with raw data
#' @param id A vector or list with id columns. Used only when combining results from \code{intrCallWrap} to match parameters with results.
#' @noRd
#' @return returns data in specified format, determined with \code{\link{intrOptions}}
i_Rbind <- function(x, id = NULL){
  x <- rbindlist(x[!sapply(x, function(x) is.null(x) || length(x) == 0)])
  if (!is.null(id)) {
    if (is.list(id)) id <- as.data.table(id)
    else id <- as.data.table(list(id))
    id <- as.data.table(as.list(id))
    setnames(id, paste0('intr_id_', 1:ncol(id)))
    id[, intr_call_id := .I]
    x <- id[x, on = 'intr_call_id']
    x[, intr_call_id := NULL]
  }

  x <- switch(intrOptions()$outFormat,
              data.frame = as.data.frame(x),
              data.table = x,
              stop('outFormat option can be either data.table or data.frame'))
  x
}
