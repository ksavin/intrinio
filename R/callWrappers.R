#' Load list of securities and their details
#'
#' @param tickers A character vector of stock symbols to load detailed data on.
#' @param exchange A string or a character vector of the same length as tickers (if specified) containing exchange(s)
#' @details Calling \code{i.securities()} without any arguments will return master data feed (a data.frame with all tickers)\cr
#' Passing a vector of tickers will return more detailed data on securities
#' @note  Master data feed took ~500 at the time of writing
#' @return Data in specified format. See \code{\link{intrOptions}} for details
#' @seealso \href{http://docs.intrinio.com/#securities}{Securities API documentation}.
i.securities <- function(tickers = NULL, exchange = NULL, ...){
  if (!is.null(tickers)) {
    if (is.null(exchange))
      return(i_Rbind(intrCallWrap('securities', identifier = tickers, ...)))
    if (length(exchange) == 1)
      return(i_Rbind(intrCallWrap('securities', identifier = tickers,
                                  MoreArgs = c(list(exch_symbol = exchange), list(...)))))
    if (length(tickers) != length(exchange))
      stop('If multiple exchanges specified, their length must match corresponding tickers length')
    return(intrCallWrap('securities', identifier = tickers, exch_symbol = exchange, MoreArgs = list(...)))
  }
  i_Rbind(intrCall('securities', exch_symbol = exchange, ...))
}

