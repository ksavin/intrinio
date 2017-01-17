#' Load list of securities and their details
#'
#' @param tickers A character vector of stock symbols to load detailed data on.
#' @param exchange A string or a character vector of the same length as tickers (if specified) containing exchange(s)
#' @param ... other arguments to pass to \code{MoreArgs} of \code{intrCallMap}
#' @details Calling \code{i.securities()} without any arguments will return master data feed (a data.frame with all tickers)\cr
#' Passing a vector of tickers will return more detailed data on securities
#' @note  Master data feed took ~500 API credits to load at the time of writing
#' @return Data in specified format. See \code{\link{intrOptions}} for details
#' @seealso \href{http://docs.intrinio.com/#securities}{Securities API documentation}.
#' @examples
#' \dontrun{
#' i.securities() #Over 500 API calls
#' i.securities(tickers = c('AAPL', 'MSFT'))
#' }
i.securities <- function(tickers = NULL, exchange = NULL, ...){
  if (!is.null(tickers)) {
    if (is.null(exchange))
      return(intrCallMap('securities', identifier = tickers, idCols = FALSE, ...))
    if (length(exchange) == 1)
      return(intrCallMap('securities', identifier = tickers, idCOls = FALSE,
                          MoreArgs = c(list(exch_symbol = exchange), list(...))))
    if (length(tickers) != length(exchange))
      stop('If multiple exchanges specified, their length must match corresponding tickers length')
    return(intrCallMap('securities', identifier = tickers, exch_symbol = exchange, idCols = FALSE, MoreArgs = list(...)))
  }
  intrCall('securities', exch_symbol = exchange, ...)
}

#' Loads Information about companies
#'
#' @param tickers A character vector of stock symbols to load detailed data on.
#' Loads an entire list of companies (master data feed) with compacted response values if \code{NULL}.
#' @param ... other arguments to pass to \code{MoreArgs} of \code{intrCallMap}
#' @details This query will return companies available at intrinio with detailed information,
#' such as hq address, phone number, industry sector, SEC cik number etc.
#' @return Data in specified format. See \code{\link{intrOptions}} for details
#' @examples
#' \dontrun{
#' i.companies()
#' i.companies(c('MSFT', 'AAPL'))
#' }
#' @seealso \href{http://docs.intrinio.com/#companies}{Companies API documentation}.
i.companies <- function(tickers = NULL, ...){
  intrCallMap('companies', identifier = tickers, idCols = FALSE, MoreArgs = list(...))
}

#' Return available indices
#'
#' @param tickers A character vector of stock symbols to load detailed data on.
#' Loads an entire list of indices (master data feed) with compacted response values if \code{NULL}.
#' @param type A character vector with index types.
#' Either \code{stock_market}, \code{economic} or \code{sic}
#' @details This query will return indices available at intrinio with detailed information,
#' such as country, continent, full name and type.
#' @return Data in specified format. See \code{\link{intrOptions}} for details
#' @seealso \href{http://docs.intrinio.com/#indices58}{Indices API documentation}.
#' @examples
#' \dontrun{
#' i.indices() # over 1000 API calls
#' i.indices(ticker = c('$SPX', '$SSEC'))
#' i.indices(type = 'stock_market')
#' }
i.indices <- function(tickers = NULL, type = NULL, ...){
  if (!is.null(tickers) && !is.null(type)) {
    if (length(tickers) == length(type))
      return(intrCallMap('indices', identifier = tickers, type = type, idCols = FALSE, MoreArgs = list(...)))
    else if (length(type) == 1)
      return(intrCallMap('indices', identifier = tickers, idCols = FALSE, MoreArgs = c(list(type = type), list(...))))
    else
      stop('If both tickers and type are not null, they should either have the same length or type should be of length 1')
  }
  intrCallMap('indices', identifier = tickers, type = type, idCols = FALSE, MoreArgs = list(...))
}
