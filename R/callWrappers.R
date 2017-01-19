#' Load list of securities and their details
#' @description Pulls a list of securities available in [U.S. Public Company Data Feed] via Intrinio API
#' @param tickers A character vector of stock symbols to load detailed data on.
#' @param exchange A string or a character vector of the same length as tickers (if specified) containing exchange(s)
#' @param ... other arguments to pass to \code{MoreArgs} of \code{intrCallMap}
#' @details Calling \code{i.securities()} without any arguments will return master data feed (a data.frame with all tickers)\cr
#' Passing a vector of tickers will return more detailed data on securities
#' @return Data in specified format. See \code{\link{intrOptions}} for details
#' @section Data Feed:
#' \href{https://intrinio.com/data/company-security-master}{Global Public Company Security Master}
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
#' @section Data Feed:
#' {https://intrinio.com/data/company-financials}{US Public Company Financials}
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
#' @section Data Feed:
#' \href{https://intrinio.com/data/company-security-master}{Global Public Company Security Master}
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

#' Securities Search/Screener
#' @description Returns information on securities that matches passed conditions
#' @param ... Any number of unquoted conditions.
#' Each condition should contain a \href{http://docs.intrinio.com/tags/intrinio-public#data-point}{data point tag} a data tag
#' on its left-hand side, a value or expression on a right-hand side and allowed operator in-between.
#' @param literal A string with intrinio-formatted conditions.
#' If not NULL, will ignore parameters and  pass literal instead.
#' Recommended when \code{i.secScreener} is used in scripts.
#' @details Currenly the following operators are available:
#' \describe{
#'   \item{\code{==}}{Equal}
#'   \item{\code{>=}}{Greater or equal}
#'   \item{\code{<=}}{Less or equal}
#'   \item{\code{>}}{Greater}
#'   \item{\code{<}}{Less}
#'   \item{\code{ \%contains\% }}{Contains text}
#' }\cr
#' Right-hand side of condition should be either a value, a function call or an expression.
#' Data tags on the right-hand side are not supported by API.\cr
#' Parser will attempt to evaluate the right-hand side and in case of a failure will consider it a string literal.\cr
#' Left-hand side is not evaluated.
#' @return Data in specified format. See \code{\link{intrOptions}} for details
#' @section Data Feed:
#' {https://intrinio.com/data/company-financials}{US Public Company Financials}
#' @examples
#' #' \dontrun{
#' i.secScreener(open_price >= 10.50, pricetoearnings > 10)
#' i.secScreener(literal = 'open_price~gte~10.50,pricetoearnings~gt~10') #Same
#'
#' #Same results with executable code on right-hand side
#' a <- 10
#' b <- 0.5
#' sq <- 100
#' i.secScreener(open_price >= a + b, pricetoearnings > sqrt(sq))
#' }
i.secScreener <- function(..., literal = NULL) {
  in.fCall <- as.list(match.call(expand.dots = TRUE))
  if (!is.null(literal)) {
    assert_that(is.string(literal))
    return(intrCall(endpoint = 'securities/search', conditions = literal))
  }
  if (any(names(in.fCall) == 'literal'))
    in.fCall <- in.fCall[names(in.fCall) != 'literal']
  in.fCall <- in.fCall[-1]
  if (length(in.fCall) < 1) stop('Must define at least 1 condition')
  q <- screenQuery(in.fCall)
  intrCall(endpoint = 'securities/search', conditions = q)
}

# makes screening query from expression list.
# Evil code warning
screenQuery <- function(in.exprList) {
  in.chExp <- lapply(in.exprList, as.character)
  lapply(in.chExp, function(x)
    if (length(x) != 3) stop(paste0('Could not parse condition: ', paste(x, collapse = ' ')))
  )
  in.operators <- sapply(in.chExp, `[`, 1)
  in.operators_parsed <- iOperators[in.operators, i, on = 'r']
  if (any(is.na(in.operators_parsed)))
    stop(paste0('Unrecognized or unsupported operators: ',
                paste0(in.operators[is.na(in.operators_parsed)], collapse = ', ')))
  in.dataTags <- sapply(in.chExp, `[`, 2)

  in.values <- sapply(in.chExp, `[`, 3)

  in.values <- sapply(in.values, function(x){
    valParsed <- try(eval(parse(text = x)), silent = TRUE)
    if (!inherits(valParsed, 'try-error') &&
        is.vector(valParsed) &&
        length(valParsed) == 1)
      return(as.character(valParsed))
    x
  })
  query <- paste0(in.dataTags, in.operators_parsed, in.values, collapse = ',')
  query
}


#' Most recent data point
#' @description Returns the most recent data point (such as P/E, price, financial) for selected identifier(s)
#' (ticker symbol, stock market index symbol, CIK ID, etc.) for selected tag(s).
#' @param tickers A character vector of stock symbols (or other eligible identifiers, such as CIK number)
#' to load detailed data on.
#' @param tags A character vector of stock symbols to load detailed data on.
#' Entire tag list is available \href{http://docs.intrinio.com/tags/intrinio-public#data-point}{here}
#' @param ... other arguments to pass to \code{MoreArgs} of \code{intrCallMap}
#' @details API allows requesting up to 150 identifier/tah combinations at a time.
#' E.g. 1 identifier and 150 items, or 150 identifiers and 1 tag, or 12 identifiers and 12 tags (144 combinations).
#' This function automatically makes multiple requests if API limits are not met, attempting to minimize number of calls.
#' It is assumed that you need \emph{all} data tags specified for every ticker specified,
#' so do not pass duplicate tickers or tags.
#' @return Data in specified format. See \code{\link{intrOptions}} for details
#' @section Data Feed:
#' \href{https://intrinio.com/data/company-financials}{US Public Company Financials}
#' @seealso \href{http://docs.intrinio.com/#data-point}{Data Point documentation}.
#' @examples
#' @noRd
#' \dontrun{
#' i.dataPoint(tickers = c('AAPL', 'MSFT), tags = c('pricetoearnings', 'totalrevenue'))
#' }
i.dataPoint <- function(tickers, tags, ...){

}
