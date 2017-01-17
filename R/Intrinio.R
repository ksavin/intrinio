#' Generic Intrinio API call
#' @description Makes a single API call to Intrinio. This is a low-level function.It is generally recommended to use intrCall
#' @noRd
intr_call_l <- function(endpoint, ...) {
  link <- modify_url(base_url(), path = endpoint, query = list(...))
  if (intrOptions()$verbose) statusMsg('Requesting:', link)
  cred <- intrAuth()
  response <- VERB('GET', link, authenticate(cred$user, cred$pass, type = 'basic'))
  code <- response$status_code

  if (code != 200) {
    stop(call_error(link, code))
  }
  res <- content(response, 'text', encoding = 'UTF-8')
  if (nchar(res) == 0) stop(call_error(link, -100))
  res <- fromJSON(res)
  if ('data' %in% names(res)) res$data <- lapply(res$data, function(x) if (is.null(x)) return(NA) else x)
  res <- lapply(res, function(x) if (is.null(x)) return(NA) else x)
  res
}

# Multipage iterator over intr_call_l
intr_call_m <- function(endpoint, pageSize, startPage, endPage, pageRecover = TRUE, ...) {
  res <- list()
  tryCatch(
    {
      for (i in startPage:endPage) {
        res[[i]] <- intr_call_l(endpoint, page_size = pageSize, page_number = i, ...)$data
      }
      res
    },
    call_error = function(e){
      if (!pageRecover) stop(e)
      msg <- paste0("---\nPage loading has failed with an error: \n", e$message,
                    '\n Returned all pages that managed to load\n---')
      if (intrOptions()$verbose) statusMsg(msg)
      warning(msg)
      return(res)
    }
  )
  res
}

#' Generic Intrinio API call with multi-page results
#' @description Unless API call results fit into Intrinio limit, they are split into multiple pages.
#' This function requests each page separately and combines results.
#' @param endpoint Name of an endpoint, such as 'companies', 'securities', 'indices', 'owners' or 'stock_exchanges'.
#' See \href{docs.intrinio.com}{API documentation}. May have a composite name, e.g. \code{'fundamentals/standardized'}
#' @param page_size Page size for results. If set to \code{'auto'} will use the largest size allowed for the endpoint.
#' See \href{http://docs.intrinio.com/#paging-limits}{Paging Limits} or \code{intrOptions()$maxPageSize}
#' @param startPage A page to start loading from.
#' @param ... Any other named flags/queries to pass.
#' @return a list with data.
#' @examples
#' intrCall('companies') # Pulls Master Data Feed (first page).
#' intrCall('companies', identifier = 'AAPL')
#' intrCall('fundamentals/standardized', identifier = 'AAPL', statement = 'income_statement', type = 'FY')
intrCall <- function(endpoint,
                     pageSize = 'auto',
                     startPage = 1,
                     endPage = NULL,
                     pageRecover = TRUE,
                     outFormat = intrOptions()$outFormat,
                     ...){
  if (pageSize == 'auto') pageSize <- auto_page_size(endpoint)
  assert_that(is.number(pageSize) && is.number(startPage) && is.string(endpoint))

  withRestarts(
    {
      res <- intr_call_l(endpoint, page_size = pageSize, page_number = startPage, ...)

      if (is.null(res$total_pages)) return(as.data.table(res))
      if (is.null(endPage)) endPage <- res$total_pages
      if (endPage <= startPage) return(as.data.table(res$data))
      cost <- endPage - startPage
      if (intrOptions()$costWarning && intrOptions()$warnThreshold < cost) {
        ans <- readline(
          paste('The request will cost', cost,
                'API credits. Run intrOptions(costWarning = FALSE) to disable this warning. Proceed (y/n)? '))
        if (!ans %in% c('Y', 'y')) return(NULL)
      }
      res <- c(list(res$data), intr_call_m(endpoint, pageSize, startPage + 1, endPage, pageRecover, ...))
      intrRbind(res, outFormat = outFormat)
    },
    skip_request = function(e) {
      if (intrOptions()$verbose)
        statusMsg('Skipped request', e$link, '\n Loading has failed with an error:\n', e$message)
      warning('Skipped request: ', e$link, '\n Loading has failed with an error:\n', e$message)
      return(NULL)
    }
  )
}

#' A multi-request wrapper around intrCall
#'
#' @description Use if needed to load data that requires multiple requests, such as data for multiple securities.
#' Each request can be multipage
#' @param endpoint Name of an endpoint, such as 'companies', 'securities', 'indices', 'owners' or 'stock_exchanges'.
#' See \href{docs.intrinio.com}{API documentation}. May have a composite name, e.g. \code{'fundamentals/standardized'}
#' @param pageSize Page size for results. If set to \code{'auto'} will use the largest size allowed for the endpoint.
#' See \href{http://docs.intrinio.com/#paging-limits}{Paging Limits} or \code{intrOptions()$maxPageSize}
#' @param idCols A single-element logical. If TRUE, will add a vectors from (...)
#' to the resulting data. FALSE is typically needed when the data loaded already contains ID columns
#' @param ... Arguments to iterate over similar to how \code{mapply} does it.
#' Must be vectors of the same length with names corresponding to Intrinio API queries
#' @param MoreArgs A named list of other constant queries to pass to \code{intrCall}.
#' Elements of MoreArgs must be vectors of length 1
intrCallMap <- function(endpoint, pageSize = 'auto', idCols = TRUE, ..., MoreArgs = NULL) {
  vectArgs <- list(...)
  vectArgs <- vectArgs[!sapply(vectArgs, is.null)]
  idCols <- if (idCols) vectArgs else NULL

  # check that MoreArgs is a named list containing single-element vectors
  if (!is.null(MoreArgs) && length(MoreArgs) > 0) {
    assert_that(is.list(MoreArgs),
                !is.null(names(MoreArgs)),
                all(nchar(names(MoreArgs)) > 0),
                all(sapply(MoreArgs, is.vector)),
                all(sapply(MoreArgs, length) == 1))
  }

  # if (...) is empty, simply call intrCall
  if (length(vectArgs) == 0) {
    return(
      do.call(intrCall, args = c(
        list(endpoint = endpoint, pageSize = pageSize, startPage = 1), MoreArgs))
    )
  }

  # Verify that (...) consists of vectors of the same length
  if (!all(sapply(vectArgs, is.vector))) stop('All arguments in ... must be vectors')
  if (length(unique(sapply(vectArgs, length))) > 1) stop('All vectors in ... must have the same length')

  # if (combine != 1L) {
  #   assert_that(is.integer(combine), length(combine) == 1, combine > 1)
  #   vectArgs <- lapply(vectArgs, function(x, size) {
  #     tbl <- data.table(x = x, chunk = 1:length(x) %/% size)[, .(x = paste0(x, collapse = ',')), chunk]
  #     tbl$x
  #   }, combine)
  # }

  res <- list() # output variable
  for (i in 1:length(vectArgs[[1]])) {
    response <-
      tryCatch({

        withCallingHandlers({
          r <- do.call(intrCall, args = c(
            list(
              endpoint = endpoint,
              pageSize = pageSize,
              startPage = 1,
              outFormat = 'data.table',
              pageRecover = FALSE),
            lapply(vectArgs, `[`, i),
            MoreArgs)
          )
          if (!is.null(r) && length(r) > 0) {
            if (any(sapply(r, is.list)))
              r <- lapply(r, function(x, id) {x[, intr_call_id := id]; x}, i)
            else r[, intr_call_id := i]
          }
          r
        },

        # if intrCall fails with empty results ask it to return null, otherwise fail
        call_error = function(e) {
          switch(
            e$name,
            resultEmpty = invokeRestart(findRestart("skip_request"), e),
            stop(e)
          )
        })

      },

      # Return error object in case of exceeding limit or server failure. Fail otherwise
      call_error = function(e){
        switch(
          e$name,
          forbidden = stop(e),
          notFound = stop(e),
          limit = e,
          serverError = e,
          unavailable = e,
          stop(e)
        )
      })

    # If error object returned, check if anything is loaded.
    # If yes, return loaded data with warning, fail otherwise
    if (inherits(response, 'error')) {
      if (i == 1 || all(sapply(res, is.null))) stop(response)
      warning(paste0('---\nLoading encountered an error and had to be interrupted.',
                     ' Returned successfully loaded results.\n',
                     response$message, '\n---'))
      return(intrRbind(res, id = idCols))
    }
    res[[i]] <- response
  }

  intrRbind(res, id = idCols) # Rbind list into table, convert to format specified
}

#' @title Authorize Web API access
#' @param user A string containing \code{API_USERNAME}
#' @param pass A string containing \code{API_PASSWORD}
#' @description The Intrinio API uses Basic Authentication over HTTPS.
#' You can find your \code{API_USERNAME} and \code{API_PASSWORD} on your User Admin Dashboard of the site.\cr
#' \code{intrAuth} simply stores your credentials in \code{intrinio_globals} environment for future use.
#' Credentials need to be specified on every package reload.
#' @seealso \href{http://blog.intrinio.com/getting-started-with-intrinio/}{Getting Started article}
#'
#' @return a list with credentials if user and password are not passed.
#' @examples
#' intrAuth('a543b029ec335ddfw66dd95bfa1ea3ac', '991d8ca963fgaa357aef78b4784d88b0')
#' intrAuth()
intrAuth <- function(user, pass){
  if (!missing(user) && !missing(pass)) {
    assign('intrinio.user', user, envir = intrinio_globals)
    assign('intrinio.pass', pass, envir = intrinio_globals)
  } else if (missing(user) && missing(pass)) {
    return(
      tryCatch(
        list(user = get('intrinio.user', intrinio_globals),
             pass = get('intrinio.pass', intrinio_globals)),
        error = function(e) stop('Not authorized. Run intrAuth() with your credentials.')
      )
    )
  } else stop('Specify either user-pass pair or neither.')
}

#' Gets or sets Intrinio package options
#' @description To avoid passing frequent options to every API wrapper, they are defined globally (within package scope).
#' @param ... Named arguments for option setting. Will return all current options if none are specified.
#'
#' @return A list of options if none are passed
#' @export
#' @details Currenly the following options are available:
#' \describe{
#'   \item{\code{verbose:}}{Will show status messages if set to \code{TRUE}.\cr
#'   Default: \code{TRUE}}
#'
#'   \item{\code{costWarning:}}{If \code{TRUE}, will ask for user confirmation in case when request costs
#'   more than \code{warnThreshold} API call credits (useful for multipage calls).\cr
#'   Default: \code{FALSE}}
#'
#'   \item{\code{warnThreshold:}}{Minimum number of API calls in multipage request to request confirmation.\cr
#'   Default: \code{50}}
#'
#'   \item{\code{outFormat}:}{Format of resulting output.\cr
#'   Possible options: \code{'data.frame'} or \code{'data.table'}.\cr
#'   Default: \code{'data.frame'}}
#'
#'   \item{\code{maxPageSize} (view only):}{
#'   A \code{data.frame} with maximum page size by endpoint.
#'   Used to determine default page size in \code{intrMultipage} call.
#'   To override, pass \code{page_size} to API call when needed. See \href{http://docs.intrinio.com/#paging-limits}{Paging Limits}
#'   }
#' }
#' @note Options are reset to default on every package reload.
#' @examples
#' # get option list
#' intrOptions()
#'
#' # modify options
#' intrOptions(verbose = TRUE, costWarning = FALSE)
intrOptions <- function(...){
  args <- list(...)
  options <- get('options', envir = intrinio_globals)
  if (length(args) == 0) return(options)
  opts_public <- names(options)[!names(options) %in% c('maxPageSize')]
  if (!all(names(args) %in% opts_public))
    stop("some options are undefined or read-only: ", paste0(names(args)[!names(args) %in% opts_public], collapse = ', '))
  options[names(args)] <- args
  assign('options', options, envir = intrinio_globals)
}

base_url <- function(){
  get('base_url', intrinio_globals)
}

# Gets a maximum allowed page size for an endpoint (values are stored locally)
auto_page_size <- function(endpoint) {
  mysub <- function(re, x) sub(re, "", x, perl = TRUE)
  endpoint <- mysub("[\\/]+$", mysub("^[\\/]+", endpoint))
  size <- maxPages[endpoint == endpoint, size]
  if (length(size) == 0) size <- maxPages[endpoint == 'else', size]
  size
}

# Custom error base class
condition <- function(subclass, message, call = sys.call(-1), ...) {
  structure(
    class = c(subclass, "condition"),
    list(message = message, call = call, ...)
  )
}

# Custom error constructor for API call errors
call_error <- function(request, errCode) {
  if (errCode %in% errCodes$code) {
    #errCodes is a data.table in sysdata.rda
    description <- errCodes[code == errCode, description]
    errName <- errCodes[code == errCode, name]
  } else description <- 'Unknown error code'
  msg <- paste0('Failed to load: ', request, '\nError ', errCode, ': ', description)
  condition(c("call_error", "error"),
            message = msg,
            name = errName,
            code = errCode,
            description = description,
            link = request
  )
}

statusMsg <- function(...){
  msg <- paste0(c(list(...), '\n'), collapse = '')
  cat(msg)
}
