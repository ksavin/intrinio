.onLoad <- function(libname, pkgname){
  assign('intrinio_globals', new.env(), envir = parent.env(environment()))
  assign('base_url', 'https://api.intrinio.com/', envir = intrinio_globals)
  assign('options', list(
    verbose = TRUE,
    costWarning = FALSE,
    warnThreshold = 50,
    maxPageSize = maxPages,
    outFormat = 'data.frame'
  ), envir = intrinio_globals)
}
