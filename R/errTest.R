parseLogFiles <- function(){
  res <- list()
  for (i in 1:10) {
    withCallingHandlers(res[[i]] <- parse_log_file(i),
             malformed_log_entry = function(e) invokeRestart('zero_log_entry')
             )
  }
  res
}

parse_log_file <- function(file){
  res <- list()
  for (j in 1:10) {
    res[[j]] <- withRestarts(parseLogEntry(j),
                             skip_log_entry = function(e) NULL,
                             zero_log_entry = function(e) 0

    )
  }
  res
}

parseLogEntry <- function(entry){
  if (entry == 8) stop(malformed_log_entry_error('Entry 8'))
  return(paste('Entry', entry))
}

condition <- function(subclass, message, call = sys.call(-1), ...) {
  structure(
    class = c(subclass, "condition"),
    list(message = message, call = call, ...)
  )
}

malformed_log_entry_error <- function(text) {
  msg <- paste0("Malformed log entry: ", text)
  condition(c("malformed_log_entry", "error"),
            message = msg,
            text = text
  )
}
