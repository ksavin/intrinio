fillRect <- function(row, col, s = 150){
  m <- matrix(FALSE, nrow = row, ncol = col)

  res <- list()
  if (row > s && col > s) {
    cMod <- col %% s
    rMod <- row %% s
    if (cMod < rMod){

    }
  }

  div <- divisors(s)
  cdiv <- divisors(col)
  rdiv <- divisors(row)
}

divisors <- function(x){
  y <- seq_len(x)
  y[ x %% y == 0 ]
}
