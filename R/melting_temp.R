#' Calculate melting temperature
#'
#' @param seq character string with DNA sequence
#' @return melting temperature in °C
#' @details
#' This function uses Wallace rule to calculate melting temperature
#' Calculation: 4(g+c)+2(a+t)
#' @export
melting_temp <- function(seq){
  seq <- unlist(strsplit(seq, split=""))
  counts <- table(seq)
  a <- sum(counts["A"],na.rm=T)
  c <- sum(counts["C"],na.rm=T)
  g <- sum(counts["G"],na.rm=T)
  t <- sum(counts["T"],na.rm=T)
  t_melt <- 4*(g+c)+2*(a+t)
  return(t_melt)
}
