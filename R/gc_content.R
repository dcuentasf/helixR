#' Calculate GC content of a DNA sequence
#'
#' @param seq character string with DNA sequence
#' @return numeric percentage of GC content
#' @export
gc_content <- function(seq){
  seq <- unlist(strsplit(seq, split=""))
  counts <- table(seq)
  a <- sum(counts["A"],na.rm=T)
  c <- sum(counts["C"],na.rm=T)
  g <- sum(counts["G"],na.rm=T)
  t <- sum(counts["T"],na.rm=T)
  gc_content <- 100*(g+c)/(a+c+g+t)
  return(gc_content)
}
