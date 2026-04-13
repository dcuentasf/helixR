#' Converts DNA to RNA by replacing T with U
#'
#' @param seq character string with DNA sequence
#' @return character string with transcribed sequence
#' @export
transcribe <- function(seq){
  seq <- unlist(strsplit(seq,split=""))
  t_idx <- which(seq=="T")
  for(i in t_idx){
    seq[i] <- "U"
  }
  rna <- paste(seq,collapse="")
  return(rna)
}
